       Identification Division.
       Program-Id.                                 pbfo9500           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    bfo                 *
      *                                Settore:    uti                 *
      *                                   Fase:    bfo950              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 01/03/96    *
      *                       Ultima revisione:    NdK del 30/04/10    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Chiusura - riapertura bolle fornitori       *
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
                     "bfo"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "uti"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "bfo950"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pbfo9500"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "  CHIUSURA - RIAPERTURA BOLLA FORNITORE "       .

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
      *        * [bft]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbft"                          .
      *        *-------------------------------------------------------*
      *        * [bfr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbfr"                          .
      *        *-------------------------------------------------------*
      *        * [ybf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfybf"                          .
      *        *-------------------------------------------------------*
      *        * [fnt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rffnt"                          .
      *        *-------------------------------------------------------*
      *        * [dcf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfdcf"                          .

      *    *===========================================================*
      *    * Work-area per bufferizzazione testata                     *
      *    *-----------------------------------------------------------*
       01  w-tes.
      *        *-------------------------------------------------------*
      *        * Valori chiave                                         *
      *        *-------------------------------------------------------*
           05  w-tes-val-key.
      *            *---------------------------------------------------*
      *            * Codice dipendenza                                 *
      *            *---------------------------------------------------*
               10  w-tes-cod-dpz          pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Numero protocollo : saa.dd.nnnnnn                 *
      *            *---------------------------------------------------*
               10  w-tes-num-prt          pic  9(11)                  .
               10  w-tes-num-prt-r        redefines
                   w-tes-num-prt                                      .
                   15  w-tes-num-prt-saa  pic  9(03)                  .
                   15  w-tes-num-prt-dpz  pic  9(02)                  .
                   15  w-tes-num-prt-prg  pic  9(06)                  .
      *            *---------------------------------------------------*
      *            * Tipo movimento per la bolla                       *
      *            *---------------------------------------------------*
               10  w-tes-cod-tmb          pic  x(05)                  .
               10  w-tes-cod-tmb-des      pic  x(30)                  .
               10  w-tes-cod-tmb-mif      pic  9(02)                  .
               10  w-tes-cod-tmb-tmf      pic  x(05)                  .
               10  w-tes-cod-tmb-cam      pic  9(05)                  .
               10  w-tes-cod-tmb-cam-tmc  pic  9(02)                  .
               10  w-tes-cod-tmb-cam-dtm  pic  x(01)                  .
               10  w-tes-cod-tmb-cam-vtm  pic  x(01)                  .
               10  w-tes-cod-tmb-cam-ltm  pic  x(04)                  .
               10  w-tes-cod-tmb-ctm      pic  x(03)                  .
               10  w-tes-cod-tmb-dfa      pic  x(01)                  .
               10  w-tes-cod-tmb-vaa      pic  x(01)                  .
               10  w-tes-cod-tmb-lsa      pic  x(04)                  .
               10  w-tes-cod-tmb-ord      pic  9(02)                  .
               10  w-tes-cod-tmb-prd      pic  9(02)                  .
               10  w-tes-cod-tmb-maf      pic  9(02)                  .
               10  w-tes-cod-tmb-dmf      pic  x(05)                  .
               10  w-tes-cod-tmb-tvd      pic  x(01)                  .
               10  w-tes-cod-tmb-ddd      pic  x(07)                  .
               10  w-tes-cod-tmb-dsl      pic  x(07)                  .
               10  w-tes-cod-tmb-dtr      pic  x(05)                  .
      *            *---------------------------------------------------*
      *            * Si/No movimento che interessa la fatturazione     *
      *            * - 01 : No                                         *
      *            * - 02 : Si, il documento sara' successivamente     *
      *            *            sottoposto a fatturazione da parte del *
      *            *            fornitore                              *
      *            * - 03 : Si,il documento stesso e' anche una fattura*
      *            *---------------------------------------------------*
               10  w-tes-int-ftr          pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Tipo movimento per la fatturazione                *
      *            *                                                   *
      *            * N.B. : Attualmente il valore di questo campo sa-  *
      *            *        ra' sempre a spaces, in quanto non sono    *
      *            *        gestite le fatture accompagnatorie per     *
      *            *        i fornitori                                *
      *            *---------------------------------------------------*
               10  w-tes-tmo-ftr          pic  x(05)                  .
      *            *---------------------------------------------------*
      *            * Data registrazione                                *
      *            *---------------------------------------------------*
               10  w-tes-dat-reg          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
      *            *---------------------------------------------------*
      *            * Codice fornitore                                  *
      *            *---------------------------------------------------*
               10  w-tes-fnt-org          pic  9(07)                  .
               10  w-tes-fnt-org-rag      pic  x(40)                  .
               10  w-tes-fnt-org-via      pic  x(40)                  .
               10  w-tes-fnt-org-loc      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Flag di bolla chiusa                              *
      *            *---------------------------------------------------*
               10  w-tes-flg-bch          pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area generica per tutto il programma                 *
      *    *-----------------------------------------------------------*
       01  w-gen.
           05  w-gen-dat-att              pic  9(07)                  .

      *    *===========================================================*
      *    * Work-area referenze                                       *
      *    *-----------------------------------------------------------*
       01  w-ref.
      *        *-------------------------------------------------------*
      *        * Referenze relative al sottoconto iva acquisti         *
      *        *-------------------------------------------------------*
           05  w-ref-stc-iac.
               10  w-ref-stc-iac-cod      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Referenza relativa default per il tipo movimento      *
      *        *-------------------------------------------------------*
           05  w-ref-cod-tmo.
      *            *---------------------------------------------------*
      *            * Modalita' di trattamento default                  *
      *            *                                                   *
      *            *  - ' ' : Nessun default                           *
      *            *  - 'I' : Default iniziale in entrata programma    *
      *            *  - 'C' : Default continuo, anche cioe' se valore  *
      *            *          successivamente variato                  *
      *            *  - 'O' : Default obbligatorio, cioe' valore non   *
      *            *          modificabile per nessun motivo           *
      *            *---------------------------------------------------*
               10  w-ref-cod-tmo-trt      pic  x(01)                  .
               10  filler                 pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Codice tipo movimento di default                  *
      *            *---------------------------------------------------*
               10  w-ref-cod-tmo-cod      pic  x(05)                  .

      *    *===========================================================*
      *    * Work per subroutines di Acc                               *
      *    *-----------------------------------------------------------*
       01  w-acc.
      *        *-------------------------------------------------------*
      *        * Work per Acc : Numero protocollo                      *
      *        *-------------------------------------------------------*
           05  w-acc-num-prt.
               10  w-acc-num-prt-num      pic  9(09)                  .
               10  w-acc-num-prt-num-r    redefines
                   w-acc-num-prt-num.
                   15  w-acc-num-prt-saa  pic  9(03)                  .
                   15  w-acc-num-prt-prg  pic  9(06)                  .

      *    *===========================================================*
      *    * Work per valori di default                                *
      *    *-----------------------------------------------------------*
       01  w-def.
      *        *-------------------------------------------------------*
      *        * Data registrazione                                    *
      *        *-------------------------------------------------------*
           05  w-def-dat-reg              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Tipo movimento per la bolla fornitore                 *
      *        *-------------------------------------------------------*
           05  w-def-cod-tmb              pic  x(05)                  .
           05  w-def-cod-tmb-des          pic  x(30)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Flag di bolla chiusa                       *
      *        *-------------------------------------------------------*
           05  w-exp-flg-bch.
               10  w-exp-flg-bch-num      pic  9(02)       value 2    .
               10  w-exp-flg-bch-lun      pic  9(02)       value 02   .
               10  w-exp-flg-bch-tbl.
                   15  filler             pic  x(02) value
                            "No"                                      .
                   15  filler             pic  x(02) value
                            "Si"                                      .

      *    *===========================================================*
      *    * Work per subroutines di Find                              *
      *    *-----------------------------------------------------------*
       01  w-fnd.
      *        *-------------------------------------------------------*
      *        * Work per Find su archivio [bft]                       *
      *        *-------------------------------------------------------*
           05  w-fnd-arc-bft.
               10  w-fnd-arc-bft-sel      pic  x(01)                  .
               10  w-fnd-arc-bft-tmb      pic  x(05)                  .
               10  w-fnd-arc-bft-dat      pic  9(07)                  .
               10  w-fnd-arc-bft-prt      pic  9(11)                  .
               
      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [ybf]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-ybf.
               10  w-let-arc-ybf-flg      pic  x(01)                  .
               10  w-let-arc-ybf-cod      pic  x(05)                  .
               10  w-let-arc-ybf-dpz      pic  9(02)                  .
               10  w-let-arc-ybf-des      pic  x(30)                  .
               10  w-let-arc-ybf-mif      pic  9(02)                  .
               10  w-let-arc-ybf-tmf      pic  x(05)                  .
               10  w-let-arc-ybf-acm      pic  x(01)                  .
               10  w-let-arc-ybf-dtc      pic  x(01)                  .
               10  w-let-arc-ybf-dct      pic  x(03)                  .
               10  w-let-arc-ybf-cam      pic  9(05)                  .
               10  w-let-arc-ybf-ctm      pic  x(03)                  .
               10  w-let-arc-ybf-dfa      pic  x(01)                  .
               10  w-let-arc-ybf-vaa      pic  x(01)                  .
               10  w-let-arc-ybf-lsa      pic  x(04)                  .
               10  w-let-arc-ybf-ord      pic  9(02)                  .
               10  w-let-arc-ybf-prd      pic  9(02)                  .
               10  w-let-arc-ybf-sgl      pic  x(03)                  .
               10  w-let-arc-ybf-maf      pic  9(02)                  .
               10  w-let-arc-ybf-dmf      pic  x(05)                  .
               10  w-let-arc-ybf-tvd      pic  x(01)                  .
               10  w-let-arc-ybf-dsl      pic  x(07)                  .
               10  w-let-arc-ybf-dtr      pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [fnt]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-fnt.
               10  w-let-arc-fnt-flg      pic  x(01)                  .
               10  w-let-arc-fnt-cod      pic  9(07)                  .
               10  w-let-arc-fnt-rag      pic  x(40)                  .
               10  w-let-arc-fnt-via      pic  x(40)                  .
               10  w-let-arc-fnt-loc      pic  x(40)                  .
               10  w-let-arc-fnt-piv      pic  9(11)                  .
               10  w-let-arc-fnt-stc      pic  9(07)                  .
               10  w-let-arc-fnt-ass      pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcf]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcf.
               10  w-let-arc-dcf-flg      pic  x(01)                  .
               10  w-let-arc-dcf-fnt      pic  9(07)                  .
               10  w-let-arc-dcf-dpz      pic  x(04)                  .
               10  w-let-arc-dcf-rag      pic  x(40)                  .
               10  w-let-arc-dcf-via      pic  x(40)                  .
               10  w-let-arc-dcf-loc      pic  x(40)                  .
               10  w-let-arc-dcf-vlt      pic  x(03)                  .
               10  w-let-arc-dcf-lng      pic  x(03)                  .
               10  w-let-arc-dcf-tai      pic  9(02)                  .
               10  w-let-arc-dcf-cbl      pic  9(02)                  .
               10  w-let-arc-dcf-obl      pic  9(02)                  .
               10  w-let-arc-dcf-tpf      pic  9(02)                  .
               10  w-let-arc-dcf-apf      pic  9(07)                  .
               10  w-let-arc-dcf-dpf      pic  x(04)                  .
               10  w-let-arc-dcf-tfz      pic  9(02)                  .
               10  w-let-arc-dcf-tdp      pic  9(02)                  .
               10  w-let-arc-dcf-tus      pic  9(02)                  .
               10  w-let-arc-dcf-tud      pic  9(07)                  .
               10  w-let-arc-dcf-tuc      pic  9(07)                  .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per Det documento gia' esistente oppure no       *
      *        *-------------------------------------------------------*
           05  w-det-doc-ges.
      *            *---------------------------------------------------*
      *            * Esito della determinazione                        *
      *            * - S : Si, documento del tipo movimento cercato e  *
      *            *       e con data e numero cercato esistente in    *
      *            *       archivio                                    *
      *            * - N : No, documento del tipo movimento cercato e  *
      *            *       e con data e numero cercato non esistente   *
      *            *       in archivio                                 *
      *            * - X : No, documento del tipo movimento cercato e  *
      *            *       e con data e numero cercato non esistente   *
      *            *       in archivio, ma esistenza di un documento   *
      *            *       con data e numero cercato, con stesso co-   *
      *            *       dice numerazione, ma con diverso tipo mo-   *
      *            *       vimento da quello cercato                   *
      *            *---------------------------------------------------*
               10  w-det-doc-ges-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Tipo movimento trovato per valore precedente de-  *
      *            * terminato a 'X'                                   *
      *            *---------------------------------------------------*
               10  w-det-doc-ges-tmt      pic  x(05)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Salvataggio valori chiave                             *
      *        *-------------------------------------------------------*
           05  w-sav-val-key.
               10  filler   occurs 200    pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Vis                               *
      *    *-----------------------------------------------------------*
       01  w-vis.
      *        *-------------------------------------------------------*
      *        * Work per Vis : Numero protocollo                      *
      *        *-------------------------------------------------------*
           05  w-vis-num-prt.
               10  w-vis-num-prt-num      pic  9(09)                  .
               10  w-vis-num-prt-num-r    redefines
                   w-vis-num-prt-num.
                   15  w-vis-num-prt-saa  pic  9(03)                  .
                   15  w-vis-num-prt-prg  pic  9(06)                  .

      *    *===========================================================*
      *    * Work-area per routine scr-mov-fil-000/999                 *
      *    *-----------------------------------------------------------*
       01  w-scr-mov-fil.
           05  w-scr-mov-fil-ctr          pic  9(05)                  .
           05  w-scr-mov-fil-wtr.
               10  w-scr-mov-fil-wtp      pic  x(01)                  .
               10  w-scr-mov-fil-wtf      pic  x(01)                  .
               10  filler                 pic  x(03)                  .

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
      *    * Link-area per accettazione codice tipo movimento          *
      *    *-----------------------------------------------------------*
           copy      "pgm/bfo/prg/cpy/acdeybf0.acl"                   .

      *    *===========================================================*
      *    * Area di interfaccia per sottoprogramma         "pazi000d" *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/pazi000d.pgl"                   .

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
      *              * Determinazione codici dipendenze per l'azienda  *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      "DA"                 to   w-dpz-tip-ope          .
           move      s-ter                to   w-dpz-ide-ter          .
           move      s-ute                to   w-dpz-ide-ute          .
           move      s-azi                to   w-dpz-ide-azi          .
           move      s-sap                to   w-dpz-ide-sap          .
           move      s-arg                to   w-dpz-ide-arg          .
           move      s-set                to   w-dpz-ide-set          .
           move      s-fas                to   w-dpz-ide-fas          .
           move      i-ide-des            to   w-dpz-ide-des          .
           move      s-pro                to   w-dpz-ide-pro          .
           call      "pgm/azi/prg/obj/pazi000d"
                                         using w-dpz                  .
           cancel    "pgm/azi/prg/obj/pazi000d"                       .
      *              *-------------------------------------------------*
      *              * Se zero dipendenze : errore ed uscita           *
      *              *-------------------------------------------------*
           if        w-dpz-ctr-dpz        >    zero
                     go to pre-exe-pgm-120.
           move      "EN"                 to   w-dpz-tip-ope          .
           call      "pgm/azi/prg/obj/pazi000d"
                                         using w-dpz                  .
           cancel    "pgm/azi/prg/obj/pazi000d"                       .
           move      "#"                  to   w-cnt-pre-exe-pgm      .
           go to     pre-exe-pgm-999.
       pre-exe-pgm-120.
      *              *-------------------------------------------------*
      *              * Selezione codice dipendenza per il programma    *
      *              *-------------------------------------------------*
           move      "SD"                 to   w-dpz-tip-ope          .
           call      "pgm/azi/prg/obj/pazi000d"
                                         using w-dpz                  .
           cancel    "pgm/azi/prg/obj/pazi000d"                       .
       pre-exe-pgm-180.
      *              *-------------------------------------------------*
      *              * Se scelta non effettuata : uscita               *
      *              *-------------------------------------------------*
           if        w-dpz-cod-prg        =    zero
                     move  "#"            to   w-cnt-pre-exe-pgm
                     go to pre-exe-pgm-999.
      *              *-------------------------------------------------*
      *              * Altrimenti memorizzazione in campo testata      *
      *              *-------------------------------------------------*
           move      w-dpz-cod-prg        to   w-tes-cod-dpz          .
      *              *-------------------------------------------------*
      *              * Preparazione data attuale                       *
      *              *-------------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-gen-dat-att          .
      *              *-------------------------------------------------*
      *              * Preparazione data documento                     *
      *              *-------------------------------------------------*
           move      w-gen-dat-att        to   w-def-dat-reg          .
      *              *-------------------------------------------------*
      *              * Normalizzazione default per tipo movimento      *
      *              *-------------------------------------------------*
           move      spaces               to   w-def-cod-tmb          .
           move      spaces               to   w-def-cod-tmb-des      .
      *              *-------------------------------------------------*
      *              * Open moduli accettazione                        *
      *              *-------------------------------------------------*
           perform   opn-mdl-acc-000      thru opn-mdl-acc-999        .
      *              *-------------------------------------------------*
      *              * Lettura referenze                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Referenze per sottoconto iva acquisti       *
      *                  *---------------------------------------------*
           perform   ref-stc-iac-000      thru ref-stc-iac-999        .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura delle referenza sottoconto iva acquisti           *
      *    *-----------------------------------------------------------*
       ref-stc-iac-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione work-area                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-ref-stc-iac-cod      .
      *              *-------------------------------------------------*
      *              * Lettura referenza                               *
      *              *-------------------------------------------------*
           move      "R:"                 to   s-ope                  .
           move      "pgm/cge/mov/cge300[iva-acq]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Test su esito operazione                        *
      *              *-------------------------------------------------*
           if        s-ves                not  = spaces
                     go to ref-stc-iac-999.
      *              *-------------------------------------------------*
      *              * Valore referenza in work-area                   *
      *              *-------------------------------------------------*
           move      s-num                to   w-ref-stc-iac-cod      .
       ref-stc-iac-999.
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
      *              * Open modulo accettazione tipo movimento per la  *
      *              * bolla fornitore                                 *
      *              *-------------------------------------------------*
           perform   cod-des-ybf-opn-000  thru cod-des-ybf-opn-999    .
       opn-mdl-acc-999.
           exit.

      *    *===========================================================*
      *    * Close moduli di accettazione                              *
      *    *-----------------------------------------------------------*
       cls-mdl-acc-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione tipo movimento per la *
      *              * bolla fornitore                                 *
      *              *-------------------------------------------------*
           perform   cod-des-ybf-cls-000  thru cod-des-ybf-cls-999    .
       cls-mdl-acc-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * [bft]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *              *-------------------------------------------------*
      *              * [bfr]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *              *-------------------------------------------------*
      *              * [ybf]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofybf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ybf                 .
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
      *              * [dcf]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [bft]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *              *-------------------------------------------------*
      *              * [bfr]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *              *-------------------------------------------------*
      *              * [ybf]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofybf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ybf                 .
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
      *              * [dcf]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
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
      *                  * Numero protocollo                           *
      *                  *---------------------------------------------*
           perform   acc-num-prt-000      thru acc-num-prt-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
       acc-key-reg-200.
      *                  *---------------------------------------------*
      *                  * Data registrazione                          *
      *                  *---------------------------------------------*
           perform   acc-dat-reg-000      thru acc-dat-reg-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
           if        v-key                =    "UP  "
                     go to acc-key-reg-100.
       acc-key-reg-300.
      *                  *---------------------------------------------*
      *                  * Tipo movimento                              *
      *                  *---------------------------------------------*
           perform   acc-cod-tmb-000      thru acc-cod-tmb-999        .
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
      *              * Numero protocollo                               *
      *              *-------------------------------------------------*
           perform   vis-num-prt-000      thru vis-num-prt-999        .
      *              *-------------------------------------------------*
      *              * Data registrazione                              *
      *              *-------------------------------------------------*
           perform   vis-dat-reg-000      thru vis-dat-reg-999        .
      *              *-------------------------------------------------*
      *              * Tipo movimento                                  *
      *              *-------------------------------------------------*
           perform   vis-cod-tmb-000      thru vis-cod-tmb-999        .
           perform   vis-cod-tmb-des-000  thru vis-cod-tmb-des-999    .
       vis-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per campi chiave                  *
      *    *-----------------------------------------------------------*
       pmt-key-reg-000.
      *              *-------------------------------------------------*
      *              * Erase linee impegnate dalla chiave              *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      06                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazioni prompts                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero protocollo                           *
      *                  *---------------------------------------------*
           perform   pmt-num-prt-000      thru pmt-num-prt-999        .
      *                  *---------------------------------------------*
      *                  * Data registrazione                          *
      *                  *---------------------------------------------*
           perform   pmt-dat-reg-000      thru pmt-dat-reg-999        .
      *                  *---------------------------------------------*
      *                  * Tipo movimento                              *
      *                  *---------------------------------------------*
           perform   pmt-tip-mpb-000      thru pmt-tip-mpb-999        .
      *              *-------------------------------------------------*
      *              * Linea di trattini di separazione                *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per numero protocollo             *
      *    *-----------------------------------------------------------*
       pmt-num-prt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero protocollo  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-num-prt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per data registrazione            *
      *    *-----------------------------------------------------------*
       pmt-dat-reg-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      52                   to   v-pos                  .
           move      "Data registrazione :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-dat-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per tipo movimento                *
      *    *-----------------------------------------------------------*
       pmt-tip-mpb-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo movimento     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-tip-mpb-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Numero protocollo             *
      *    *-----------------------------------------------------------*
       acc-num-prt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Composizione work per accettazione          *
      *                  *---------------------------------------------*
           move      w-tes-num-prt-saa    to   w-acc-num-prt-saa      .
           move      w-tes-num-prt-prg    to   w-acc-num-prt-prg      .
       acc-num-prt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      "<B"                 to   v-edm                  .
           move      04                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-acc-num-prt-num    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-num-prt-999.
       acc-num-prt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-acc-num-prt-num      .
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Decomposizione work di accettazione         *
      *                  *---------------------------------------------*
           if        w-acc-num-prt-num    =    zero
                     go to acc-num-prt-300.
           move      w-acc-num-prt-saa    to   w-tes-num-prt-saa      .
           move      w-tes-cod-dpz        to   w-tes-num-prt-dpz      .
           move      w-acc-num-prt-prg    to   w-tes-num-prt-prg      .
       acc-num-prt-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-num-prt-400.
      *                  *---------------------------------------------*
      *                  * Find su archivio [bft]                      *
      *                  *---------------------------------------------*
           move      spaces               to   w-fnd-arc-bft-tmb      .
           move      zero                 to   w-fnd-arc-bft-dat      .
           move      zero                 to   w-fnd-arc-bft-prt      .
           perform   fnd-arc-bft-000      thru fnd-arc-bft-999        .
           if        w-fnd-arc-bft-sel    not  = spaces
                     go to acc-num-prt-100.
      *                  *---------------------------------------------*
      *                  * Forzatura function-key Do                   *
      *                  *---------------------------------------------*
           move      "DO  "               to   v-key                  .
       acc-num-prt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se il protocollo e' diverso da zero : simu- *
      *                  * lazione della ricerca                       *
      *                  *---------------------------------------------*
           if        w-acc-num-prt-num    =    zero
                     go to acc-num-prt-600.
      *                  *---------------------------------------------*
      *                  * Lettura testata tramite protocollo          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-tes-num-prt        to   rf-bft-num-prt         .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *                      *-----------------------------------------*
      *                      * Se registrazione non esistente : mes-   *
      *                      * saggio di errore                        *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     go to acc-num-prt-420.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Documento non esistente in archivio !             
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-num-prt-100.
       acc-num-prt-420.
      *                  *---------------------------------------------*
      *                  * Find precablato su archivio [bft]           *
      *                  *---------------------------------------------*
           move      rf-bft-cod-tmb       to   w-fnd-arc-bft-tmb      .
           move      rf-bft-dat-reg       to   w-fnd-arc-bft-dat      .
           move      rf-bft-num-prt       to   w-fnd-arc-bft-prt      .
           perform   fnd-arc-bft-000      thru fnd-arc-bft-999        .
           if        w-fnd-arc-bft-sel    not  = spaces
                     go to acc-num-prt-100.
      *                  *---------------------------------------------*
      *                  * Forzatura function-key Do                   *
      *                  *---------------------------------------------*
           move      "DO  "               to   v-key                  .
       acc-num-prt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-num-prt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-num-prt-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-num-prt-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-num-prt-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-num-prt-999.
       acc-num-prt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Numero protocollo          *
      *    *-----------------------------------------------------------*
       vis-num-prt-000.
      *              *-------------------------------------------------*
      *              * Composizione work per visualizzazione           *
      *              *-------------------------------------------------*
           move      w-tes-num-prt-saa    to   w-vis-num-prt-saa      .
           move      w-tes-num-prt-prg    to   w-vis-num-prt-prg      .
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      "<B"                 to   v-edm                  .
           move      04                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-vis-num-prt-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-prt-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Data registrazione            *
      *    *-----------------------------------------------------------*
       acc-dat-reg-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuale preparazione del valore di de-    *
      *                  * fault                                       *
      *                  *---------------------------------------------*
           if        w-tes-dat-reg        =    zero
                     move  w-def-dat-reg  to   w-tes-dat-reg          .
       acc-dat-reg-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      04                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-dat-reg        to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-dat-reg-999.
       acc-dat-reg-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   w-tes-dat-reg          .
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-dat-reg-400.
      *                  *---------------------------------------------*
      *                  * Find su archivio [bft]                      *
      *                  *---------------------------------------------*
           move      spaces               to   w-fnd-arc-bft-tmb      .
           move      zero                 to   w-fnd-arc-bft-dat      .
           move      zero                 to   w-fnd-arc-bft-prt      .
           perform   fnd-arc-bft-000      thru fnd-arc-bft-999        .
           if        w-fnd-arc-bft-sel    not  = spaces
                     go to acc-dat-reg-100.
      *                  *---------------------------------------------*
      *                  * Forzatura function-key Do                   *
      *                  *---------------------------------------------*
           move      "DO  "               to   v-key                  .
       acc-dat-reg-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a zero : reimpostazione, a meno   *
      *                  * che non si sia su Up                        *
      *                  *---------------------------------------------*
           if        w-tes-dat-reg        not  = zero
                     go to acc-dat-reg-600.
           if        v-key                =    "UP  "
                     go to acc-dat-reg-600
           else      go to acc-dat-reg-100.
       acc-dat-reg-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-reg-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-dat-reg-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-dat-reg-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-dat-reg-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-dat-reg-999.
       acc-dat-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Data documento             *
      *    *-----------------------------------------------------------*
       vis-dat-reg-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      04                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      w-tes-dat-reg        to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-reg-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Tipo movimento per bolla for- *
      *    * nitore                                                    *
      *    *-----------------------------------------------------------*
       acc-cod-tmb-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione valore di default              *
      *                  *---------------------------------------------*
           if        w-tes-cod-tmb        not  = spaces
                     go to acc-cod-tmb-100.
      *                  *---------------------------------------------*
      *                  * Preparazione e visualizzazione default      *
      *                  *---------------------------------------------*
           move      w-def-cod-tmb        to   w-tes-cod-tmb          .
           move      w-def-cod-tmb-des    to   w-tes-cod-tmb-des      .
           perform   vis-cod-tmb-des-000  thru vis-cod-tmb-des-999    .
       acc-cod-tmb-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-ybf-ope      .
           move      w-tes-cod-tmb        to   w-cod-des-ybf-cod      .
           move      05                   to   w-cod-des-ybf-lin      .
           move      22                   to   w-cod-des-ybf-pos      .
           move      05                   to   w-cod-des-ybf-dln      .
           move      28                   to   w-cod-des-ybf-dps      .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-des-ybf-cll-000  thru cod-des-ybf-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-ybf-foi-000  thru cod-des-ybf-foi-999    .
       acc-cod-tmb-110.
           perform   cod-des-ybf-cll-000  thru cod-des-ybf-cll-999    .
           if        w-cod-des-ybf-ope    =    "F+"
                     go to acc-cod-tmb-115.
           if        w-cod-des-ybf-ope    =    "AC"
                     go to acc-cod-tmb-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-tmb-115.
           perform   cod-des-ybf-foi-000  thru cod-des-ybf-foi-999    .
           go to     acc-cod-tmb-110.
       acc-cod-tmb-120.
           move      w-cod-des-ybf-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-tmb-999.
       acc-cod-tmb-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-cod-tmb          .
       acc-cod-tmb-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      w-tes-cod-tmb        to   w-all-str-alf          .
           move      05                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-cod-tmb-100.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [ybf] generale             *
      *                  *---------------------------------------------*
           move      w-tes-cod-tmb        to   w-let-arc-ybf-cod      .
           move      zero                 to   w-let-arc-ybf-dpz      .
           perform   let-arc-ybf-000      thru let-arc-ybf-999        .
           move      w-let-arc-ybf-des    to   w-tes-cod-tmb-des      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-cod-tmb-des-000  thru vis-cod-tmb-des-999    .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-ybf-flg    not  = spaces
                     go to acc-cod-tmb-100.
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione, a meno *
      *                  * che non si sia su Up                        *
      *                  *---------------------------------------------*
           if        w-tes-cod-tmb        not  = spaces
                     go to acc-cod-tmb-405.
           if        v-key                =    "UP  "
                     go to acc-cod-tmb-405
           else      go to acc-cod-tmb-100.
       acc-cod-tmb-405.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori associati al tipo    *
      *                  * movimento                                   *
      *                  *---------------------------------------------*
           move      w-let-arc-ybf-mif    to   w-tes-cod-tmb-mif      .
           move      w-let-arc-ybf-tmf    to   w-tes-cod-tmb-tmf      .
           move      w-let-arc-ybf-cam    to   w-tes-cod-tmb-cam      .
           move      w-let-arc-ybf-ctm    to   w-tes-cod-tmb-ctm      .
           move      w-let-arc-ybf-dfa    to   w-tes-cod-tmb-dfa      .
           move      w-let-arc-ybf-vaa    to   w-tes-cod-tmb-vaa      .
           move      w-let-arc-ybf-lsa    to   w-tes-cod-tmb-lsa      .
           move      w-let-arc-ybf-ord    to   w-tes-cod-tmb-ord      .
           move      w-let-arc-ybf-prd    to   w-tes-cod-tmb-prd      .
           move      w-let-arc-ybf-maf    to   w-tes-cod-tmb-maf      .
           move      w-let-arc-ybf-dmf    to   w-tes-cod-tmb-dmf      .
           move      w-let-arc-ybf-tvd    to   w-tes-cod-tmb-tvd      .
           move      w-let-arc-ybf-dsl    to   w-tes-cod-tmb-ddd      .
           move      w-let-arc-ybf-dtr    to   w-tes-cod-tmb-dtr      .
      *                  *---------------------------------------------*
      *                  * Lettura archivio [ybf] relativo alla dipen- *
      *                  * denza                                       *
      *                  *---------------------------------------------*
           move      w-tes-cod-tmb        to   w-let-arc-ybf-cod      .
           move      w-tes-cod-dpz        to   w-let-arc-ybf-dpz      .
           perform   let-arc-ybf-000      thru let-arc-ybf-999        .
       acc-cod-tmb-410.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo validita'   *
      *                  * per le dipendenze                           *
      *                  *---------------------------------------------*
           if        w-tes-cod-tmb-tvd    =    spaces
                     go to acc-cod-tmb-411
           else if   w-tes-cod-tmb-tvd    =    "S"
                     go to acc-cod-tmb-412
           else if   w-tes-cod-tmb-tvd    =    "D"
                     go to acc-cod-tmb-413
           else if   w-tes-cod-tmb-tvd    =    "X"
                     go to acc-cod-tmb-414.
       acc-cod-tmb-411.
      *                  *---------------------------------------------*
      *                  * Tipo validita' per le dipendenze : Tutte    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     acc-cod-tmb-420.
       acc-cod-tmb-412.
      *                  *---------------------------------------------*
      *                  * Tipo validita' per le dipendenze : Solo la  *
      *                  * Sede                                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su codice dipendenza in uso        *
      *                      *-----------------------------------------*
           if        w-tes-cod-dpz       not  = 1
                     go to acc-cod-tmb-418.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     acc-cod-tmb-420.
       acc-cod-tmb-413.
      *                  *---------------------------------------------*
      *                  * Tipo validita' per le dipendenze : Solo le  *
      *                  * Dipendenze                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su codice dipendenza in uso        *
      *                      *-----------------------------------------*
           if        w-tes-cod-dpz       =    1
                     go to acc-cod-tmb-418.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     acc-cod-tmb-420.
       acc-cod-tmb-414.
      *                  *---------------------------------------------*
      *                  * Tipo validita' per le dipendenze : Solo le  *
      *                  * Dipendenze indicate                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se record esistente                *
      *                      *-----------------------------------------*
           if        w-let-arc-ybf-flg    not  = spaces
                     go to acc-cod-tmb-418.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     acc-cod-tmb-420.
       acc-cod-tmb-418.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Tipo movimento incompatibile con la dipendenza in 
      -              "uso !          "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-cod-tmb-100.
       acc-cod-tmb-420.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione codice dislocazione         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se record esistente si bufferizza il    *
      *                      * codice dislocazione in esso contenuto,  *
      *                      * altrimenti si bufferizza quello di de-  *
      *                      * fault                                   *
      *                      *-----------------------------------------*
           if        w-let-arc-ybf-flg    =    spaces
                     move  w-let-arc-ybf-dsl
                                          to   w-tes-cod-tmb-dsl
           else      move  w-tes-cod-tmb-ddd
                                          to   w-tes-cod-tmb-dsl      .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione tipo movimento per fattura  *
      *                  *---------------------------------------------*
           move      w-tes-cod-tmb-tmf    to   w-tes-tmo-ftr          .
       acc-cod-tmb-500.
      *                  *---------------------------------------------*
      *                  * Parametri associati alla causale di magaz-  *
      *                  * zino                                        *
      *                  *---------------------------------------------*
       acc-cod-tmb-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-tmb-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-tmb-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-tmb-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-tmb-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-tmb-999.
       acc-cod-tmb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : tipo movimento per la bolla*
      *    *-----------------------------------------------------------*
       vis-cod-tmb-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-tes-cod-tmb        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-tmb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : descrizione tipo movimento *
      *    *-----------------------------------------------------------*
       vis-cod-tmb-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      28                   to   v-pos                  .
           move      w-tes-cod-tmb-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-tmb-des-999.
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
       acc-nok-reg-325.
      *                          *-------------------------------------*
      *                          * Decremento numero pagina attuale    *
      *                          *-------------------------------------*
           subtract  1                    from w-cnt-sts-imp-npt      .
      *                          *-------------------------------------*
      *                          * Test se pagina da trattare          *
      *                          *-------------------------------------*
           perform   snp-tes-reg-000      thru snp-tes-reg-999        .
      *                          *-------------------------------------*
      *                          * Se no : decremento ulteriore        *
      *                          *-------------------------------------*
           if        w-cnt-sts-imp-snp    not  = spaces and
                     w-cnt-sts-imp-npt    >    1
                     go to acc-nok-reg-325.
      *                          *-------------------------------------*
      *                          * Riciclo ad impostazione testata     *
      *                          *-------------------------------------*
           go to     acc-nok-reg-200.
       acc-nok-reg-400.
      *                      *-----------------------------------------*
      *                      * Se spostamento a pagina successiva      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Salvataggio numero pagina attuale   *
      *                          *-------------------------------------*
           move      w-cnt-sts-imp-npt    to   w-cnt-sts-imp-svp      .
       acc-nok-reg-425.
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
                     move  w-cnt-sts-imp-svp
                                          to   w-cnt-sts-imp-npt
                     go to acc-nok-reg-800.
      *                          *-------------------------------------*
      *                          * Incremento numero pagina attuale    *
      *                          *-------------------------------------*
           add       1                    to   w-cnt-sts-imp-npt      .
      *                          *-------------------------------------*
      *                          * Test se pagina da trattare          *
      *                          *-------------------------------------*
           perform   snp-tes-reg-000      thru snp-tes-reg-999        .
      *                          *-------------------------------------*
      *                          * Se no : a re-incremento             *
      *                          *-------------------------------------*
           if        w-cnt-sts-imp-snp    not  = spaces
                     go to acc-nok-reg-425.
      *                          *-------------------------------------*
      *                          * Status di visualizzazione prompts e *
      *                          * dati della chiave a : no            *
      *                          *-------------------------------------*
           move      spaces               to   w-cnt-sts-pmt-key      .
           move      spaces               to   w-cnt-sts-vis-key      .
      *                          *-------------------------------------*
      *                          * Status di visualizzazione prompts e *
      *                          * dati della pagina impostata a : no  *
      *                          *-------------------------------------*
           move      spaces               to   w-cnt-sts-pmt-ptx
                                              (w-cnt-sts-imp-svp)     .
           move      spaces               to   w-cnt-sts-vis-ptx
                                              (w-cnt-sts-imp-svp)     .
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
      *              * La testata e' composta di nr. 1 pagina          *
      *              *-------------------------------------------------*
           move      1                    to   w-cnt-sts-imp-mpt      .
       dmp-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Determinazione si/no pagina w-cnt-sts-imp-npt da trattare *
      *    *-----------------------------------------------------------*
       snp-tes-reg-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-imp-snp      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del numero pagina        *
      *              *-------------------------------------------------*
           go to     snp-tes-reg-100
                     depending            on   w-cnt-sts-imp-npt      .
           go to     snp-tes-reg-999.
       snp-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Test per pagina 1                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     snp-tes-reg-999.
       snp-tes-reg-999.
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
      *                  * Flag di bolla chiusa                        *
      *                  *---------------------------------------------*
           perform   acc-flg-bch-000      thru acc-flg-bch-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
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
      *              * Codice fornitore                                *
      *              *-------------------------------------------------*
           perform   vis-fnt-org-000      thru vis-fnt-org-999        .
           perform   vis-fnt-org-des-000  thru vis-fnt-org-des-999    .
      *              *-------------------------------------------------*
      *              * Flag di bolla chiusa                            *
      *              *-------------------------------------------------*
           perform   vis-flg-bch-000      thru vis-flg-bch-999        .
      *              *-------------------------------------------------*
      *              * Status bolla                                    *
      *              *-------------------------------------------------*
           perform   vis-sts-bfo-000      thru vis-sts-bfo-999        .
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
           move      07                   to   v-lin                  .
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
      *              * Codice fornitore                                *
      *              *-------------------------------------------------*
           perform   pmt-fnt-org-000      thru pmt-fnt-org-999        .
      *              *-------------------------------------------------*
      *              * Flag di bolla chiusa                            *
      *              *-------------------------------------------------*
           perform   pmt-flg-bch-000      thru pmt-flg-bch-999        .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice fornitore                 *
      *    *-----------------------------------------------------------*
       pmt-fnt-org-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      25                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Fornitore commerciale   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-fnt-org-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Flag di bolla chiusa             *
      *    *-----------------------------------------------------------*
       pmt-flg-bch-000.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      25                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Chiusura bolla          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazione Note                            *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "N.B.: Il programma pone a stato di 'chiuso' l'inte
      -              "ra bolla, righe comprese,     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "      anche se inevasa o parzialmente evasa.      
      -              "                              "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "  *** ATTENZIONE che gli eventuali ordini evasi da
      -              "l documento siano, a loro *** "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "  *** volta, chiusi.                              
      -              "                          *** "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-flg-bch-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Presa visione per pagina     *
      *    *-----------------------------------------------------------*
       acc-pre-vpg-000.
      *              *-------------------------------------------------*
      *              * Se non esiste alcuna pagina attiva successiva   *
      *              * alla pagina attuale : uscita                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio numero pagina attuale           *
      *                  *---------------------------------------------*
           move      w-cnt-sts-imp-npt    to   w-cnt-sts-imp-svp      .
       acc-pre-vpg-100.
      *                  *---------------------------------------------*
      *                  * Se all'ultima pagina : ripristino pagina    *
      *                  * salvata ed uscita                           *
      *                  *---------------------------------------------*
           if        w-cnt-sts-imp-npt    not  < w-cnt-sts-imp-mpt
                     move  w-cnt-sts-imp-svp
                                          to   w-cnt-sts-imp-npt
                     go to acc-pre-vpg-999.
      *                  *---------------------------------------------*
      *                  * Incremento numero pagina                    *
      *                  *---------------------------------------------*
           add       1                    to   w-cnt-sts-imp-npt      .
      *                  *---------------------------------------------*
      *                  * Test se pagina da trattare                  *
      *                  *---------------------------------------------*
           perform   snp-tes-reg-000      thru snp-tes-reg-999        .
      *                  *---------------------------------------------*
      *                  * Se no : a re-incremento                     *
      *                  *---------------------------------------------*
           if        w-cnt-sts-imp-snp    not  = spaces
                     go to acc-pre-vpg-100.
      *                  *---------------------------------------------*
      *                  * Se si : ripristino pagina salvata e accet-  *
      *                  * tazione presa visione                       *
      *                  *---------------------------------------------*
           move      w-cnt-sts-imp-svp    to   w-cnt-sts-imp-npt      .
       acc-pre-vpg-200.
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
      *    * Visualizzazione campo testata : Codice fornitore          *
      *    *-----------------------------------------------------------*
       vis-fnt-org-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      08                   to   v-lin                  .
           move      27                   to   v-pos                  .
           move      "<B"                 to   v-edm                  .
           move      w-tes-fnt-org (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-fnt-org-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione fornitore     *
      *    *-----------------------------------------------------------*
       vis-fnt-org-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-fnt-org-rag (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-fnt-org-via (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-fnt-org-loc (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-fnt-org-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Flag di bolla chiusa         *
      *    *-----------------------------------------------------------*
       acc-flg-bch-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-flg-bch-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-flg-bch-lun    to   v-car                  .
           move      w-exp-flg-bch-num    to   v-ldt                  .
           move      "NS#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      12                   to   v-lin                  .
           move      27                   to   v-pos                  .
           move      w-exp-flg-bch-tbl    to   v-txt                  .
           if        w-tes-flg-bch (1)    =    spaces
                     move  01             to   v-num
           else if   w-tes-flg-bch (1)    =    "#"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-flg-bch-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-flg-bch-999.
       acc-flg-bch-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  spaces         to   w-tes-flg-bch (1)
           else if   v-num                =    02
                     move  "#"            to   w-tes-flg-bch (1)
           else      move  spaces         to   w-tes-flg-bch (1)      .
       acc-flg-bch-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-flg-bch-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-flg-bch-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-flg-bch-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-flg-bch-100.
       acc-flg-bch-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Flag di bolla chiusa      *
      *    *-----------------------------------------------------------*
       vis-flg-bch-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-flg-bch-lun    to   v-car                  .
           move      w-exp-flg-bch-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      12                   to   v-lin                  .
           move      27                   to   v-pos                  .
           move      w-exp-flg-bch-tbl    to   v-txt                  .
           if        w-tes-flg-bch (1)    =    spaces
                     move  01             to   v-num
           else if   w-tes-flg-bch (1)    =    "#"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-flg-bch-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Status bolla              *
      *    *-----------------------------------------------------------*
       vis-sts-bfo-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      41                   to   v-pos                  .
           if        rf-bft-flg-bch       not  = spaces
                     move  "Chiusa       "
                                          to   v-alf
           else      move  "Aperta       "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sts-bfo-999.
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
      *              *-------------------------------------------------*
      *              * Test su tipo movimento                          *
      *              *-------------------------------------------------*
           if        w-tes-cod-tmb        not  = spaces
                     go to cnt-tdo-key-100.
           move      "Manca il tipo movimento !                         
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-key-900.
       cnt-tdo-key-100.
      *              *-------------------------------------------------*
      *              * Test su data registrazione                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se mancante                            *
      *                  *---------------------------------------------*
           if        w-tes-dat-reg        not  = zero
                     go to cnt-tdo-key-150.
           move      "Manca la data registrazione !                     
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-key-900.
       cnt-tdo-key-150.
      *                  *---------------------------------------------*
      *                  * Se numero protocollo a zero                 *
      *                  *---------------------------------------------*
           if        w-tes-num-prt        not  = zero
                     go to cnt-tdo-key-200.
       cnt-tdo-key-200.
      *              *-------------------------------------------------*
      *              * Test su sottoconto iva acquisti                 *
      *              *-------------------------------------------------*
           if        w-ref-stc-iac-cod    not  = zero
                     go to cnt-tdo-key-300.
           move      "Sottoconto iva acquisti indeterminato !           
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-key-900.
       cnt-tdo-key-300.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
       cnt-tdo-key-800.
      *              *-------------------------------------------------*
      *              * Uscita per controlli superati                   *
      *              *-------------------------------------------------*
           go to     cnt-tdo-key-999.
       cnt-tdo-key-900.
      *              *-------------------------------------------------*
      *              * Trattamento errore                              *
      *              *-------------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *              *-------------------------------------------------*
      *              * Flag di uscita ad errore                        *
      *              *-------------------------------------------------*
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
      *              * Test : tipo movimento                           *
      *              *-------------------------------------------------*
           if        w-tes-cod-tmb        =    spaces and
                     w-tes-dat-reg        =    zero   and
                     w-tes-num-prt        =    zero
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
           move      zero                 to   w-tes-num-prt          .
           move      spaces               to   w-tes-cod-tmb          .
           move      spaces               to   w-tes-cod-tmb-des      .
           move      zero                 to   w-tes-cod-tmb-mif      .
           move      spaces               to   w-tes-cod-tmb-tmf      .
           move      zero                 to   w-tes-cod-tmb-cam      .
           move      zero                 to   w-tes-cod-tmb-cam-tmc  .
           move      spaces               to   w-tes-cod-tmb-cam-dtm  .
           move      spaces               to   w-tes-cod-tmb-cam-vtm  .
           move      spaces               to   w-tes-cod-tmb-cam-ltm  .
           move      spaces               to   w-tes-cod-tmb-ctm      .
           move      spaces               to   w-tes-cod-tmb-dfa      .
           move      spaces               to   w-tes-cod-tmb-vaa      .
           move      spaces               to   w-tes-cod-tmb-lsa      .
           move      zero                 to   w-tes-cod-tmb-ord      .
           move      zero                 to   w-tes-cod-tmb-prd      .
           move      zero                 to   w-tes-cod-tmb-maf      .
           move      spaces               to   w-tes-cod-tmb-dmf      .
           move      spaces               to   w-tes-cod-tmb-tvd      .
           move      spaces               to   w-tes-cod-tmb-ddd      .
           move      spaces               to   w-tes-cod-tmb-dsl      .
           move      zero                 to   w-tes-int-ftr          .
           move      spaces               to   w-tes-tmo-ftr          .
           move      zero                 to   w-tes-dat-reg          .
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *-----------------------------------------------------------*
       nor-nok-tes-000.
           move      zero                 to   w-tes-fnt-org (1)      .
           move      spaces               to   w-tes-fnt-org-rag (1)  .
           move      spaces               to   w-tes-fnt-org-via (1)  .
           move      spaces               to   w-tes-fnt-org-loc (1)  .
           move      spaces               to   w-tes-flg-bch (1)      .
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
      *              * Lettura testata                                 *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "IDEDOC    "         to   f-key                  .
           move      w-tes-dat-reg        to   rf-bft-dat-reg         .
           move      w-tes-cod-dpz        to   rf-bft-cod-dpz         .
           move      w-tes-cod-tmb        to   rf-bft-cod-tmb         .
           move      w-tes-num-prt        to   rf-bft-num-prt         .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *                  *---------------------------------------------*
      *                  * Se registrazione esistente : a Modifica     *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to rou-let-reg-100.
       rou-let-reg-050.
      *                  *---------------------------------------------*
      *                  * Se movimento non trovato                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Documento non esistente in archivio !             
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * Flag di uscita ad errore                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-rou-let-reg      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     rou-let-reg-999.
       rou-let-reg-100.
      *                  *---------------------------------------------*
      *                  * Se movimento trovato                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se bolla chiusa                    *
      *                      *-----------------------------------------*
           if        rf-bft-flg-bch       =    spaces
                     go to rou-let-reg-120.
      *                          *-------------------------------------*
      *                          * Messaggio di errore                 *
      *                          *-------------------------------------*
           move      "Bolla fornitore gia' chiusa !                     
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       rou-let-reg-120.
      *                      *-----------------------------------------*
      *                      * Tipo funzionamento : Modifica           *
      *                      *-----------------------------------------*
           move      "M"                  to   w-cnt-mfu-tip-fun      .
      *                      *-----------------------------------------*
      *                      * Completamento valori attuali chiave     *
      *                      *-----------------------------------------*
           move      w-tes-cod-tmb-mif    to   w-tes-int-ftr          .
      *                      *-----------------------------------------*
      *                      * Determinazione valori attuali testata   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Valori contenuti direttamente in    *
      *                          * record [bft]                        *
      *                          *-------------------------------------*
           move      rf-bft-cod-arc       to   w-tes-fnt-org (1)      .
      *                          *-------------------------------------*
      *                          * Valori contenuti indirettamente in  *
      *                          * record [bft]                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Anagrafica archivio             *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Lettura record [dcf] prin-  *
      *                                  * cipale                      *
      *                                  *-----------------------------*
           move      w-tes-fnt-org (1)    to   w-let-arc-dcf-fnt      .
           move      spaces               to   w-let-arc-dcf-dpz      .
           perform   let-arc-dcf-000      thru let-arc-dcf-999        .
           move      w-let-arc-dcf-rag    to   w-tes-fnt-org-rag (1)  .
           move      w-let-arc-dcf-via    to   w-tes-fnt-org-via (1)  .
           move      w-let-arc-dcf-loc    to   w-tes-fnt-org-loc (1)  .
      *                      *-----------------------------------------*
      *                      * Valori precedenti anagrafica            *
      *                      *-----------------------------------------*
           move      w-tes-val-aep (1)    to   w-tes-val-aep (2)      .
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
      *    * Routine pre-richiesta di ratifica tasto Delete            *
      *    *-----------------------------------------------------------*
       pre-snx-del-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-snx-del      .
      *              *-------------------------------------------------*
      *              * Disabilitazione manuale tasto Delt              *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-pre-snx-del      .
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
       pos-cnf-ins-999.
           exit.

      *    *===========================================================*
      *    * Routine post-conferma di modifica                         *
      *    *-----------------------------------------------------------*
       pos-cnf-mod-000.
      *              *-------------------------------------------------*
      *              * Aggiornamento della data di registrazione di    *
      *              * default                                         *
      *              *-------------------------------------------------*
           move      w-tes-dat-reg        to   w-def-dat-reg          .
      *              *-------------------------------------------------*
      *              * Aggiornamento del tipo movimento di default     *
      *              *-------------------------------------------------*
           move      w-tes-cod-tmb        to   w-def-cod-tmb          .
           move      w-tes-cod-tmb-des    to   w-def-cod-tmb-des      .
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
       pos-cnf-ann-999.
           exit.

      *    *===========================================================*
      *    * Scrittura movimento su file                               *
      *    *-----------------------------------------------------------*
       scr-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Trattamento righe bolla                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatore righe scritte    *
      *                  *---------------------------------------------*
           move      zero                 to   w-scr-mov-fil-ctr      .
      *                  *---------------------------------------------*
      *                  * Start su file [bfr]                         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-tes-num-prt        to   rf-bfr-num-prt         .
           move      zero                 to   rf-bfr-num-prg         .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *                      *-----------------------------------------*
      *                      * Test su esito operazione                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to scr-mov-fil-500.
       scr-mov-fil-100.
      *                  *---------------------------------------------*
      *                  * Ottenimento sequenziale record [bfr]        *
      *                  *---------------------------------------------*
           move      "GN"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *                      *-----------------------------------------*
      *                      * Test se 'At End'                        *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to scr-mov-fil-500.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
           if        rf-bfr-num-prt       not  = w-tes-num-prt
                     go to scr-mov-fil-500.
       scr-mov-fil-120.
      *                  *---------------------------------------------*
      *                  * Selezioni sul record                        *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Fine selezione sul record                   *
      *                  *---------------------------------------------*
           go to     scr-mov-fil-200.
       scr-mov-fil-160.
      *                  *---------------------------------------------*
      *                  * Se selezione sul record non superata        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Release record [bfr]                    *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *                      *-----------------------------------------*
      *                      * Riciclo                                 *
      *                      *-----------------------------------------*
           go to     scr-mov-fil-100.
       scr-mov-fil-200.
      *                  *---------------------------------------------*
      *                  * Incremento contatore righe scritte          *
      *                  *---------------------------------------------*
           add       1                    to   w-scr-mov-fil-ctr      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento record [bfr]                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di riga chiusa                     *
      *                      *-----------------------------------------*
           move      w-tes-flg-bch (1)    to   rf-bfr-flg-rch         .
      *                      *-----------------------------------------*
      *                      * Update record [bfr]                     *
      *                      *-----------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *                      *-----------------------------------------*
      *                      * Release record [bfr]                    *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
       scr-mov-fil-220.
      *                  *---------------------------------------------*
      *                  * Riciclo a riga successiva                   *
      *                  *---------------------------------------------*
           go to     scr-mov-fil-100.
       scr-mov-fil-500.
      *              *-------------------------------------------------*
      *              * Release ultimo record [bfr]                     *
      *              *-------------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *              *-------------------------------------------------*
      *              * Test su contatore righe scritte                 *
      *              *-------------------------------------------------*
           if        w-scr-mov-fil-ctr    >    zero
                     go to scr-mov-fil-600.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Operazione non eseguita.                           
      -              "               "    to   w-err-box-err-msg      .
           move      "Nessuna riga per la bolla richiamata.             
      -              "               "    to   w-err-box-err-m02      .
           perform   box-msg-e02-000      thru box-msg-e02-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     scr-mov-fil-999.
       scr-mov-fil-600.
      *              *-------------------------------------------------*
      *              * Aggiornamento record [bft]                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ottenimento record [bft]                    *
      *                  *---------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-tes-num-prt        to   rf-bft-num-prt         .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *                  *---------------------------------------------*
      *                  * Flag di bolla chiusa                        *
      *                  *---------------------------------------------*
           move      w-tes-flg-bch (1)    to   rf-bft-flg-bch         .
      *                  *---------------------------------------------*
      *                  * Update record [bft]                         *
      *                  *---------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *                  *---------------------------------------------*
      *                  * Rilascio record [bft]                       *
      *                  *---------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
       scr-mov-fil-800.
      *              *-------------------------------------------------*
      *              * Messaggio di operazione completata              *
      *              *-------------------------------------------------*
           move      "Operazione eseguita con successo.                 
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     scr-mov-fil-999.
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Find su archivio [bft]                                    *
      *    *-----------------------------------------------------------*
       fnd-arc-bft-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-fnd-arc-bft-sel      .
       fnd-arc-bft-010.
      *              *-------------------------------------------------*
      *              * Test preliminare                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se sono stati preparati i valori in input   *
      *                  * si procede come se ricerca gia' effettuata  *
      *                  *---------------------------------------------*
           if        w-fnd-arc-bft-tmb    not  = spaces or
                     w-fnd-arc-bft-dat    not  = zero   or
                     w-fnd-arc-bft-prt    not  = zero
                     go to fnd-arc-bft-080.
       fnd-arc-bft-030.
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pbfo3010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     move  "#"            to   w-fnd-arc-bft-sel
                     go to  fnd-arc-bft-999.
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
      *              * Preparazione variabile di i.p.c. per codice di- *
      *              * pendenza                                        *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cod-dpz"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      02                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      w-tes-cod-dpz        to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Richiamo programma di interrogazione            *
      *              *-------------------------------------------------*
           move      "pgm/bfo/prg/obj/pbfo3010"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
       fnd-arc-bft-050.
      *              *-------------------------------------------------*
      *              * Estrazione di eventuali variabili di i.p.c. de- *
      *              * terminate da function-key "SLCT" durante l'in-  *
      *              * terrogazione                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo movimento                              *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cod-tmb"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     move  "#"            to   w-fnd-arc-bft-sel
                     go to fnd-arc-bft-999.
           move      s-alf                to   w-fnd-arc-bft-tmb      .
      *                  *---------------------------------------------*
      *                  * Data registrazione                          *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "dat-reg"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     move  "#"            to   w-fnd-arc-bft-sel
                     go to fnd-arc-bft-999.
           move      s-dat                to   w-fnd-arc-bft-dat      .
      *                  *---------------------------------------------*
      *                  * Numero protocollo                           *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "num-prt"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     move  "#"            to   w-fnd-arc-bft-sel
                     go to fnd-arc-bft-999.
           move      s-num                to   w-fnd-arc-bft-prt      .
       fnd-arc-bft-080.
      *              *-------------------------------------------------*
      *              * Salvataggio valori chiave                       *
      *              *-------------------------------------------------*
           move      w-tes-val-key        to   w-sav-val-key          .
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori estratti                 *
      *              *-------------------------------------------------*
           move      w-fnd-arc-bft-tmb    to   w-tes-cod-tmb          .
           move      w-fnd-arc-bft-dat    to   w-tes-dat-reg          .
           move      w-fnd-arc-bft-prt    to   w-tes-num-prt          .
       fnd-arc-bft-100.
      *              *-------------------------------------------------*
      *              * Determinazione campi derivati e effettuazione   *
      *              * controlli come se fossero stati impostati       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [ybf] generale             *
      *                  *---------------------------------------------*
           move      w-tes-cod-tmb        to   w-let-arc-ybf-cod      .
           move      zero                 to   w-let-arc-ybf-dpz      .
           perform   let-arc-ybf-000      thru let-arc-ybf-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : errore                   *
      *                  *---------------------------------------------*
           if        w-let-arc-ybf-flg    not  = spaces
                     go to fnd-arc-bft-900.
      *                  *---------------------------------------------*
      *                  * Se a spaces : errore                        *
      *                  *---------------------------------------------*
           if        w-tes-cod-tmb        =    spaces
                     go to fnd-arc-bft-900.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori associati al tipo    *
      *                  * movimento                                   *
      *                  *---------------------------------------------*
           move      w-let-arc-ybf-des    to   w-tes-cod-tmb-des      .
           move      w-let-arc-ybf-mif    to   w-tes-cod-tmb-mif      .
           move      w-let-arc-ybf-tmf    to   w-tes-cod-tmb-tmf      .
           move      w-let-arc-ybf-cam    to   w-tes-cod-tmb-cam      .
           move      w-let-arc-ybf-ctm    to   w-tes-cod-tmb-ctm      .
           move      w-let-arc-ybf-dfa    to   w-tes-cod-tmb-dfa      .
           move      w-let-arc-ybf-vaa    to   w-tes-cod-tmb-vaa      .
           move      w-let-arc-ybf-lsa    to   w-tes-cod-tmb-lsa      .
           move      w-let-arc-ybf-ord    to   w-tes-cod-tmb-ord      .
           move      w-let-arc-ybf-prd    to   w-tes-cod-tmb-prd      .
           move      w-let-arc-ybf-maf    to   w-tes-cod-tmb-maf      .
           move      w-let-arc-ybf-dmf    to   w-tes-cod-tmb-dmf      .
           move      w-let-arc-ybf-tvd    to   w-tes-cod-tmb-tvd      .
           move      w-let-arc-ybf-dsl    to   w-tes-cod-tmb-ddd      .
           move      w-let-arc-ybf-dtr    to   w-tes-cod-tmb-dtr      .
      *                  *---------------------------------------------*
      *                  * Lettura archivio [ybf] relativo alla dipen- *
      *                  * denza                                       *
      *                  *---------------------------------------------*
           move      w-tes-cod-tmb        to   w-let-arc-ybf-cod      .
           move      w-tes-cod-dpz        to   w-let-arc-ybf-dpz      .
           perform   let-arc-ybf-000      thru let-arc-ybf-999        .
       fnd-arc-bft-410.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo validita'   *
      *                  * per le dipendenze                           *
      *                  *---------------------------------------------*
           if        w-tes-cod-tmb-tvd    =    spaces
                     go to fnd-arc-bft-411
           else if   w-tes-cod-tmb-tvd    =    "S"
                     go to fnd-arc-bft-412
           else if   w-tes-cod-tmb-tvd    =    "D"
                     go to fnd-arc-bft-413
           else if   w-tes-cod-tmb-tvd    =    "X"
                     go to fnd-arc-bft-414.
       fnd-arc-bft-411.
      *                  *---------------------------------------------*
      *                  * Tipo validita' per le dipendenze : Tutte    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     fnd-arc-bft-420.
       fnd-arc-bft-412.
      *                  *---------------------------------------------*
      *                  * Tipo validita' per le dipendenze : Solo la  *
      *                  * Sede                                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su codice dipendenza in uso        *
      *                      *-----------------------------------------*
           if        w-tes-cod-dpz       not  = 1
                     go to fnd-arc-bft-418.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     fnd-arc-bft-420.
       fnd-arc-bft-413.
      *                  *---------------------------------------------*
      *                  * Tipo validita' per le dipendenze : Solo le  *
      *                  * Dipendenze                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su codice dipendenza in uso        *
      *                      *-----------------------------------------*
           if        w-tes-cod-dpz       =    1
                     go to fnd-arc-bft-418.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     fnd-arc-bft-420.
       fnd-arc-bft-414.
      *                  *---------------------------------------------*
      *                  * Tipo validita' per le dipendenze : Solo le  *
      *                  * Dipendenze indicate                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se record esistente                *
      *                      *-----------------------------------------*
           if        w-let-arc-ybf-flg    not  = spaces
                     go to fnd-arc-bft-418.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     fnd-arc-bft-420.
       fnd-arc-bft-418.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Tipo movimento incompatibile con la dipendenza in 
      -              "uso !          "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * A errore                                    *
      *                  *---------------------------------------------*
           go to     fnd-arc-bft-900.
       fnd-arc-bft-420.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione codice dislocazione         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se record esistente si bufferizza il    *
      *                      * codice dislocazione in esso contenuto,  *
      *                      * altrimenti si bufferizza quello di de-  *
      *                      * fault                                   *
      *                      *-----------------------------------------*
           if        w-let-arc-ybf-flg    =    spaces
                     move  w-let-arc-ybf-dsl
                                          to   w-tes-cod-tmb-dsl
           else      move  w-tes-cod-tmb-ddd
                                          to   w-tes-cod-tmb-dsl      .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione tipo movimento per fattura  *
      *                  *---------------------------------------------*
           move      w-tes-cod-tmb-tmf    to   w-tes-tmo-ftr          .
       fnd-arc-bft-500.
      *                  *---------------------------------------------*
      *                  * Parametri associati alla causale di magaz-  *
      *                  * zino                                        *
      *                  *---------------------------------------------*
       fnd-arc-bft-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Completamento visualizzazione campi-chiave  *
      *                  *---------------------------------------------*
           perform   vis-cod-tmb-000      thru vis-cod-tmb-999        .
           perform   vis-cod-tmb-des-000  thru vis-cod-tmb-des-999    .
           perform   vis-dat-reg-000      thru vis-dat-reg-999        .
           perform   vis-num-prt-000      thru vis-num-prt-999        .
      *                  *---------------------------------------------*
      *                  * Uscita con successo                         *
      *                  *---------------------------------------------*
           go to     fnd-arc-bft-999.
       fnd-arc-bft-900.
      *              *-------------------------------------------------*
      *              * Trattamento errore                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripristino valori chiave salvati            *
      *                  *---------------------------------------------*
           move      w-sav-val-key        to   w-tes-val-key          .
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-fnd-arc-bft-sel      .
       fnd-arc-bft-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [ybf]                         *
      *    *-----------------------------------------------------------*
       let-arc-ybf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-ybf-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spazi                          *
      *              *-------------------------------------------------*
           if        w-let-arc-ybf-cod    =    spaces
                     go to let-arc-ybf-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODTMB"             to   f-key                  .
           move      w-let-arc-ybf-cod    to   rf-ybf-cod-tmb         .
           move      w-let-arc-ybf-dpz    to   rf-ybf-cod-dpz         .
           move      "pgm/bfo/fls/ioc/obj/iofybf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ybf                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-ybf-400.
       let-arc-ybf-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-ybf-des-tmb       to   w-let-arc-ybf-des      .
           move      rf-ybf-int-ftr       to   w-let-arc-ybf-mif      .
           move      rf-ybf-tmo-ftr       to   w-let-arc-ybf-tmf      .
           move      rf-ybf-cau-mag       to   w-let-arc-ybf-cam      .
           move      rf-ybf-cod-mic       to   w-let-arc-ybf-ctm      .
           move      rf-ybf-def-tar       to   w-let-arc-ybf-dfa      .
           move      rf-ybf-snv-tar       to   w-let-arc-ybf-vaa      .
           move      rf-ybf-lst-tar       to   w-let-arc-ybf-lsa      .
           move      rf-ybf-org-doc       to   w-let-arc-ybf-ord      .
           move      rf-ybf-prv-doc       to   w-let-arc-ybf-prd      .
           move      rf-ybf-mov-afd       to   w-let-arc-ybf-maf      .
           move      rf-ybf-def-tmf       to   w-let-arc-ybf-dmf      .
           move      rf-ybf-vld-dpz       to   w-let-arc-ybf-tvd      .
           move      rf-ybf-cod-dsl       to   w-let-arc-ybf-dsl      .
           move      rf-ybf-def-tpr       to   w-let-arc-ybf-dtr      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-ybf-999.
       let-arc-ybf-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-ybf-flg      .
           move      all   "."            to   w-let-arc-ybf-des      .
           go to     let-arc-ybf-600.
       let-arc-ybf-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-ybf-des      .
       let-arc-ybf-600.
           move      zero                 to   w-let-arc-ybf-mif      .
           move      spaces               to   w-let-arc-ybf-tmf      .
           move      spaces               to   w-let-arc-ybf-acm      .
           move      spaces               to   w-let-arc-ybf-dtc      .
           move      spaces               to   w-let-arc-ybf-dct      .
           move      zero                 to   w-let-arc-ybf-cam      .
           move      spaces               to   w-let-arc-ybf-ctm      .
           move      spaces               to   w-let-arc-ybf-dfa      .
           move      spaces               to   w-let-arc-ybf-vaa      .
           move      spaces               to   w-let-arc-ybf-lsa      .
           move      zero                 to   w-let-arc-ybf-ord      .
           move      zero                 to   w-let-arc-ybf-prd      .
           move      spaces               to   w-let-arc-ybf-sgl      .
           move      zero                 to   w-let-arc-ybf-maf      .
           move      spaces               to   w-let-arc-ybf-dmf      .
           move      spaces               to   w-let-arc-ybf-tvd      .
           move      spaces               to   w-let-arc-ybf-dsl      .
           move      spaces               to   w-let-arc-ybf-dtr      .
       let-arc-ybf-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [fnt]                         *
      *    *-----------------------------------------------------------*
       let-arc-fnt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-fnt-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-fnt-cod    =    zero
                     go to let-arc-fnt-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFNT"             to   f-key                  .
           move      w-let-arc-fnt-cod    to   rf-fnt-cod-fnt         .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-fnt-400.
       let-arc-fnt-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-fnt-rag-soc       to   w-let-arc-fnt-rag      .
           move      rf-fnt-via-fnt       to   w-let-arc-fnt-via      .
           move      rf-fnt-loc-fnt       to   w-let-arc-fnt-loc      .
           move      rf-fnt-prt-iva       to   w-let-arc-fnt-piv      .
           move      rf-fnt-cod-cge       to   w-let-arc-fnt-stc      .
           move      rf-fnt-cod-iva       to   w-let-arc-fnt-ass      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-fnt-999.
       let-arc-fnt-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-fnt-flg      .
           move      all   "."            to   w-let-arc-fnt-rag      .
           go to     let-arc-fnt-520.
       let-arc-fnt-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-fnt-rag      .
       let-arc-fnt-520.
           move      spaces               to   w-let-arc-fnt-via      .
           move      spaces               to   w-let-arc-fnt-loc      .
           move      zero                 to   w-let-arc-fnt-piv      .
           move      zero                 to   w-let-arc-fnt-stc      .
           move      zero                 to   w-let-arc-fnt-ass      .
       let-arc-fnt-999.
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
      *              * Test se codici a zero o spazi                   *
      *              *-------------------------------------------------*
           if        w-let-arc-dcf-fnt    =    zero
                     go to let-arc-dcf-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFNT"             to   f-key                  .
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
           move      rf-dcf-via-dcf       to   w-let-arc-dcf-via      .
           move      rf-dcf-loc-dcf       to   w-let-arc-dcf-loc      .
           move      rf-dcf-cod-vlt       to   w-let-arc-dcf-vlt      .
           move      rf-dcf-cod-lng       to   w-let-arc-dcf-lng      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dcf-999.
       let-arc-dcf-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dcf-flg      .
           move      all   "."            to   w-let-arc-dcf-rag      .
           go to     let-arc-dcf-520.
       let-arc-dcf-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcf-rag      .
       let-arc-dcf-520.
           move      spaces               to   w-let-arc-dcf-via      .
           move      spaces               to   w-let-arc-dcf-loc      .
           move      spaces               to   w-let-arc-dcf-vlt      .
           move      spaces               to   w-let-arc-dcf-lng      .
       let-arc-dcf-999.
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
      *    * Subroutines per l'accettazione tipo movimento per bolla   *
      *    *-----------------------------------------------------------*
           copy      "pgm/bfo/prg/cpy/acdeybf0.acs"                   .

