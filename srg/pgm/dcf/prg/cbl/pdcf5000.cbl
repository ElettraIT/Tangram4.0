       Identification Division.
       Program-Id.                                 pdcf5000           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dcf                 *
      *                                Settore:    lst                 *
      *                                   Fase:    dcf500              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 13/06/92    *
      *                       Ultima revisione:    Ndk del 05/06/19    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Condizioni di acquisto da fornitori         *
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
                     "lst"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "dcf500"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pdcf5000"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "   CONDIZIONI DI ACQUISTO DA FORNITORI  "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di definizione della valuta base                     *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/c"                                  .

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
      *        * [aaf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaf"                          .
      *        *-------------------------------------------------------*
      *        * [aaq]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaq"                          .
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [dpm]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dpm/fls/rec/rfdpm"                          .
      *        *-------------------------------------------------------*
      *        * [mtv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mtv/fls/rec/rfmtv"                          .
      *        *-------------------------------------------------------*
      *        * [pdx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfpdx"                          .
      *        *-------------------------------------------------------*
      *        * [zvl]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzvl"                          .
      *        *-------------------------------------------------------*
      *        * [fnt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rffnt"                          .
      *        *-------------------------------------------------------*
      *        * [dcf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfdcf"                          .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Si/No gestione materie prime attiva                   *
      *        *-------------------------------------------------------*
           05  w-prs-dpm-snx              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No gestione materiali vari attiva                  *
      *        *-------------------------------------------------------*
           05  w-prs-mtv-snx              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Modalita' di espressione del tempo di consegna        *
      *        *                                                       *
      *        *  - 0 : In giorni                                      *
      *        *  - 1 : In settimane                                   *
      *        *-------------------------------------------------------*
           05  w-prs-mde-tdc              pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/no gestione formato di acquisto                    *
      *        *-------------------------------------------------------*
           05  w-prs-snx-fda              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Tipo controllo su codice fornitore preferenziale      *
      *        *-------------------------------------------------------*
           05  w-prs-fnt-pfz              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No accettazione riferimenti al listino             *
      *        *-------------------------------------------------------*
           05  w-prs-rif-lst              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazioni relative ai default di inserimento  *
      *        *-------------------------------------------------------*
           05  w-prs-def-ins.
      *            *---------------------------------------------------*
      *            * Valori letti da formato base                      *
      *            *---------------------------------------------------*
               10  w-prs-def-ins-fmb      pic  9(02)                  .
               10  filler                 pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Descrizione di default                            *
      *            *---------------------------------------------------*
               10  w-prs-def-ins-dad      pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per valori di i.p.c.                            *
      *    *-----------------------------------------------------------*
       01  w-ipc.
      *        *-------------------------------------------------------*
      *        * Variabili di i.p.c. da livello precedente             *
      *        *-------------------------------------------------------*
           05  w-ipc-dlp.
      *            *---------------------------------------------------*
      *            * Per valori chiave                                 *
      *            *---------------------------------------------------*
               10  w-ipc-dlp-vlk.
      *                *-----------------------------------------------*
      *                * Flag di utilizzo                              *
      *                *-----------------------------------------------*
                   15  w-ipc-dlp-vlk-flg  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Tipo codice di magazzino                      *
      *                *-----------------------------------------------*
                   15  w-ipc-dlp-vlk-tpm  pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Codice numerico di magazzino                  *
      *                *-----------------------------------------------*
                   15  w-ipc-dlp-vlk-nrm  pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Codice alfanumerico di magazzino              *
      *                *-----------------------------------------------*
                   15  w-ipc-dlp-vlk-afm  pic  x(14)                  .
      *                *-----------------------------------------------*
      *                * Codice fornitore commerciale                  *
      *                *-----------------------------------------------*
                   15  w-ipc-dlp-vlk-dcf  pic  9(07)                  .

      *    *===========================================================*
      *    * Work-area per bufferizzazione testata                     *
      *    *-----------------------------------------------------------*
       01  w-tes.
      *        *-------------------------------------------------------*
      *        * Valori chiave                                         *
      *        *-------------------------------------------------------*
           05  w-tes-val-key.
               10  w-tes-cod-dcf          pic  9(07)                  .
               10  w-tes-cod-dcf-rag      pic  x(40)                  .
               10  w-tes-cod-dcf-vlt      pic  x(03)                  .
               10  w-tes-tip-mag          pic  9(02)                  .
               10  w-tes-num-pro          pic  9(07)                  .
               10  w-tes-alf-pro          pic  x(14)                  .
               10  w-tes-num-pro-des      pic  x(40)                  .
               10  w-tes-num-pro-umi      pic  x(03)                  .
               10  w-tes-num-pro-plb      pic  9(09)                  .
               10  w-tes-num-pro-deq      pic  9(01)                  .
               10  w-tes-num-pro-dep      pic  9(01)                  .
               10  w-tes-fda-pif          pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
               10  w-tes-ide-dat          pic  9(07)                  .
               10  w-tes-ide-ute          pic  x(08)                  .
               10  w-tes-ide-fas          pic  x(06)                  .
               10  w-tes-cop-sfn          pic  x(14)                  .
               10  w-tes-dep-sfn.
                   15  w-tes-dep-rig occurs 10
                                          pic  x(40)                  .
               10  w-tes-xdp-sfn          pic  9(01)                  .
               10  w-tes-snx-tum          pic  x(01)                  .
               10  w-tes-umf-tum          pic  x(03)                  .
               10  w-tes-nde-tum          pic  9(01)                  .
               10  w-tes-cmo-tum          pic  9(06)v9(03)            .
               10  w-tes-cdi-tum          pic  9(06)v9(03)            .
               10  w-tes-dpz-dcf          pic  x(04)                  .
               10  w-tes-ann-not.
                   15  w-tes-ann-rig occurs 10
                                          pic  x(40)                  .
               10  w-tes-tmp-cns          pic  9(03)                  .
               10  w-tes-sgl-vlt          pic  x(03)                  .
               10  w-tes-sgl-vlt-des      pic  x(20)                  .
               10  w-tes-dec-vlt          pic  9(01)                  .
               10  w-tes-fda-ndp          pic  x(01)                  .
               10  w-tes-dec-prz          pic  9(01)                  .
               10  w-tes-tip-pza          pic  9(02)                  .
               10  w-tes-lot-acq          pic  9(06)v9(03)            .
               10  w-tes-tap-pes          pic  9(02)                  .
               10  w-tes-uda-pes          pic  9(07)                  .
               10  w-tes-lgv-vlt          pic  x(03)                  .
               10  w-tes-lgv-vlt-des      pic  x(20)                  .
               10  w-tes-lgv-dcv          pic  9(01)                  .
               10  w-tes-lgv-tdc          pic  x(01)                  .
               10  w-tes-lgv-cdc          pic  9(06)v9(05)            .
               10  w-tes-lgv-pdt          pic  9(01)v9(02)            .
               10  w-tes-vlt-std          pic  x(03)                  .
               10  w-tes-ndp-std          pic  9(01)                  .
               10  w-tes-prz-med          pic  9(09)                  .
               10  w-tes-tmp-med          pic  9(03)                  .
               10  w-tes-tbl-pes.
                   15  w-tes-ele-pes occurs 06.
                       20  w-tes-qta-pes  pic  9(06)v9(03)            .
                       20  w-tes-prz-pes  pic  9(09)                  .
                       20  w-tes-csr-pes  pic  9(05)                  .
                       20  w-tes-psr-pes occurs 05
                                          pic  9(02)v9(01)            .
               10  w-tes-per-mpa          pic  9(02)v9(01)            .
               10  w-tes-rif-lst          pic  x(20)                  .
               10  w-tes-per-ric          pic  9(06)v9(03)            .
               10  w-tes-alx-exp.
                   15  filler  occurs 11  pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcp.
               10  w-let-arc-dcp-flg      pic  x(01)                  .
               10  w-let-arc-dcp-num      pic  9(07)                  .
               10  w-let-arc-dcp-alf      pic  x(14)                  .
               10  w-let-arc-dcp-des      pic  x(40)                  .
               10  w-let-arc-dcp-tip      pic  9(02)                  .
               10  w-let-arc-dcp-umi      pic  x(03)                  .
               10  w-let-arc-dcp-deq      pic  9(01)                  .
               10  w-let-arc-dcp-dep      pic  9(01)                  .
               10  w-let-arc-dcp-civ      pic  9(05)                  .
               10  w-let-arc-dcp-plb      pic  9(09)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dpm]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dpm.
               10  w-let-arc-dpm-flg      pic  x(01)                  .
               10  w-let-arc-dpm-num      pic  9(07)                  .
               10  w-let-arc-dpm-alf      pic  x(14)                  .
               10  w-let-arc-dpm-des      pic  x(40)                  .
               10  w-let-arc-dpm-umi      pic  x(03)                  .
               10  w-let-arc-dpm-deq      pic  9(01)                  .
               10  w-let-arc-dpm-dep      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [mtv]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-mtv.
               10  w-let-arc-mtv-flg      pic  x(01)                  .
               10  w-let-arc-mtv-num      pic  9(07)                  .
               10  w-let-arc-mtv-alf      pic  x(14)                  .
               10  w-let-arc-mtv-des      pic  x(40)                  .
               10  w-let-arc-mtv-umi      pic  x(03)                  .
               10  w-let-arc-mtv-deq      pic  9(01)                  .
               10  w-let-arc-mtv-dep      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zvl]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zvl.
               10  w-let-arc-zvl-flg      pic  x(01)                  .
               10  w-let-arc-zvl-cod      pic  x(03)                  .
               10  w-let-arc-zvl-des      pic  x(20)                  .
               10  w-let-arc-zvl-din      pic  x(20)                  .
               10  w-let-arc-zvl-dec      pic  9(01)                  .
               10  w-let-arc-zvl-tdc      pic  x(01)                  .
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
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcf]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcf.
               10  w-let-arc-dcf-flg      pic  x(01)                  .
               10  w-let-arc-dcf-fnt      pic  9(07)                  .
               10  w-let-arc-dcf-dpz      pic  x(04)                  .
               10  w-let-arc-dcf-rag      pic  x(40)                  .
               10  w-let-arc-dcf-vlt      pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [aaq]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-aaq.
               10  w-let-arc-aaq-flg      pic  x(01)                  .
               10  w-let-arc-aaq-tip      pic  9(02)                  .
               10  w-let-arc-aaq-num      pic  9(07)                  .
               10  w-let-arc-aaq-vlt      pic  x(03)                  .
               10  w-let-arc-aaq-ndv      pic  9(01)                  .
               10  w-let-arc-aaq-ndp      pic  9(01)                  .
               10  w-let-arc-aaq-prz      pic  9(09)                  .
               10  w-let-arc-aaq-tmp      pic  9(03)                  .
               10  w-let-arc-aaq-pfz      pic  9(07)                  .

      *    *===========================================================*
      *    * Work-area referenze                                       *
      *    *-----------------------------------------------------------*
       01  w-ref.
           05  filler                     pic  9(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per Det testo in file [pdx]                      *
      *        *-------------------------------------------------------*
           05  w-det-txt-pdx.
               10  w-det-txt-pdx-snx      pic  x(01)                  .
               10  w-det-txt-pdx-tip      pic  9(02)                  .
               10  w-det-txt-pdx-arc      pic  9(07)                  .
               10  w-det-txt-pdx-mag      pic  9(07)                  .
               10  w-det-txt-pdx-fda      pic  x(14)                  .
               10  w-det-txt-pdx-ctr      pic  9(05)                  .
               10  w-det-txt-pdx-txt.
                   15  w-det-txt-pdx-rig occurs 10
                                          pic  x(40)                  .

      *    *===========================================================*
      *    * Work per subroutines di Acc                               *
      *    *-----------------------------------------------------------*
       01  w-acc.
      *        *-------------------------------------------------------*
      *        * Comodo per decimali prezzo                            *
      *        *-------------------------------------------------------*
           05  w-acc-dec-prz.
      *            *---------------------------------------------------*
      *            * Comodi                                            *
      *            *---------------------------------------------------*
               10  w-acc-dec-prz-ctr      pic  9(03)                  .
               10  w-acc-dec-prz-wdd      pic s9(01)                  .
               10  w-acc-dec-prz-wpz      pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per opzione descrizione                        *
      *        *-------------------------------------------------------*
           05  w-acc-opz-des.
      *            *---------------------------------------------------*
      *            * Comodi                                            *
      *            *---------------------------------------------------*
               10  w-acc-opz-des-acc      pic  9(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Ctl                               *
      *    *-----------------------------------------------------------*
       01  w-ctl.
           05  filler                     pic  9(01)                  .

      *    *===========================================================*
      *    * Work-area per contatori e indici                          *
      *    *-----------------------------------------------------------*
       01  w-cix.
      *        *-------------------------------------------------------*
      *        * Contatore per percentuali                             *
      *        *-------------------------------------------------------*
           05  w-cix-ctr-001              pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area per routine cnt-tdo-nok-000/999                 *
      *    *-----------------------------------------------------------*
       01  w-cnt-tdo-nok.
      *        *-------------------------------------------------------*
      *        * Contatori di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-cnt-tdo-nok-c01          pic  9(03)                  .
           05  w-cnt-tdo-nok-c02          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Numero decimali per quantita' lotto di acquisto       *
      *        *-------------------------------------------------------*
           05  w-cnt-tdo-nok-wnd          pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per ridefinizione quantita' lotto di acquisto    *
      *        *-------------------------------------------------------*
           05  w-cnt-tdo-nok-wrq          pic  9(06)v9(03)            .
           05  w-cnt-tdo-nok-wrq-r redefines
               w-cnt-tdo-nok-wrq.
               10  w-cnt-tdo-nok-wni      pic  9(06)                  .
               10  w-cnt-tdo-nok-w1d      pic  9(01)                  .
               10  w-cnt-tdo-nok-w2d      pic  9(01)                  .
               10  w-cnt-tdo-nok-w3d      pic  9(01)                  .

      *    *===========================================================*
      *    * Work area per accettazione tabella prezzi e sconti        *
      *    *-----------------------------------------------------------*
       01  w-pes.
           05  w-pes-flg-vuo              pic  x(01)                  .
           05  w-pes-flg-exi              pic  x(01)                  .
           05  w-pes-ctr-vuo              pic  9(02)                  .
           05  w-pes-ctr-rig              pic  9(02)                  .
           05  w-pes-cts-rig              pic  9(02)                  .
           05  w-pes-ctx-rig              pic  9(02)                  .
           05  w-pes-cty-rig              pic  9(02)                  .
           05  w-pes-ctz-rig              pic  9(02)                  .
           05  w-pes-sav-qta              pic  9(06)v9(03)            .
           05  w-pes-imm-lin              pic  x(80) value
               "                             |           |            | 
      -        "                       |"                             .
           05  w-pes-ctr-001              pic  9(02)                  .
           05  w-pes-ctr-002              pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Tipo prodotto                                         *
      *        *-------------------------------------------------------*
           05  w-sav-tip-mag              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Numero decimali prezzo                                *
      *        *-------------------------------------------------------*
           05  w-sav-dec-prz              pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Tipo applicazione prezzi e sconti                     *
      *        *-------------------------------------------------------*
           05  w-sav-tap-pes              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Sigla valuta per legame valutario                     *
      *        *-------------------------------------------------------*
           05  w-sav-lgv-vlt              pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Tipo prezzo di acquisto                               *
      *        *-------------------------------------------------------*
           05  w-sav-tip-pza              pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per defaults di impostazione                    *
      *    *-----------------------------------------------------------*
       01  w-def.
           05  w-def-cod-dcf              pic  9(07) value zero       .
           05  w-def-tip-mag              pic  9(02) value zero       .
           05  w-def-alf-mag              pic  x(14) value spaces     .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo prodotto                              *
      *        *-------------------------------------------------------*
           05  w-exp-tip-mag.
               10  w-exp-tip-mag-num      pic  9(02)       value 03   .
               10  w-exp-tip-mag-lun      pic  9(02)       value 25   .
               10  w-exp-tip-mag-tbl.
                   15  filler             pic  x(25) value
                            "Prodotto di vendita      "               .
                   15  filler             pic  x(25) value
                            "Materia prima            "               .
                   15  filler             pic  x(25) value
                            "Materiale vario          "               .
               10  w-exp-tip-mag-tbr redefines
                   w-exp-tip-mag-tbl.
                   15  w-exp-tip-mag-ele occurs 03
                                          pic  x(25)                  .
               10  w-exp-tip-mag-ast.
                   15  filler             pic  x(06) value "010304"   .
               10  w-exp-tip-mag-ass redefines
                   w-exp-tip-mag-ast.
                   15  w-exp-tip-mag-tpm occurs 03
                                          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo magazzino limitato ai soli tipi am-   *
      *        *            messi                                      *
      *        *-------------------------------------------------------*
           05  w-exp-tpm-amm.
               10  w-exp-tpm-amm-num      pic  9(02)                  .
               10  w-exp-tpm-amm-lun      pic  9(02) value 25         .
               10  w-exp-tpm-amm-tbl.
                   15  w-exp-tpm-amm-ele occurs 03
                                          pic  x(25)                  .
               10  w-exp-tpm-amm-ass.
                   15  w-exp-tpm-amm-tpm occurs 03
                                          pic  9(02)                  .
               10  w-exp-tpm-amm-i01      pic  9(02)                  .
               10  w-exp-tpm-amm-c01      pic  9(02)                  .
               10  w-exp-tpm-amm-c02      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per : Opzione per la descrizione                 *
      *        *-------------------------------------------------------*
           05  w-exp-opz-des.
               10  w-exp-opz-des-num      pic  9(02)       value 2    .
               10  w-exp-opz-des-lun      pic  9(02)       value 45   .
               10  w-exp-opz-des-tbl.
                   15  filler             pic  x(45) value
                       "quella della scheda anagrafica Prodotto      ".
                   15  filler             pic  x(45) value
                       "Specifica per il fornitore                   ".
      *        *-------------------------------------------------------*
      *        * Work per : Tipo prezzo d'acquisto                     *
      *        *-------------------------------------------------------*
           05  w-exp-tip-pza.
               10  w-exp-tip-pza-num      pic  9(02)       value 2    .
               10  w-exp-tip-pza-lun      pic  9(02)       value 45   .
               10  w-exp-tip-pza-tbl.
                   15  filler             pic  x(45) value
                       "Specifico per l'acquisto                     ".
                   15  filler             pic  x(45) value
                       "stesso prezzo del Listino base di vendita    ".
      *        *-------------------------------------------------------*
      *        * Work per : Tipo applicazione prezzi e sconti          *
      *        *-------------------------------------------------------*
           05  w-exp-tap-pes.
               10  w-exp-tap-pes-num      pic  9(02)       value 2    .
               10  w-exp-tap-pes-lun      pic  9(02)       value 35   .
               10  w-exp-tap-pes-tbl.
                   15  filler             pic  x(35) value
                       "Indipendentemente dalla quantita'  "          .
                   15  filler             pic  x(35) value
                       "a seconda della Quantita'          "          .
      *        *-------------------------------------------------------*
      *        * Work per : Si/no trasformazione u.d.m.                *
      *        *-------------------------------------------------------*
           05  w-exp-snx-tum.
               10  w-exp-snx-tum-num      pic  9(02)       value 3    .
               10  w-exp-snx-tum-lun      pic  9(02)       value 15   .
               10  w-exp-snx-tum-tbl.
                   15  filler             pic  x(15)
                            value "No             "                   .
                   15  filler             pic  x(15)
                            value "Si             "                   .
                   15  filler             pic  x(15)
                            value "solo per Prezzo"                   .
      *        *-------------------------------------------------------*
      *        * Work per : Modifica eventuale dati generali           *
      *        *-------------------------------------------------------*
           05  w-exp-agg-aaq.
               10  w-exp-agg-aaq-num      pic  9(02)       value 2    .
               10  w-exp-agg-aaq-lun      pic  9(02)       value 40   .
               10  w-exp-agg-aaq-tbl.
                   15  filler             pic  x(40) value
                            "Aggiornamento scheda dati generali      ".
                   15  filler             pic  x(40) value
                            "Nessuna azione                          ".
               10  w-exp-agg-aaq-sce      pic  9(02)                  .

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
      *    * Work per subroutines di editing                           *
      *    *-----------------------------------------------------------*
       01  w-edt.
      *        *-------------------------------------------------------*
      *        * Area editata per 5 percentuali di sconto              *
      *        *-------------------------------------------------------*
           05  w-edt-psr-pes.
               10  w-edt-psr-pes-edt.
                   15  w-edt-psr-pes-ele occurs 05.
                       20  w-edt-psr-pes-per
                                          pic  x(04)                  .
                       20  filler         pic  x(02)                  .
               10  w-edt-psr-pes-c01      pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di compattamento                     *
      *    *-----------------------------------------------------------*
       01  w-cmp.
      *        *-------------------------------------------------------*
      *        * Work per compattamento righe di note                  *
      *        *-------------------------------------------------------*
           05  w-cmp-rig-txt.
               10  w-cmp-rig-txt-flg      pic  x(01)                  .
               10  w-cmp-rig-txt-nri      pic  9(02)                  .
               10  w-cmp-rig-txt-nrd      pic  9(02)                  .
               10  w-cmp-rig-txt-ctr      pic  9(02)                  .
               10  w-cmp-rig-txt-max      pic  9(02)                  .
               10  w-cmp-rig-txt-txt.
                   15  w-cmp-rig-txt-rig occurs 10
                                          pic  x(40)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice prodotto d'acquisto     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acodaaq0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice commerciale fornitore   *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmndcf0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice valuta                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acodzvl0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione formato di acquisizione        *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acodaaf0.acl"                   .

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
           if        w-cnt-pre-snx-del    not  = spaces
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
      *              * Subroutine per la preparazione dei valori rela- *
      *              * tivi alla valuta base, determinati dalla segre- *
      *              * teria                                           *
      *              *-------------------------------------------------*
           move      "VB"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dec                to   c-dec                  .
           move      s-asx                to   c-sgl                  .
           move      s-sgn                to   c-tdc                  .
           move      s-num                to   c-cdc                  .
           move      s-adx (01 : 20)      to   c-des                  .
           move      s-adx (21 : 20)      to   c-din                  .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Si/No gestione materie prime attiva         *
      *                  *---------------------------------------------*
           perform   prs-dpm-snx-000      thru prs-dpm-snx-999        .
      *                  *---------------------------------------------*
      *                  * Si/No gestione materiali vari attiva        *
      *                  *---------------------------------------------*
           perform   prs-mtv-snx-000      thru prs-mtv-snx-999        .
      *                  *---------------------------------------------*
      *                  * Modalita' di espressione tempo di consegna  *
      *                  *---------------------------------------------*
           perform   prs-mde-tdc-000      thru prs-mde-tdc-999        .
      *                  *---------------------------------------------*
      *                  * Si/No gestione formato di acquisto          *
      *                  *---------------------------------------------*
           perform   prs-snx-fda-000      thru prs-snx-fda-999        .
      *                  *---------------------------------------------*
      *                  * Tipo controllo su codice fornitore prefe-   *
      *                  * renziale prodotti                           *
      *                  *---------------------------------------------*
           perform   prs-fnt-pfz-000      thru prs-fnt-pfz-999        .
      *                  *---------------------------------------------*
      *                  * Si/no accettazione riferimento al listino   *
      *                  *---------------------------------------------*
           perform   prs-rif-lst-000      thru prs-rif-lst-999        .
      *                  *---------------------------------------------*
      *                  * Default di inserimento                      *
      *                  *---------------------------------------------*
           perform   prs-def-ins-000      thru prs-def-ins-999        .
      *              *-------------------------------------------------*
      *              * Determinazione tipi magazzino ammessi           *
      *              *-------------------------------------------------*
           perform   det-tpm-amm-000      thru det-tpm-amm-999        .
      *              *-------------------------------------------------*
      *              * Lettura i.p.c. da livello precedente per valori *
      *              * chiave                                          *
      *              *-------------------------------------------------*
           perform   ipc-dlp-vlk-000      thru ipc-dlp-vlk-999        .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice prodotto 'aaq'  *
      *              *-------------------------------------------------*
           perform   cod-cod-aaq-opn-000  thru cod-cod-aaq-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice fornitore com-  *
      *              * merciale                                        *
      *              *-------------------------------------------------*
           perform   cod-mne-dcf-opn-000  thru cod-mne-dcf-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice valuta          *
      *              *-------------------------------------------------*
           perform   cod-cod-zvl-opn-000  thru cod-cod-zvl-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione formato di acquisizio- *
      *              * ne                                              *
      *              *-------------------------------------------------*
           perform   cod-cod-aaf-opn-000  thru cod-cod-aaf-opn-999    .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice prodotto 'aaq' *
      *              *-------------------------------------------------*
           perform   cod-cod-aaq-cls-000  thru cod-cod-aaq-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice fornitore com- *
      *              * merciale                                        *
      *              *-------------------------------------------------*
           perform   cod-mne-dcf-cls-000  thru cod-mne-dcf-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice valuta         *
      *              *-------------------------------------------------*
           perform   cod-cod-zvl-cls-000  thru cod-cod-zvl-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione formato di acquisizio-*
      *              * ne                                              *
      *              *-------------------------------------------------*
           perform   cod-cod-aaf-cls-000  thru cod-cod-aaf-cls-999    .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/No gestione materie prime  *
      *    *                             attiva                        *
      *    *-----------------------------------------------------------*
       prs-dpm-snx-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dpm[snx]"       to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-dpm-snx
           else      move  spaces         to   w-prs-dpm-snx          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-dpm-snx        =    "S" or
                     w-prs-dpm-snx        =    "N"
                     go to prs-dpm-snx-999.
           move      "N"                  to   w-prs-dpm-snx          .
       prs-dpm-snx-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/No gestione materiali vari *
      *    *                             attiva                        *
      *    *-----------------------------------------------------------*
       prs-mtv-snx-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/mtv[snx]"       to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-mtv-snx
           else      move  "N"            to   w-prs-mtv-snx          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-mtv-snx        not  = "S" and
                     w-prs-mtv-snx        not  = "N"
                     move  "N"            to   w-prs-mtv-snx          .
       prs-mtv-snx-999.
           exit.

      *    *===========================================================*
      *    * Lettura della personalizzazione relativa alla modalita'   *
      *    * di espressione del tempo di consegna per l'acquisto       *
      *    *-----------------------------------------------------------*
       prs-mde-tdc-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dcf[mde-tdc]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-prs-mde-tdc
           else      move  0              to   w-prs-mde-tdc          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-mde-tdc        not  = 1
                     move  0              to   w-prs-mde-tdc          .
       prs-mde-tdc-999.
           exit.

      *    *===========================================================*
      *    * Lettura della personalizzazione relativa alla gestione    *
      *    * del formato di acquisto                                   *
      *    *-----------------------------------------------------------*
       prs-snx-fda-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dcf/aaf[snx-fda]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-snx-fda
           else      move  spaces         to   w-prs-snx-fda          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-snx-fda        not  = "N"
                     move  "S"            to   w-prs-snx-fda          .
       prs-snx-fda-999.
           exit.

      *    *===========================================================*
      *    * Lettura della personalizzazione relativa al tipo control- *
      *    * lo su codice fornitore preferenziale                      *
      *    *-----------------------------------------------------------*
       prs-fnt-pfz-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/orf/mov/orf300[fnt-pfz]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-fnt-pfz
           else      move  spaces         to   w-prs-fnt-pfz          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-fnt-pfz        not  = "M"
                     move  spaces         to   w-prs-fnt-pfz          .
       prs-fnt-pfz-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : si/no accettazione riferimen- *
      *    *                             to al listino fornitore       *
      *    *-----------------------------------------------------------*
       prs-rif-lst-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/orf/mov/orf300[rif-lst]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-rif-lst
           else      move  spaces         to   w-prs-rif-lst          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-rif-lst        not  = "S" and
                     w-prs-rif-lst        not  = "X"
                     move  "N"            to   w-prs-rif-lst          .
       prs-rif-lst-999.
           exit.

      *    *===========================================================*
      *    * Lettura della personalizzazione relativa ai default di    *
      *    * inserimento                                               *
      *    *-----------------------------------------------------------*
       prs-def-ins-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dcf/lst/dcf500[def-ins]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-def-ins
           else      move  spaces         to   w-prs-def-ins          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-def-ins-fmb    not  = 01
                     move  00             to   w-prs-def-ins-fmb      .
           if        w-prs-def-ins-dad    not  = 01
                     move  00             to   w-prs-def-ins-dad      .
       prs-def-ins-999.
           exit.

      *    *===========================================================*
      *    * Lettura i.p.c. da livello precedente per valori chiave    *
      *    *-----------------------------------------------------------*
       ipc-dlp-vlk-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione iniziale                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-ipc-dlp-vlk-flg      .
      *              *-------------------------------------------------*
      *              * Estrazione della eventuale variabile di i.p.c.  *
      *              * "tip-mag" per tipo magazzino passato            *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "tip-mag"            to   s-var                  .
           move      "-"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     go to ipc-dlp-vlk-900.
           move      s-num                to   w-ipc-dlp-vlk-tpm      .
      *              *-------------------------------------------------*
      *              * Estrazione della eventuale variabile di i.p.c.  *
      *              * "num-mag" per codice magazzino numerico         *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "num-mag"            to   s-var                  .
           move      "-"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     go to ipc-dlp-vlk-900.
           move      s-num                to   w-ipc-dlp-vlk-nrm      .
      *              *-------------------------------------------------*
      *              * Estrazione della eventuale variabile di i.p.c.  *
      *              * "alf-mag" per codice magazzino alfanumerico     *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "alf-mag"            to   s-var                  .
           move      "-"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     go to ipc-dlp-vlk-900.
           move      s-alf                to   w-ipc-dlp-vlk-afm      .
      *              *-------------------------------------------------*
      *              * Estrazione della eventuale variabile di i.p.c.  *
      *              * "cod-dcf" per codice fornitore passato          *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cod-dcf"            to   s-var                  .
           move      "-"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     go to ipc-dlp-vlk-900.
           move      s-num                to   w-ipc-dlp-vlk-dcf      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ipc-dlp-vlk-999.
       ipc-dlp-vlk-900.
      *              *-------------------------------------------------*
      *              * Se errori nella lettura : si attiva il flag di  *
      *              * utilizzo simulando come se le variabili fossero *
      *              * gia' state utilizzate                           *
      *              *-------------------------------------------------*
           move      "#"                  to   w-ipc-dlp-vlk-flg      .
       ipc-dlp-vlk-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * [aaf]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *              *-------------------------------------------------*
      *              * [aaq]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *              *-------------------------------------------------*
      *              * [dcp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * [pdx]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *              *-------------------------------------------------*
      *              * [dpm]                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione materie prime attiva       *
      *                  *---------------------------------------------*
           if        w-prs-dpm-snx        not  = "S"
                     go to rou-opn-fls-020.
      *                  *---------------------------------------------*
      *                  * Apertura file                               *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
       rou-opn-fls-020.
      *              *-------------------------------------------------*
      *              * [mtv]                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione materiali vari attiva      *
      *                  *---------------------------------------------*
           if        w-prs-mtv-snx        not  = "S"
                     go to rou-opn-fls-040.
      *                  *---------------------------------------------*
      *                  * Apertura file                               *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofmtv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
       rou-opn-fls-040.
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
      *              *-------------------------------------------------*
      *              * [zvl]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzvl"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvl                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [aaf]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *              *-------------------------------------------------*
      *              * [aaq]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *              *-------------------------------------------------*
      *              * [dcp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * [pdx]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *              *-------------------------------------------------*
      *              * [dpm]                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione materie prime attiva       *
      *                  *---------------------------------------------*
           if        w-prs-dpm-snx        not  = "S"
                     go to rou-cls-fls-020.
      *                  *---------------------------------------------*
      *                  * Chiusura file                               *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
       rou-cls-fls-020.
      *              *-------------------------------------------------*
      *              * [mtv]                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione materiali vari attiva      *
      *                  *---------------------------------------------*
           if        w-prs-mtv-snx        not  = "S"
                     go to rou-cls-fls-040.
      *                  *---------------------------------------------*
      *                  * Chiusura file                               *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofmtv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
       rou-cls-fls-040.
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
      *              *-------------------------------------------------*
      *              * [zvl]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzvl"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvl                 .
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
           perform   acc-cod-dcf-000      thru acc-cod-dcf-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
       acc-key-reg-200.
      *                  *---------------------------------------------*
      *                  * Tipo prodotto                               *
      *                  *---------------------------------------------*
           perform   acc-tip-mag-000      thru acc-tip-mag-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
           if        v-key                =    "UP  "
                     go to acc-key-reg-100.
       acc-key-reg-300.
      *                  *---------------------------------------------*
      *                  * Codice prodotto                             *
      *                  *---------------------------------------------*
           perform   acc-num-pro-000      thru acc-num-pro-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
           if        v-key                =    "UP  "
                     go to acc-key-reg-200.
       acc-key-reg-400.
      *                  *---------------------------------------------*
      *                  * Formato                                     *
      *                  *---------------------------------------------*
           perform   acc-fda-pif-000      thru acc-fda-pif-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
           if        v-key                =    "UP  "
                     go to acc-key-reg-300.
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
           perform   vis-cod-dcf-000      thru vis-cod-dcf-999        .
           perform   vis-cod-dcf-rag-000  thru vis-cod-dcf-rag-999    .
      *              *-------------------------------------------------*
      *              * Tipo prodotto                                   *
      *              *-------------------------------------------------*
           perform   vis-tip-mag-000      thru vis-tip-mag-999        .
      *              *-------------------------------------------------*
      *              * Codice prodotto                                 *
      *              *-------------------------------------------------*
           perform   vis-num-pro-000      thru vis-num-pro-999        .
           perform   vis-num-pro-des-000  thru vis-num-pro-des-999    .
           perform   vis-num-pro-umi-000  thru vis-num-pro-umi-999    .
      *              *-------------------------------------------------*
      *              * Formato                                         *
      *              *-------------------------------------------------*
           perform   vis-fda-pif-000      thru vis-fda-pif-999        .
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
           move      08                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Codice fornitore                                *
      *              *-------------------------------------------------*
           perform   pmt-cod-dcf-000      thru pmt-cod-dcf-999        .
      *              *-------------------------------------------------*
      *              * Tipo prodotto                                   *
      *              *-------------------------------------------------*
           perform   pmt-tip-mag-000      thru pmt-tip-mag-999        .
      *              *-------------------------------------------------*
      *              * Codice prodotto                                 *
      *              *-------------------------------------------------*
           perform   pmt-num-pro-000      thru pmt-num-pro-999        .
           perform   pmt-num-pro-umi-000  thru pmt-num-pro-umi-999    .
      *              *-------------------------------------------------*
      *              * Formato                                         *
      *              *-------------------------------------------------*
           perform   pmt-fda-pif-000      thru pmt-fda-pif-999        .
      *              *-------------------------------------------------*
      *              * Linea di trattini                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Codice fornitore              *
      *    *-----------------------------------------------------------*
       pmt-cod-dcf-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice fornitore       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-dcf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Tipo prodotto                 *
      *    *-----------------------------------------------------------*
       pmt-tip-mag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo   prodotto        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-tip-mag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Unita' di misura prodotto     *
      *    *-----------------------------------------------------------*
       pmt-num-pro-umi-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      72                   to   v-pos                  .
           move      "Udm :"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-num-pro-umi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Codice prodotto               *
      *    *-----------------------------------------------------------*
       pmt-num-pro-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
      *
           if        w-tes-tip-mag        =    01
                     move  "Codice prodotto        :"
                                          to   v-alf
           else if   w-tes-tip-mag        =    03
                     move  "Codice materia prima   :"
                                          to   v-alf
           else if   w-tes-tip-mag        =    04
                     move  "Codice materiale vario :"
                                          to   v-alf
           else      move  "Codice prodotto        :"
                                          to   v-alf                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-num-pro-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Formato                       *
      *    *-----------------------------------------------------------*
       pmt-fda-pif-000.
      *              *-------------------------------------------------*
      *              * Test se prompt da visualizzare                  *
      *              *-------------------------------------------------*
           if        w-prs-snx-fda        not  = "S"
                     go to pmt-fda-pif-999.
       pmt-fda-pif-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Formato                :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-fda-pif-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice fornitore                     *
      *    *-----------------------------------------------------------*
       acc-cod-dcf-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se passata variabile di ipc e se pri-  *
      *                  * mo utilizzo di essa                         *
      *                  *---------------------------------------------*
           if        w-ipc-dlp-vlk-flg    not  = spaces
                     go to acc-cod-dcf-020.
           move      w-ipc-dlp-vlk-dcf    to   w-tes-cod-dcf          .
           perform   vis-cod-dcf-000      thru vis-cod-dcf-999        .
           go to     acc-cod-dcf-400.
       acc-cod-dcf-020.
      *                  *---------------------------------------------*
      *                  * Preparazione valore di default              *
      *                  *---------------------------------------------*
           if        w-def-cod-dcf        =    zero
                     go to acc-cod-dcf-100.
           if        w-tes-cod-dcf        not  = zero
                     go to acc-cod-dcf-100.
           move      w-def-cod-dcf        to   w-tes-cod-dcf          .
      *                  *---------------------------------------------*
      *                  * Lettura record anagrafica contabile del     *
      *                  * fornitore                                   *
      *                  *---------------------------------------------*
           move      w-tes-cod-dcf        to   w-let-arc-fnt-cod      .
           perform   let-arc-fnt-000      thru let-arc-fnt-999        .
      *                  *---------------------------------------------*
      *                  * Lettura record anagrafica commerciale del   *
      *                  * fornitore                                   *
      *                  *---------------------------------------------*
           move      w-tes-cod-dcf        to   w-let-arc-dcf-fnt      .
           move      spaces               to   w-let-arc-dcf-dpz      .
           perform   let-arc-dcf-000      thru let-arc-dcf-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione ragione sociale del forni-   *
      *                  * tore                                        *
      *                  *---------------------------------------------*
           if        w-let-arc-dcf-flg    =    spaces
                     move  w-let-arc-dcf-rag
                                          to   w-tes-cod-dcf-rag
           else      move  w-let-arc-fnt-rag
                                          to   w-tes-cod-dcf-rag      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione ragione sociale             *
      *                  *---------------------------------------------*
           perform   vis-cod-dcf-rag-000  thru vis-cod-dcf-rag-999    .
       acc-cod-dcf-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-dcf-ope      .
           move      w-tes-cod-dcf        to   w-cod-mne-dcf-cod      .
           move      04                   to   w-cod-mne-dcf-lin      .
           move      26                   to   w-cod-mne-dcf-pos      .
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
       acc-cod-dcf-110.
           perform   cod-mne-dcf-cll-000  thru cod-mne-dcf-cll-999    .
           if        w-cod-mne-dcf-ope    =    "F+"
                     go to acc-cod-dcf-115.
           if        w-cod-mne-dcf-ope    =    "AC"
                     go to acc-cod-dcf-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-dcf-115.
           perform   cod-mne-dcf-foi-000  thru cod-mne-dcf-foi-999    .
           go to     acc-cod-dcf-110.
       acc-cod-dcf-120.
           move      w-cod-mne-dcf-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-dcf-999.
       acc-cod-dcf-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-dcf          .
      *              *-------------------------------------------------*
      *              * Preparazione default per accettazione successi- *
      *              * va                                              *
      *              *-------------------------------------------------*
           move      w-tes-cod-dcf        to   w-def-cod-dcf          .
       acc-cod-dcf-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cod-dcf-410.
      *                  *---------------------------------------------*
      *                  * Lettura record anagrafica contabile del     *
      *                  * fornitore                                   *
      *                  *---------------------------------------------*
           move      w-tes-cod-dcf        to   w-let-arc-fnt-cod      .
           perform   let-arc-fnt-000      thru let-arc-fnt-999        .
      *                  *---------------------------------------------*
      *                  * Lettura record anagrafica commerciale del   *
      *                  * fornitore                                   *
      *                  *---------------------------------------------*
           move      w-tes-cod-dcf        to   w-let-arc-dcf-fnt      .
           move      spaces               to   w-let-arc-dcf-dpz      .
           perform   let-arc-dcf-000      thru let-arc-dcf-999        .
       acc-cod-dcf-420.
      *                  *---------------------------------------------*
      *                  * Memorizzazione ragione sociale del forni-   *
      *                  * tore                                        *
      *                  *---------------------------------------------*
           if        w-let-arc-dcf-flg    =    spaces
                     move  w-let-arc-dcf-rag
                                          to   w-tes-cod-dcf-rag
           else      move  w-let-arc-fnt-rag
                                          to   w-tes-cod-dcf-rag      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione ragione sociale             *
      *                  *---------------------------------------------*
           perform   vis-cod-dcf-rag-000  thru vis-cod-dcf-rag-999    .
       acc-cod-dcf-430.
      *                  *---------------------------------------------*
      *                  * Memorizzazione codice valuta da anagrafica  *
      *                  * commerciale del fornitore                   *
      *                  *---------------------------------------------*
           move      w-let-arc-dcf-vlt    to   w-tes-cod-dcf-vlt      .
       acc-cod-dcf-440.
      *                  *---------------------------------------------*
      *                  * Se mancano sia l'anagrafica contabile che   *
      *                  * quella commerciale : reimpostazione         *
      *                  *---------------------------------------------*
           if        w-let-arc-fnt-flg    not  = spaces and
                     w-let-arc-dcf-flg    not  = spaces
                     go to acc-cod-dcf-100.
      *                  *---------------------------------------------*
      *                  * Se manca l'anagrafica commerciale : messag- *
      *                  * gio e reimpostazione                        *
      *                  *---------------------------------------------*
           if        w-let-arc-dcf-flg    =    spaces
                     go to acc-cod-dcf-450.
           move      "Mancano i dati commerciali del fornitore !"
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     acc-cod-dcf-100.
       acc-cod-dcf-450.
      *                  *---------------------------------------------*
      *                  * Se codice a zero : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-tes-cod-dcf        not  = zero
                     go to acc-cod-dcf-600.
           go to     acc-cod-dcf-100.
       acc-cod-dcf-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-dcf-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-dcf-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-dcf-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-dcf-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-dcf-999.
       acc-cod-dcf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice fornitore                  *
      *    *-----------------------------------------------------------*
       vis-cod-dcf-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      04                   to   v-lin                  .
           move      26                   to   v-pos                  .
           move      w-tes-cod-dcf        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-dcf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Ragione sociale fornitore         *
      *    *-----------------------------------------------------------*
       vis-cod-dcf-rag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-dcf-rag    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-dcf-rag-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo prodotto                        *
      *    *-----------------------------------------------------------*
       acc-tip-mag-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se passata variabile di ipc e se pri-  *
      *                  * mo utilizzo di essa                         *
      *                  *---------------------------------------------*
           if        w-ipc-dlp-vlk-flg    not  = spaces
                     go to acc-tip-mag-020.
           move      zero                 to   w-exp-tpm-amm-c01      .
       acc-tip-mag-010.
           add       1                    to   w-exp-tpm-amm-c01      .
           if        w-exp-tpm-amm-c01    >    w-exp-tpm-amm-num
                     move  "#"            to   w-ipc-dlp-vlk-flg
                     go to acc-tip-mag-020.
           if        w-ipc-dlp-vlk-tpm    not  = w-exp-tpm-amm-tpm
                                                (w-exp-tpm-amm-c01)
                     go to acc-tip-mag-010.
           move      w-ipc-dlp-vlk-tpm    to   w-tes-tip-mag          .
           perform   vis-tip-mag-000      thru vis-tip-mag-999        .
           go to     acc-tip-mag-400.
       acc-tip-mag-020.
      *                  *---------------------------------------------*
      *                  * Se la tabella dei tipi magazzino ammessi e' *
      *                  * formata da un solo elemento : si forza il   *
      *                  * valore corrispondente, si visualizza, e si  *
      *                  * esce                                        *
      *                  *---------------------------------------------*
           if        w-exp-tpm-amm-num    not  = 1
                     go to acc-tip-mag-050.
           move      w-exp-tip-mag-tpm (1)
                                          to   w-tes-tip-mag          .
           perform   vis-tip-mag-000      thru vis-tip-mag-999        .
           go to     acc-tip-mag-999.
       acc-tip-mag-050.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-tip-mag        to   w-sav-tip-mag          .
      *                  *---------------------------------------------*
      *                  * Preparazione valore di default              *
      *                  *---------------------------------------------*
           move      w-def-tip-mag        to   w-tes-tip-mag          .
       acc-tip-mag-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tpm-amm-lun    to   v-car                  .
           move      w-exp-tpm-amm-num    to   v-ldt                  .
           move      spaces               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tpm-amm-tbl    to   v-txt                  .
           move      05                   to   v-lin                  .
           move      26                   to   v-pos                  .
           move      zero                 to   w-exp-tpm-amm-c01      .
       acc-tip-mag-102.
           add       1                    to   w-exp-tpm-amm-c01      .
           if        w-exp-tpm-amm-c01    >    w-exp-tpm-amm-num
                     move  zero           to   v-num
                     go to acc-tip-mag-104.
           if        w-tes-tip-mag        =    w-exp-tpm-amm-tpm
                                              (w-exp-tpm-amm-c01)
                     move  w-exp-tpm-amm-c01
                                          to   v-num
                     go to acc-tip-mag-104.
           go to     acc-tip-mag-102.
       acc-tip-mag-104.
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-tip-mag-999.
       acc-tip-mag-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-exp-tpm-amm-i01      .
           if        w-exp-tpm-amm-i01    =    zero
                     move  zero           to   w-tes-tip-mag
           else      move  w-exp-tpm-amm-tpm
                          (w-exp-tpm-amm-i01)
                                          to   w-tes-tip-mag          .
       acc-tip-mag-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che il valore sia accettabile          *
      *                  *---------------------------------------------*
           if        w-tes-tip-mag        =    zero
                     go to acc-tip-mag-100.
       acc-tip-mag-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt codice prodotto                      *
      *                  *---------------------------------------------*
           perform   pmt-num-pro-000      thru pmt-num-pro-999        .
      *                  *---------------------------------------------*
      *                  * Aggiornamento valore di default             *
      *                  *---------------------------------------------*
           move      w-tes-tip-mag        to   w-def-tip-mag          .
      *                  *---------------------------------------------*
      *                  * Normalizzazione codice, descrizione e       *
      *                  * unita' di misura del prodotto               *
      *                  *---------------------------------------------*
           if        w-tes-num-pro        =    zero
                     go to acc-tip-mag-800.
           move      zero                 to   w-tes-num-pro          .
           move      spaces               to   w-tes-alf-pro          .
           move      spaces               to   w-tes-num-pro-des      .
           move      spaces               to   w-tes-num-pro-umi      .
           move      zero                 to   w-tes-num-pro-plb      .
           move      zero                 to   w-tes-num-pro-deq      .
           move      zero                 to   w-tes-num-pro-dep      .
           perform   vis-num-pro-000      thru vis-num-pro-999        .
           perform   vis-num-pro-des-000  thru vis-num-pro-des-999    .
           perform   vis-num-pro-umi-000  thru vis-num-pro-umi-999    .
       acc-tip-mag-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-tip-mag-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-tip-mag-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-tip-mag-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-tip-mag-999.
       acc-tip-mag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Tipo prodotto              *
      *    *-----------------------------------------------------------*
       vis-tip-mag-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-mag-lun    to   v-car                  .
           move      w-exp-tip-mag-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-mag-tbl    to   v-txt                  .
           move      05                   to   v-lin                  .
           move      26                   to   v-pos                  .
           if        w-tes-tip-mag        =    01
                     move  01             to   v-num
           else if   w-tes-tip-mag        =    03
                     move  02             to   v-num
           else if   w-tes-tip-mag        =    04
                     move  03             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-mag-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice prodotto                      *
      *    *-----------------------------------------------------------*
       acc-num-pro-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-num-pro-100.
      *              *-------------------------------------------------*
      *              * Accettazione                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo di prodotto *
      *                  *---------------------------------------------*
           if        w-tes-tip-mag        =    01
                     go to acc-num-pro-120
           else if   w-tes-tip-mag        =    03
                     go to acc-num-pro-140
           else if   w-tes-tip-mag        =    04
                     go to acc-num-pro-160
           else      go to acc-num-pro-999.
       acc-num-pro-120.
      *                  *---------------------------------------------*
      *                  * Se Prodotto di vendita                      *
      *                  *---------------------------------------------*
           perform   acc-cod-dcp-000      thru acc-cod-dcp-999        .
           go to     acc-num-pro-999.
       acc-num-pro-140.
      *                  *---------------------------------------------*
      *                  * Se Materia prima                            *
      *                  *---------------------------------------------*
           perform   acc-cod-dpm-000      thru acc-cod-dpm-999        .
           go to     acc-num-pro-999.
       acc-num-pro-160.
      *                  *---------------------------------------------*
      *                  * Se Materiale vario                          *
      *                  *---------------------------------------------*
           perform   acc-cod-mtv-000      thru acc-cod-mtv-999        .
           go to     acc-num-pro-999.
       acc-num-pro-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Codice prodotto            *
      *    *-----------------------------------------------------------*
       vis-num-pro-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      26                   to   v-pos                  .
           move      w-tes-alf-pro        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-pro-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione prodotto      *
      *    *-----------------------------------------------------------*
       vis-num-pro-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-num-pro-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-pro-des-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Unita' di misura prodotto *
      *    *-----------------------------------------------------------*
       vis-num-pro-umi-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      w-tes-num-pro-umi    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-pro-umi-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice prodotto di vendita           *
      *    *-----------------------------------------------------------*
       acc-cod-dcp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se passata variabile di ipc e se pri-  *
      *                  * mo utilizzo di essa                         *
      *                  *---------------------------------------------*
           if        w-ipc-dlp-vlk-flg    not  = spaces
                     go to acc-cod-dcp-050.
           move      w-ipc-dlp-vlk-nrm    to   w-tes-num-pro          .
           move      w-ipc-dlp-vlk-afm    to   w-tes-alf-pro          .
           perform   vis-num-pro-000      thru vis-num-pro-999        .
           go to     acc-cod-dcp-400.
       acc-cod-dcp-050.
      *                  *---------------------------------------------*
      *                  * Preparazione valore di default              *
      *                  *---------------------------------------------*
           move      w-def-alf-mag        to   w-tes-alf-pro          .
       acc-cod-dcp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-aaq-ope      .
           move      "A"                  to   w-cod-cod-aaq-tac      .
           move      w-tes-tip-mag        to   w-cod-cod-aaq-tco      .
           move      w-tes-num-pro        to   w-cod-cod-aaq-num      .
           move      w-tes-alf-pro        to   w-cod-cod-aaq-alf      .
           move      06                   to   w-cod-cod-aaq-lin      .
           move      26                   to   w-cod-cod-aaq-pos      .
           move      06                   to   w-cod-cod-aaq-dln      .
           move      41                   to   w-cod-cod-aaq-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-cod-aaq-cll-000  thru cod-cod-aaq-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-aaq-foi-000  thru cod-cod-aaq-foi-999    .
       acc-cod-dcp-110.
           perform   cod-cod-aaq-cll-000  thru cod-cod-aaq-cll-999    .
           if        w-cod-cod-aaq-ope    =    "F+"
                     go to acc-cod-dcp-115.
           if        w-cod-cod-aaq-ope    =    "AC"
                     go to acc-cod-dcp-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-dcp-115.
           perform   cod-cod-aaq-foi-000  thru cod-cod-aaq-foi-999    .
           go to     acc-cod-dcp-110.
       acc-cod-dcp-120.
           move      w-cod-cod-aaq-num    to   v-num                  .
           move      w-cod-cod-aaq-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-dcp-999.
       acc-cod-dcp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-num-pro          .
           move      v-alf                to   w-tes-alf-pro          .
       acc-cod-dcp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Aggiornamento valore di default             *
      *                  *---------------------------------------------*
           move      w-tes-alf-pro        to   w-def-alf-mag          .
      *                  *---------------------------------------------*
      *                  * Lettura archivio [dcp]                      *
      *                  *---------------------------------------------*
           move      w-tes-num-pro        to   w-let-arc-dcp-num      .
           perform   let-arc-dcp-000      thru let-arc-dcp-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione valori letti                 *
      *                  *---------------------------------------------*
           move      w-let-arc-dcp-des    to   w-tes-num-pro-des      .
           move      w-let-arc-dcp-umi    to   w-tes-num-pro-umi      .
           move      w-let-arc-dcp-plb    to   w-tes-num-pro-plb      .
           move      w-let-arc-dcp-deq    to   w-tes-num-pro-deq      .
           move      w-let-arc-dcp-dep    to   w-tes-num-pro-dep      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-num-pro-des-000  thru vis-num-pro-des-999    .
      *                  *---------------------------------------------*
      *                  * Visualizzazione unita' di misura            *
      *                  *---------------------------------------------*
           perform   vis-num-pro-umi-000  thru vis-num-pro-umi-999    .
      *                  *---------------------------------------------*
      *                  * Se valore non trovato : reimpostazione      *
      *                  *---------------------------------------------*
           if        w-let-arc-dcp-flg    not  = spaces
                     go to acc-cod-dcp-100.
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione a meno  *
      *                  * che non sia stato premuto 'Up'              *
      *                  *---------------------------------------------*
           if        w-tes-alf-pro        not  = spaces
                     go to acc-cod-dcp-500.
           if        v-key                =    "UP  "
                     go to acc-cod-dcp-600
           else      go to acc-cod-dcp-100.
       acc-cod-dcp-500.
      *                  *---------------------------------------------*
      *                  * Lettura file [aaq] per controllo            *
      *                  *---------------------------------------------*
           move      w-tes-tip-mag        to   w-let-arc-aaq-tip      .
           move      w-tes-num-pro        to   w-let-arc-aaq-num      .
           perform   let-arc-aaq-000      thru let-arc-aaq-999        .
           if        w-let-arc-aaq-flg    =    spaces
                     go to acc-cod-dcp-550.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore e reimpostazione    *
      *                      *-----------------------------------------*
           move      "Mancano i dati generali di acquisto !"
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     acc-cod-dcp-100.
       acc-cod-dcp-550.
      *                  *---------------------------------------------*
      *                  * Preparazione valori medi letti da scheda di *
      *                  * acquisto 'aaq'                              *
      *                  *---------------------------------------------*
           move      w-let-arc-aaq-vlt    to   w-tes-vlt-std (1)      .
           move      w-let-arc-aaq-ndp    to   w-tes-ndp-std (1)      .
           move      w-let-arc-aaq-prz    to   w-tes-prz-med (1)      .
           move      w-let-arc-aaq-tmp    to   w-tes-tmp-med (1)      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione sigla valuta standard per   *
      *                  * il prodotto                                 *
      *                  *---------------------------------------------*
           perform   vis-vlt-std-000      thru vis-vlt-std-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione numero decimali standard    *
      *                  * per il prodotto                             *
      *                  *---------------------------------------------*
           perform   vis-ndp-std-000      thru vis-ndp-std-999        .
       acc-cod-dcp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Attivazione flag di utilizzo variabili di   *
      *                  * ipc                                         *
      *                  *---------------------------------------------*
           move      "#"                  to   w-ipc-dlp-vlk-flg      .
       acc-cod-dcp-650.
      *                  *---------------------------------------------*
      *                  * Eventuale controllo su codice fornitore     *
      *                  * preferenziale                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su personalizzazione               *
      *                      *-----------------------------------------*
           if        w-prs-fnt-pfz        =    spaces
                     go to acc-cod-dcp-800.
      *                      *-----------------------------------------*
      *                      * Test di confronto                       *
      *                      *-----------------------------------------*
           if        w-let-arc-aaq-pfz    =    zero
                     go to acc-cod-dcp-800.
           if        w-tes-cod-dcf        =    w-let-arc-aaq-pfz
                     go to acc-cod-dcp-800.
      *                      *-----------------------------------------*
      *                      * Messaggio di avviso                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Editing codice fornitore preferen-  *
      *                          * ziale                               *
      *                          *-------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-let-arc-aaq-pfz    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Lettura anagrafica fornitore prefe- *
      *                          * renziale                            *
      *                          *-------------------------------------*
           move      w-let-arc-aaq-pfz    to   w-let-arc-fnt-cod      .
           perform   let-arc-fnt-000      thru let-arc-fnt-999        .
      *                          *-------------------------------------*
      *                          * Composizione messaggio              *
      *                          *-------------------------------------*
           move      65                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      "Fornitore pref. :"  to   w-all-str-cat (1)      .
           move      v-edt                to   w-all-str-cat (2)      .
           move      "-"                  to   w-all-str-cat (3)      .
           move      w-let-arc-fnt-rag    to   w-all-str-cat (4)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           move      w-all-str-alf        to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       acc-cod-dcp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-dcp-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-dcp-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-dcp-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-dcp-999.
       acc-cod-dcp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice materia prima                 *
      *    *-----------------------------------------------------------*
       acc-cod-dpm-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se passata variabile di ipc e se pri-  *
      *                  * mo utilizzo di essa                         *
      *                  *---------------------------------------------*
           if        w-ipc-dlp-vlk-flg    not  = spaces
                     go to acc-cod-dpm-100.
           move      w-ipc-dlp-vlk-nrm    to   w-tes-num-pro          .
           move      w-ipc-dlp-vlk-afm    to   w-tes-alf-pro          .
           perform   vis-num-pro-000      thru vis-num-pro-999        .
           go to     acc-cod-dpm-400.
       acc-cod-dpm-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-aaq-ope      .
           move      "A"                  to   w-cod-cod-aaq-tac      .
           move      w-tes-tip-mag        to   w-cod-cod-aaq-tco      .
           move      w-tes-num-pro        to   w-cod-cod-aaq-num      .
           move      w-tes-alf-pro        to   w-cod-cod-aaq-alf      .
           move      06                   to   w-cod-cod-aaq-lin      .
           move      26                   to   w-cod-cod-aaq-pos      .
           move      06                   to   w-cod-cod-aaq-dln      .
           move      41                   to   w-cod-cod-aaq-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-cod-aaq-cll-000  thru cod-cod-aaq-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-aaq-foi-000  thru cod-cod-aaq-foi-999    .
       acc-cod-dpm-110.
           perform   cod-cod-aaq-cll-000  thru cod-cod-aaq-cll-999    .
           if        w-cod-cod-aaq-ope    =    "F+"
                     go to acc-cod-dpm-115.
           if        w-cod-cod-aaq-ope    =    "AC"
                     go to acc-cod-dpm-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-dpm-115.
           perform   cod-cod-aaq-foi-000  thru cod-cod-aaq-foi-999    .
           go to     acc-cod-dpm-110.
       acc-cod-dpm-120.
           move      w-cod-cod-aaq-num    to   v-num                  .
           move      w-cod-cod-aaq-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-dpm-999.
       acc-cod-dpm-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-num-pro          .
           move      v-alf                to   w-tes-alf-pro          .
       acc-cod-dpm-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [dpm]                      *
      *                  *---------------------------------------------*
           move      w-tes-num-pro        to   w-let-arc-dpm-num      .
           perform   let-arc-dpm-000      thru let-arc-dpm-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-dpm-des    to   w-tes-num-pro-des      .
           move      w-let-arc-dpm-umi    to   w-tes-num-pro-umi      .
           move      w-let-arc-dpm-deq    to   w-tes-num-pro-deq      .
           move      zero                 to   w-tes-num-pro-dep      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-num-pro-des-000  thru vis-num-pro-des-999    .
      *                  *---------------------------------------------*
      *                  * Visualizzazione unita' di misura            *
      *                  *---------------------------------------------*
           perform   vis-num-pro-umi-000  thru vis-num-pro-umi-999    .
      *                  *---------------------------------------------*
      *                  * Se valore non trovato : reimpostazione      *
      *                  *---------------------------------------------*
           if        w-let-arc-dpm-flg    not  = spaces
                     go to acc-cod-dpm-100.
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione a meno  *
      *                  * che non sia stato premuto 'Up'              *
      *                  *---------------------------------------------*
           if        w-tes-alf-pro        not  = spaces
                     go to acc-cod-dpm-500.
           if        v-key                =    "UP  "
                     go to acc-cod-dpm-600
           else      go to acc-cod-dpm-100.
       acc-cod-dpm-500.
      *                  *---------------------------------------------*
      *                  * Lettura file [aaq] per controllo            *
      *                  *---------------------------------------------*
           move      w-tes-tip-mag        to   w-let-arc-aaq-tip      .
           move      w-tes-num-pro        to   w-let-arc-aaq-num      .
           perform   let-arc-aaq-000      thru let-arc-aaq-999        .
           if        w-let-arc-aaq-flg    =    spaces
                     go to acc-cod-dpm-550.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore e reimpostazione    *
      *                      *-----------------------------------------*
           move      "Mancano i dati generali di acquisto !"
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     acc-cod-dpm-100.
       acc-cod-dpm-550.
      *                  *---------------------------------------------*
      *                  * Preparazione valori medi letti da scheda di *
      *                  * acquisto 'aaq'                              *
      *                  *---------------------------------------------*
           move      w-let-arc-aaq-vlt    to   w-tes-vlt-std (1)      .
           move      w-let-arc-aaq-ndp    to   w-tes-ndp-std (1)      .
           move      w-let-arc-aaq-prz    to   w-tes-prz-med (1)      .
           move      w-let-arc-aaq-tmp    to   w-tes-tmp-med (1)      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione sigla valuta standard per   *
      *                  * il prodotto                                 *
      *                  *---------------------------------------------*
           perform   vis-vlt-std-000      thru vis-vlt-std-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione numero decimali standard    *
      *                  * per il prodotto                             *
      *                  *---------------------------------------------*
           perform   vis-ndp-std-000      thru vis-ndp-std-999        .
       acc-cod-dpm-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Attivazione flag di utilizzo variabili di   *
      *                  * ipc                                         *
      *                  *---------------------------------------------*
           move      "#"                  to   w-ipc-dlp-vlk-flg      .
       acc-cod-dpm-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-dpm-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-dpm-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-dpm-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-dpm-999.
       acc-cod-dpm-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice materiale vario               *
      *    *-----------------------------------------------------------*
       acc-cod-mtv-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se passata variabile di ipc e se pri-  *
      *                  * mo utilizzo di essa                         *
      *                  *---------------------------------------------*
           if        w-ipc-dlp-vlk-flg    not  = spaces
                     go to acc-cod-mtv-100.
           move      w-ipc-dlp-vlk-nrm    to   w-tes-num-pro          .
           move      w-ipc-dlp-vlk-afm    to   w-tes-alf-pro          .
           perform   vis-num-pro-000      thru vis-num-pro-999        .
           go to     acc-cod-mtv-400.
       acc-cod-mtv-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-aaq-ope      .
           move      "A"                  to   w-cod-cod-aaq-tac      .
           move      w-tes-tip-mag        to   w-cod-cod-aaq-tco      .
           move      w-tes-num-pro        to   w-cod-cod-aaq-num      .
           move      w-tes-alf-pro        to   w-cod-cod-aaq-alf      .
           move      06                   to   w-cod-cod-aaq-lin      .
           move      26                   to   w-cod-cod-aaq-pos      .
           move      06                   to   w-cod-cod-aaq-dln      .
           move      41                   to   w-cod-cod-aaq-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-cod-aaq-cll-000  thru cod-cod-aaq-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-aaq-foi-000  thru cod-cod-aaq-foi-999    .
       acc-cod-mtv-110.
           perform   cod-cod-aaq-cll-000  thru cod-cod-aaq-cll-999    .
           if        w-cod-cod-aaq-ope    =    "F+"
                     go to acc-cod-mtv-115.
           if        w-cod-cod-aaq-ope    =    "AC"
                     go to acc-cod-mtv-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-mtv-115.
           perform   cod-cod-aaq-foi-000  thru cod-cod-aaq-foi-999    .
           go to     acc-cod-mtv-110.
       acc-cod-mtv-120.
           move      w-cod-cod-aaq-num    to   v-num                  .
           move      w-cod-cod-aaq-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-mtv-999.
       acc-cod-mtv-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-num-pro          .
           move      v-alf                to   w-tes-alf-pro          .
       acc-cod-mtv-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [mtv]                      *
      *                  *---------------------------------------------*
           move      w-tes-num-pro        to   w-let-arc-mtv-num      .
           perform   let-arc-mtv-000      thru let-arc-mtv-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-mtv-des    to   w-tes-num-pro-des      .
           move      w-let-arc-mtv-umi    to   w-tes-num-pro-umi      .
           move      w-let-arc-mtv-deq    to   w-tes-num-pro-deq      .
           move      zero                 to   w-tes-num-pro-dep      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-num-pro-des-000  thru vis-num-pro-des-999    .
      *                  *---------------------------------------------*
      *                  * Visualizzazione unita' di misura            *
      *                  *---------------------------------------------*
           perform   vis-num-pro-umi-000  thru vis-num-pro-umi-999    .
      *                  *---------------------------------------------*
      *                  * Se valore non trovato : reimpostazione      *
      *                  *---------------------------------------------*
           if        w-let-arc-mtv-flg    not  = spaces
                     go to acc-cod-mtv-100.
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione a meno  *
      *                  * che non sia stato premuto 'Up'              *
      *                  *---------------------------------------------*
           if        w-tes-alf-pro        not  = spaces
                     go to acc-cod-mtv-500.
           if        v-key                =    "UP  "
                     go to acc-cod-mtv-600
           else      go to acc-cod-mtv-100.
       acc-cod-mtv-500.
      *                  *---------------------------------------------*
      *                  * Lettura file [aaq] per controllo            *
      *                  *---------------------------------------------*
           move      w-tes-tip-mag        to   w-let-arc-aaq-tip      .
           move      w-tes-num-pro        to   w-let-arc-aaq-num      .
           perform   let-arc-aaq-000      thru let-arc-aaq-999        .
           if        w-let-arc-aaq-flg    =    spaces
                     go to acc-cod-mtv-550.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore e reimpostazione    *
      *                      *-----------------------------------------*
           move      "Mancano i dati generali di acquisto !"
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     acc-cod-mtv-100.
       acc-cod-mtv-550.
      *                  *---------------------------------------------*
      *                  * Preparazione valori medi letti da scheda di *
      *                  * acquisto 'aaq'                              *
      *                  *---------------------------------------------*
           move      w-let-arc-aaq-vlt    to   w-tes-vlt-std (1)      .
           move      w-let-arc-aaq-ndp    to   w-tes-ndp-std (1)      .
           move      w-let-arc-aaq-prz    to   w-tes-prz-med (1)      .
           move      w-let-arc-aaq-tmp    to   w-tes-tmp-med (1)      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione sigla valuta standard per   *
      *                  * il prodotto                                 *
      *                  *---------------------------------------------*
           perform   vis-vlt-std-000      thru vis-vlt-std-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione numero decimali standard    *
      *                  * per il prodotto                             *
      *                  *---------------------------------------------*
           perform   vis-ndp-std-000      thru vis-ndp-std-999        .
       acc-cod-mtv-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Attivazione flag di utilizzo variabili di   *
      *                  * ipc                                         *
      *                  *---------------------------------------------*
           move      "#"                  to   w-ipc-dlp-vlk-flg      .
       acc-cod-mtv-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-mtv-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-mtv-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-mtv-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-mtv-999.
       acc-cod-mtv-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Formato                              *
      *    *-----------------------------------------------------------*
       acc-fda-pif-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-prs-snx-fda        not  = "S"
                     go to acc-fda-pif-999.
       acc-fda-pif-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-aaf-ope      .
           move      w-tes-tip-mag        to   w-cod-cod-aaf-tpm      .
           move      w-tes-num-pro        to   w-cod-cod-aaf-nrm      .
           move      w-tes-alf-pro        to   w-cod-cod-aaf-afm      .
           move      w-tes-cod-dcf        to   w-cod-cod-aaf-dcf      .
           move      w-tes-fda-pif        to   w-cod-cod-aaf-fda      .
           move      07                   to   w-cod-cod-aaf-lin      .
           move      26                   to   w-cod-cod-aaf-pos      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-cod-aaf-cll-000  thru cod-cod-aaf-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-aaf-foi-000  thru cod-cod-aaf-foi-999    .
       acc-fda-pif-110.
           perform   cod-cod-aaf-cll-000  thru cod-cod-aaf-cll-999    .
           if        w-cod-cod-aaf-ope    =    "F+"
                     go to acc-fda-pif-115.
           if        w-cod-cod-aaf-ope    =    "AC"
                     go to acc-fda-pif-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-fda-pif-115.
           perform   cod-cod-aaf-foi-000  thru cod-cod-aaf-foi-999    .
           go to     acc-fda-pif-110.
       acc-fda-pif-120.
           move      w-cod-cod-aaf-fda    to   v-alf                  .
       acc-fda-pif-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-fda-pif-999.
       acc-fda-pif-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-fda-pif          .
       acc-fda-pif-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a non spaces il primo carattere   *
      *                  * non deve essere a spaces                    *
      *                  *---------------------------------------------*
           if        w-tes-fda-pif        =    spaces
                     go to acc-fda-pif-600.
      *
           if        w-tes-fda-pif
                    (01 : 01)             =    spaces
                     go to acc-fda-pif-100.
       acc-fda-pif-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-fda-pif-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-fda-pif-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-fda-pif-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-fda-pif-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-fda-pif-999.
       acc-fda-pif-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Formato                           *
      *    *-----------------------------------------------------------*
       vis-fda-pif-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
           if        w-prs-snx-fda        not  = "S"
                     go to vis-fda-pif-999.
       vis-fda-pif-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      26                   to   v-pos                  .
           move      w-tes-fda-pif        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-fda-pif-999.
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
                     move  all "#"        to   w-cnt-sts-ing-pte
                     move  all "#"        to   w-cnt-sts-imp-pte      .
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
      *              * La testata e' composta di nr. 3 pagine          *
      *              *-------------------------------------------------*
           move      3                    to   w-cnt-sts-imp-mpt      .
       dmp-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Determinazione si/no pagina w-cnt-sts-imp-npt da trattare *
      *    *-----------------------------------------------------------*
       snp-tes-reg-000.
      *              *-------------------------------------------------*
      *              * Flag di uscita a Si                             *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-imp-snp      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del numero pagina        *
      *              *-------------------------------------------------*
           go to     snp-tes-reg-100
                     snp-tes-reg-200
                     snp-tes-reg-300
                     depending            on   w-cnt-sts-imp-npt      .
           go to     snp-tes-reg-999.
       snp-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Pagina numero 1                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sempre gestita                              *
      *                  *---------------------------------------------*
           go to     snp-tes-reg-999.
       snp-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Pagina numero 2                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sempre gestita                              *
      *                  *---------------------------------------------*
           go to     snp-tes-reg-999.
       snp-tes-reg-300.
      *              *-------------------------------------------------*
      *              * Pagina numero 3                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sempre gestita                              *
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
      *                  * Codice prodotto per fornitore               *
      *                  *---------------------------------------------*
           perform   acc-cop-sfn-000      thru acc-cop-sfn-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-105.
      *                  *---------------------------------------------*
      *                  * Opzione per la descrizione                  *
      *                  *---------------------------------------------*
           perform   acc-opz-des-000      thru acc-opz-des-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-100.
       acc-tes-reg-110.
      *                  *---------------------------------------------*
      *                  * Descrizione prodotto per fornitore          *
      *                  *---------------------------------------------*
           perform   acc-dep-sfn-000      thru acc-dep-sfn-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-105.
       acc-tes-reg-115.
      *                  *---------------------------------------------*
      *                  * Riferimento al listino                      *
      *                  *---------------------------------------------*
           perform   acc-rif-lst-000      thru acc-rif-lst-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-110.
       acc-tes-reg-120.
      *                  *---------------------------------------------*
      *                  * Sigla valuta                                *
      *                  *---------------------------------------------*
           perform   acc-sgl-vlt-000      thru acc-sgl-vlt-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-115.
       acc-tes-reg-130.
      *                  *---------------------------------------------*
      *                  * Tipo prezzo d'acquisto                      *
      *                  *---------------------------------------------*
           perform   acc-tip-pza-000      thru acc-tip-pza-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-120.
       acc-tes-reg-140.
      *                  *---------------------------------------------*
      *                  * Numero decimali prezzo d'acquisto           *
      *                  *---------------------------------------------*
           perform   acc-dec-prz-000      thru acc-dec-prz-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-130.
       acc-tes-reg-150.
      *                  *---------------------------------------------*
      *                  * Lotto di acquisto                           *
      *                  *---------------------------------------------*
           perform   acc-lot-acq-000      thru acc-lot-acq-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-140.
       acc-tes-reg-160.
      *                  *---------------------------------------------*
      *                  * Tipo applicazione prezzi e sconti           *
      *                  *---------------------------------------------*
           perform   acc-tap-pes-000      thru acc-tap-pes-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-150.
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
      *                  * Prezzo d'acquisto                           *
      *                  *---------------------------------------------*
           perform   acc-prz-pes-000      thru acc-prz-pes-999        .
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
      *                  * Percentuali di sconto                       *
      *                  *---------------------------------------------*
           perform   acc-psr-pes-000      thru acc-psr-pes-999        .
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
      *                  * Tabella prezzi d'acquisto e sconti          *
      *                  *---------------------------------------------*
           perform   acc-tbl-pes-000      thru acc-tbl-pes-999        .
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
      *                  * Data aggiornamento prezzo                   *
      *                  *---------------------------------------------*
           perform   acc-uda-pes-000      thru acc-uda-pes-999        .
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
      *                  * Tempo di consegna                           *
      *                  *---------------------------------------------*
           perform   acc-tmp-cns-000      thru acc-tmp-cns-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-230.
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
                     go to acc-tes-reg-240.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
           go to     acc-tes-reg-999.
       acc-tes-reg-300.
      *                  *---------------------------------------------*
      *                  * Sigla valuta per legame valutario           *
      *                  *---------------------------------------------*
           perform   acc-lgv-vlt-000      thru acc-lgv-vlt-999        .
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
      *                  * Coefficiente di cambio per legame valutario *
      *                  *---------------------------------------------*
           perform   acc-lgv-cdc-000      thru acc-lgv-cdc-999        .
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
      *                  * % di tolleranza per legame valutario        *
      *                  *---------------------------------------------*
           perform   acc-lgv-pdt-000      thru acc-lgv-pdt-999        .
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
      *                  * Si/no trasformazione u.d.m.                 *
      *                  *---------------------------------------------*
           perform   acc-snx-tum-000      thru acc-snx-tum-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-320.
       acc-tes-reg-340.
      *                  *---------------------------------------------*
      *                  * Unita' di misura fornitore                  *
      *                  *---------------------------------------------*
           perform   acc-umf-tum-000      thru acc-umf-tum-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-330.
       acc-tes-reg-350.
      *                  *---------------------------------------------*
      *                  * Numero decimali quantita'                   *
      *                  *---------------------------------------------*
           perform   acc-nde-tum-000      thru acc-nde-tum-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-340.
       acc-tes-reg-360.
      *                  *---------------------------------------------*
      *                  * Coefficiente moltiplicatore                 *
      *                  *---------------------------------------------*
           perform   acc-cmo-tum-000      thru acc-cmo-tum-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-350.
       acc-tes-reg-370.
      *                  *---------------------------------------------*
      *                  * Coefficiente divisore                       *
      *                  *---------------------------------------------*
           perform   acc-cdi-tum-000      thru acc-cdi-tum-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-360.
       acc-tes-reg-380.
      *                  *---------------------------------------------*
      *                  * Annotazioni                                 *
      *                  *---------------------------------------------*
           perform   acc-ann-not-000      thru acc-ann-not-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-370.
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
                     vis-tes-reg-200
                     vis-tes-reg-300
                     depending            on   w-cnt-sts-imp-npt      .
           go to     vis-tes-reg-999.
       vis-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Codice prodotto per fornitore                   *
      *              *-------------------------------------------------*
           perform   vis-cop-sfn-000      thru vis-cop-sfn-999        .
      *              *-------------------------------------------------*
      *              * Opzione per la descrizione                      *
      *              *-------------------------------------------------*
           perform   vis-opz-des-000      thru vis-opz-des-999        .
      *              *-------------------------------------------------*
      *              * Descrizione prodotto per fornitore              *
      *              *-------------------------------------------------*
           perform   vis-dep-sfn-000      thru vis-dep-sfn-999        .
      *              *-------------------------------------------------*
      *              * Riferimento al listino                          *
      *              *-------------------------------------------------*
           perform   vis-rif-lst-000      thru vis-rif-lst-999        .
      *              *-------------------------------------------------*
      *              * Sigla valuta                                    *
      *              *-------------------------------------------------*
           perform   vis-sgl-vlt-000      thru vis-sgl-vlt-999        .
           perform   vis-sgl-vlt-des-000  thru vis-sgl-vlt-des-999    .
      *              *-------------------------------------------------*
      *              * Sigla valuta standard per il prodotto           *
      *              *-------------------------------------------------*
           perform   vis-vlt-std-000      thru vis-vlt-std-999        .
      *              *-------------------------------------------------*
      *              * Tipo prezzo d'acquisto                          *
      *              *-------------------------------------------------*
           perform   vis-tip-pza-000      thru vis-tip-pza-999        .
      *              *-------------------------------------------------*
      *              * Numero decimali prezzo d'acquisto               *
      *              *-------------------------------------------------*
           perform   vis-dec-prz-000      thru vis-dec-prz-999        .
      *              *-------------------------------------------------*
      *              * Numero decimali prezzo d'acquisto standard      *
      *              *-------------------------------------------------*
           perform   vis-ndp-std-000      thru vis-ndp-std-999        .
      *              *-------------------------------------------------*
      *              * Lotto di acquisto                               *
      *              *-------------------------------------------------*
           perform   vis-lot-acq-000      thru vis-lot-acq-999        .
      *              *-------------------------------------------------*
      *              * Tipo applicazione prezzi e sconti               *
      *              *-------------------------------------------------*
           perform   vis-tap-pes-000      thru vis-tap-pes-999        .
           go to     vis-tes-reg-999.
       vis-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Prezzo d'acquisto                               *
      *              *-------------------------------------------------*
           perform   vis-prz-pes-000      thru vis-prz-pes-999        .
      *              *-------------------------------------------------*
      *              * Prezzo di acquisto medio                        *
      *              *-------------------------------------------------*
           perform   vis-prz-med-000      thru vis-prz-med-999        .
      *              *-------------------------------------------------*
      *              * Percentuali di sconto                           *
      *              *-------------------------------------------------*
           perform   vis-psr-pes-000      thru vis-psr-pes-999        .
      *              *-------------------------------------------------*
      *              * Tabella prezzi d'acquisto e sconti              *
      *              *-------------------------------------------------*
           perform   vis-tbl-pes-000      thru vis-tbl-pes-999        .
      *              *-------------------------------------------------*
      *              * Data aggiornamento prezzo                       *
      *              *-------------------------------------------------*
           perform   vis-uda-pes-000      thru vis-uda-pes-999        .
      *              *-------------------------------------------------*
      *              * Tempo di consegna                               *
      *              *-------------------------------------------------*
           perform   vis-tmp-cns-000      thru vis-tmp-cns-999        .
      *              *-------------------------------------------------*
      *              * Tempo medio di consegna                         *
      *              *-------------------------------------------------*
           perform   vis-tmp-med-000      thru vis-tmp-med-999        .
           go to     vis-tes-reg-999.
       vis-tes-reg-300.
      *              *-------------------------------------------------*
      *              * Sigla valuta per legame valutario               *
      *              *-------------------------------------------------*
           perform   vis-lgv-vlt-000      thru vis-lgv-vlt-999        .
      *              *-------------------------------------------------*
      *              * Coefficiente di cambio per legame valutario     *
      *              *-------------------------------------------------*
           perform   vis-lgv-cdc-000      thru vis-lgv-cdc-999        .
      *              *-------------------------------------------------*
      *              * % di tolleranza per legame valutario            *
      *              *-------------------------------------------------*
           perform   vis-lgv-pdt-000      thru vis-lgv-pdt-999        .
      *              *-------------------------------------------------*
      *              * Si/no trasformazione u.d.m.                     *
      *              *-------------------------------------------------*
           perform   vis-snx-tum-000      thru vis-snx-tum-999        .
      *              *-------------------------------------------------*
      *              * Unita' di misura fornitore                      *
      *              *-------------------------------------------------*
           perform   vis-umf-tum-000      thru vis-umf-tum-999        .
      *              *-------------------------------------------------*
      *              * Numero decimali quantita'                       *
      *              *-------------------------------------------------*
           perform   vis-nde-tum-000      thru vis-nde-tum-999        .
      *              *-------------------------------------------------*
      *              * Coefficiente moltiplicatore                     *
      *              *-------------------------------------------------*
           perform   vis-cmo-tum-000      thru vis-cmo-tum-999        .
      *              *-------------------------------------------------*
      *              * Coefficiente divisore                           *
      *              *-------------------------------------------------*
           perform   vis-cdi-tum-000      thru vis-cdi-tum-999        .
      *              *-------------------------------------------------*
      *              * Annotazioni                                     *
      *              *-------------------------------------------------*
           perform   vis-ann-not-000      thru vis-ann-not-999        .
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
           move      09                   to   v-lin                  .
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
      *              * Codice prodotto per fornitore                   *
      *              *-------------------------------------------------*
           perform   pmt-cop-sfn-000      thru pmt-cop-sfn-999        .
      *              *-------------------------------------------------*
      *              * Opzione per la descrizione                      *
      *              *-------------------------------------------------*
           perform   pmt-opz-des-000      thru pmt-opz-des-999        .
      *              *-------------------------------------------------*
      *              * Descrizione prodotto per fornitore              *
      *              *-------------------------------------------------*
           perform   pmt-dep-sfn-000      thru pmt-dep-sfn-999        .
      *              *-------------------------------------------------*
      *              * Riferimento al listino                          *
      *              *-------------------------------------------------*
           perform   pmt-rif-lst-000      thru pmt-rif-lst-999        .
      *              *-------------------------------------------------*
      *              * Sigla valuta                                    *
      *              *-------------------------------------------------*
           perform   pmt-sgl-vlt-000      thru pmt-sgl-vlt-999        .
      *              *-------------------------------------------------*
      *              * Tipo prezzo d'acquisto                          *
      *              *-------------------------------------------------*
           perform   pmt-tip-pza-000      thru pmt-tip-pza-999        .
      *              *-------------------------------------------------*
      *              * Numero decimali prezzo d'acquisto               *
      *              *-------------------------------------------------*
           perform   pmt-dec-prz-000      thru pmt-dec-prz-999        .
      *              *-------------------------------------------------*
      *              * Lotto di acquisto                               *
      *              *-------------------------------------------------*
           perform   pmt-lot-acq-000      thru pmt-lot-acq-999        .
      *              *-------------------------------------------------*
      *              * Tipo applicazione prezzi e sconti               *
      *              *-------------------------------------------------*
           perform   pmt-tap-pes-000      thru pmt-tap-pes-999        .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Prezzo d'acquisto                               *
      *              *-------------------------------------------------*
           perform   pmt-prz-pes-000      thru pmt-prz-pes-999        .
      *              *-------------------------------------------------*
      *              * Percentuali di sconto                           *
      *              *-------------------------------------------------*
           perform   pmt-psr-pes-000      thru pmt-psr-pes-999        .
      *              *-------------------------------------------------*
      *              * Tabella prezzi d'acquisto e sconti              *
      *              *-------------------------------------------------*
           perform   pmt-tbl-pes-000      thru pmt-tbl-pes-999        .
      *              *-------------------------------------------------*
      *              * Data aggiornamento prezzo                       *
      *              *-------------------------------------------------*
           perform   pmt-uda-pes-000      thru pmt-uda-pes-999        .
      *              *-------------------------------------------------*
      *              * Tempo di consegna                               *
      *              *-------------------------------------------------*
           perform   pmt-tmp-cns-000      thru pmt-tmp-cns-999        .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-300.
      *              *-------------------------------------------------*
      *              * Sigla valuta per legame valutario               *
      *              *-------------------------------------------------*
           perform   pmt-lgv-vlt-000      thru pmt-lgv-vlt-999        .
      *              *-------------------------------------------------*
      *              * Coefficiente di cambio per legame valutario     *
      *              *-------------------------------------------------*
           perform   pmt-lgv-cdc-000      thru pmt-lgv-cdc-999        .
      *              *-------------------------------------------------*
      *              * % di tolleranza per legame valutario            *
      *              *-------------------------------------------------*
           perform   pmt-lgv-pdt-000      thru pmt-lgv-pdt-999        .
      *              *-------------------------------------------------*
      *              * Si/no trasformazione u.d.m.                     *
      *              *-------------------------------------------------*
           perform   pmt-snx-tum-000      thru pmt-snx-tum-999        .
      *              *-------------------------------------------------*
      *              * Unita' di misura fornitore                      *
      *              *-------------------------------------------------*
           perform   pmt-umf-tum-000      thru pmt-umf-tum-999        .
      *              *-------------------------------------------------*
      *              * Numero decimali quantita'                       *
      *              *-------------------------------------------------*
           perform   pmt-nde-tum-000      thru pmt-nde-tum-999        .
      *              *-------------------------------------------------*
      *              * Coefficiente moltiplicatore                     *
      *              *-------------------------------------------------*
           perform   pmt-cmo-tum-000      thru pmt-cmo-tum-999        .
      *              *-------------------------------------------------*
      *              * Coefficiente divisore                           *
      *              *-------------------------------------------------*
           perform   pmt-cdi-tum-000      thru pmt-cdi-tum-999        .
      *              *-------------------------------------------------*
      *              * Annotazioni                                     *
      *              *-------------------------------------------------*
           perform   pmt-ann-not-000      thru pmt-ann-not-999        .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice prodotto per fornitore    *
      *    *-----------------------------------------------------------*
       pmt-cop-sfn-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice per il fornitore    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cop-sfn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Opzione per la descrizione       *
      *    *-----------------------------------------------------------*
       pmt-opz-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Descrizione per documenti  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-opz-des-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Descrizione prodotto per fornit. *
      *    *-----------------------------------------------------------*
       pmt-dep-sfn-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           if        w-acc-opz-des-acc    =    02
                     move  "Descrizione per fornitore  :"
                                          to   v-alf
           else      move  spaces         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dep-sfn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : riferimento al listino fornitore        *
      *    *-----------------------------------------------------------*
       pmt-rif-lst-000.
      *              *-------------------------------------------------*
      *              * Pre-visualizzazione prompts                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da visualizzare               *
      *                  *---------------------------------------------*
           if        w-prs-rif-lst        =    "N"
                     go to pmt-rif-lst-999.
       pmt-rif-lst-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts                         *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Rif. a listino fornitore   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-rif-lst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Sigla valuta                     *
      *    *-----------------------------------------------------------*
       pmt-sgl-vlt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Sigla valuta per il prezzo :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-sgl-vlt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tipo prezzo di acquisto          *
      *    *-----------------------------------------------------------*
       pmt-tip-pza-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           if        w-tes-tip-mag        =    01
                     move  "Tipo prezzo di acquisto    :"
                                          to   v-alf
           else      move  spaces         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-pza-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Numero decimali prezzo d'acqui-  *
      *    * sto                                                       *
      *    *-----------------------------------------------------------*
       pmt-dec-prz-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero decimali prezzo     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dec-prz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Lotto di acquisto                *
      *    *-----------------------------------------------------------*
       pmt-lot-acq-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Lotto di acquisto          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-lot-acq-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tipo applicazione prezzi e sconti*
      *    *-----------------------------------------------------------*
       pmt-tap-pes-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo applicazione prezzi e :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                    sconti  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tap-pes-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Prezzo d'acquisto                *
      *    *-----------------------------------------------------------*
       pmt-prz-pes-000.
      *              *-------------------------------------------------*
      *              * Test se da visualizzare                         *
      *              *-------------------------------------------------*
           if        w-tes-tap-pes (1)    not  = 01
                     go to pmt-prz-pes-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Prezzo di acquisto per     :"
                                          to   v-alf                  .
      *
           if        w-tes-snx-tum (1)    =    "N"
                     move  w-tes-num-pro-umi
                                          to   v-alf (24 : 03)
           else if   w-tes-snx-tum (1)    =    "S"
                     move  w-tes-umf-tum (1)
                                          to   v-alf (24 : 03)
           else if   w-tes-snx-tum (1)    =    "P"
                     move  w-tes-umf-tum (1)
                                          to   v-alf (24 : 03)
           else      move  w-tes-num-pro-umi
                                          to   v-alf (24 : 03)        .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-prz-pes-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Percentuali di sconto            *
      *    *-----------------------------------------------------------*
       pmt-psr-pes-000.
      *              *-------------------------------------------------*
      *              * Test se da visualizzare                         *
      *              *-------------------------------------------------*
           if        w-tes-tap-pes (1)    not  = 01
                     go to pmt-psr-pes-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Percentuali di sconto      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-psr-pes-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tabella prezzi e sconti          *
      *    *-----------------------------------------------------------*
       pmt-tbl-pes-000.
      *              *-------------------------------------------------*
      *              * Test se da visualizzare                         *
      *              *-------------------------------------------------*
           if        w-tes-tap-pes (1)    not  = 02
                     go to pmt-tbl-pes-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                             +--------------------
      -              "-----------------------------+"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tabella prezzi di acquisto : | Quantita' |   Prezz
      -              "o   | Percentuali di sconto  |"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                             +-----------+--------
      -              "----+------------------------+"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                             |           |        
      -              "    |                        |"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                             |           |        
      -              "    |                        |"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                             |           |        
      -              "    |                        |"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                             |           |        
      -              "    |                        |"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                             |           |        
      -              "    |                        |"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                             |           |        
      -              "    |                        |"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                             +--------------------
      -              "-----------------------------+"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tbl-pes-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Data aggiornamento prezzo        *
      *    *-----------------------------------------------------------*
       pmt-uda-pes-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           if        w-tes-tap-pes (1)    =    01
                     move  14             to   v-lin
           else      move  19             to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data aggiornamento prezzo  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-uda-pes-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tempo di consegna in giorni      *
      *    *-----------------------------------------------------------*
       pmt-tmp-cns-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           if        w-tes-tap-pes (1)    =    01
                     move  16             to   v-lin
           else      move  21             to   v-lin                  .
           move      01                   to   v-pos                  .
           if        w-prs-mde-tdc        =    1
                     move  "Tempo di consegna  (sett.) :"
                                          to   v-alf
           else      move  "Tempo di consegna    (gg.) :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tmp-cns-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Sigla valuta per legame valutario*
      *    *-----------------------------------------------------------*
       pmt-lgv-vlt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Legame valutario           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-lgv-vlt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Coefficiente di cambio per lega- *
      *    * me valutario                                              *
      *    *-----------------------------------------------------------*
       pmt-lgv-cdc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Coefficiente di cambio     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-lgv-cdc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Percentuali di tolleranza per    *
      *    * legame valutario                                          *
      *    *-----------------------------------------------------------*
       pmt-lgv-pdt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tolleranza         (+/- %) :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-lgv-pdt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no trasformazione u.d.m.      *
      *    *-----------------------------------------------------------*
       pmt-snx-tum-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "            --- Dati per la trasformazione dell'un
      -              "ita' di misura ---            "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Trasformazione da eseguire :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-tum-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Unita' di misura fornitore       *
      *    *-----------------------------------------------------------*
       pmt-umf-tum-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Unita' di misura fornitore :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-umf-tum-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Numero decimali quantita'        *
      *    *-----------------------------------------------------------*
       pmt-nde-tum-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero decimali quantita'  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-nde-tum-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Coefficiente moltiplicatore      *
      *    *-----------------------------------------------------------*
       pmt-cmo-tum-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione della trasformazione     *
      *              * dell'unita' di misura                           *
      *              *-------------------------------------------------*
           if        w-tes-snx-tum (1)    not  = "P"
                     go to pmt-cmo-tum-200.
       pmt-cmo-tum-100.
      *              *-------------------------------------------------*
      *              * Se trasformazione solo per il prezzo            *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      22                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      47                   to   v-pos                  .
           move      "Coeff. divisore      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-cmo-tum-999.
       pmt-cmo-tum-200.
      *              *-------------------------------------------------*
      *              * Tutti gli altri casi                            *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      22                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      47                   to   v-pos                  .
           move      "Coeff. moltiplicatore:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-cmo-tum-999.
       pmt-cmo-tum-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Coefficiente divisore            *
      *    *-----------------------------------------------------------*
       pmt-cdi-tum-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione della trasformazione     *
      *              * dell'unita' di misura                           *
      *              *-------------------------------------------------*
           if        w-tes-snx-tum (1)    not  = "P"
                     go to pmt-cdi-tum-200.
       pmt-cdi-tum-100.
      *              *-------------------------------------------------*
      *              * Se trasformazione solo per il prezzo            *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      22                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      47                   to   v-pos                  .
           move      "Coeff. moltiplicatore:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-cdi-tum-999.
       pmt-cdi-tum-200.
      *              *-------------------------------------------------*
      *              * Tutti gli altri casi                            *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      22                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      47                   to   v-pos                  .
           move      "Coeff. divisore      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-cdi-tum-999.
       pmt-cdi-tum-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Annotazioni                      *
      *    *-----------------------------------------------------------*
       pmt-ann-not-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Annotazioni                :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ann-not-999.
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
      *    * Accettazione campo testata : Codice prodotto fornitore    *
      *    *-----------------------------------------------------------*
       acc-cop-sfn-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cop-sfn-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-cop-sfn (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cop-sfn-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cop-sfn-999.
       acc-cop-sfn-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-cop-sfn (1)      .
       acc-cop-sfn-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a non spaces il primo carattere   *
      *                  * non deve essere a spaces                    *
      *                  *---------------------------------------------*
           if        w-tes-cop-sfn (1)    =    spaces
                     go to acc-cop-sfn-600.
      *
           if        w-tes-cop-sfn (1)
                    (01 : 01)             =    spaces
                     go to acc-cop-sfn-100.
       acc-cop-sfn-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cop-sfn-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cop-sfn-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cop-sfn-100.
       acc-cop-sfn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice prodotto fornitore *
      *    *-----------------------------------------------------------*
       vis-cop-sfn-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cop-sfn (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cop-sfn-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Opzione per la descrizione   *
      *    *-----------------------------------------------------------*
       acc-opz-des-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-opz-des-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-opz-des-lun    to   v-car                  .
           move      w-exp-opz-des-num    to   v-ldt                  .
           move      "PS#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-opz-des-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-acc-opz-des-acc    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-opz-des-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-opz-des-999.
       acc-opz-des-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-acc-opz-des-acc      .
       acc-opz-des-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore zero non ammesso, a meno che non si  *
      *                  * sia in Up                                   *
      *                  *---------------------------------------------*
           if        w-acc-opz-des-acc    not  = zero
                     go to acc-opz-des-600.
           if        v-key                =    "UP  "
                     go to acc-opz-des-600
           else      go to acc-opz-des-100.
       acc-opz-des-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt descrizione per il fornitore         *
      *                  *---------------------------------------------*
           perform   pmt-dep-sfn-000      thru pmt-dep-sfn-999        .
      *                  *---------------------------------------------*
      *                  * Trattamento descrizione per il fornitore    *
      *                  *---------------------------------------------*
           if        w-acc-opz-des-acc    not  = 01
                     go to acc-opz-des-800.
           move      spaces               to   w-tes-dep-sfn (1)      .
           perform   vis-dep-sfn-000      thru vis-dep-sfn-999        .
           move      zero                 to   w-tes-xdp-sfn (1)      .
       acc-opz-des-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-opz-des-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-opz-des-100.
       acc-opz-des-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Opzione per la descrizione        *
      *    *-----------------------------------------------------------*
       vis-opz-des-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-opz-des-lun    to   v-car                  .
           move      w-exp-opz-des-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-opz-des-tbl    to   v-txt                  .
           move      w-acc-opz-des-acc    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-opz-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Descrizione per fornitore    *
      *    *-----------------------------------------------------------*
       acc-dep-sfn-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-acc-opz-des-acc    =    01
                     go to acc-dep-sfn-999.
      *                  *---------------------------------------------*
      *                  * Eventuale default                           *
      *                  *---------------------------------------------*
           if        w-tes-dep-rig (1, 1) not  = spaces
                     go to acc-dep-sfn-100.
           if        w-prs-def-ins-dad    not  = 01
                     go to acc-dep-sfn-100.
           move      w-tes-num-pro-des    to   w-tes-dep-rig (1, 1)   .
       acc-dep-sfn-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "T"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-ldt                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-dep-sfn (1)    to   v-txt                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dep-sfn-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dep-sfn-999.
       acc-dep-sfn-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-txt                to   w-tes-dep-sfn (1)      .
       acc-dep-sfn-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : nessun compattamento   *
      *                  *---------------------------------------------*
           if        w-tes-dep-sfn (1)    not  = spaces
                     go to acc-dep-sfn-500
           else      go to acc-dep-sfn-600.
       acc-dep-sfn-500.
      *                  *---------------------------------------------*
      *                  * Compattamento verso l'alto delle righe di   *
      *                  * descrizione                                 *
      *                  *---------------------------------------------*
           move      w-tes-dep-sfn (1)    to   w-cmp-rig-txt-txt      .
           move      10                   to   w-cmp-rig-txt-max      .
           perform   cmp-rig-txt-000      thru cmp-rig-txt-999        .
      *                  *---------------------------------------------*
      *                  * Se compattamento avvenuto : reimpostazione  *
      *                  *---------------------------------------------*
           if        w-cmp-rig-txt-flg    =    spaces
                     go to acc-dep-sfn-600.
           move      w-cmp-rig-txt-txt    to   w-tes-dep-sfn (1)      .
           go to     acc-dep-sfn-100.
       acc-dep-sfn-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di estensione alla descrizione         *
      *                  *---------------------------------------------*
           if        w-cmp-rig-txt-nrd    not  > 1
                     move  0              to   w-tes-xdp-sfn (1)
           else      move  1              to   w-tes-xdp-sfn (1)      .
       acc-dep-sfn-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dep-sfn-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dep-sfn-100.
       acc-dep-sfn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione per documenti *
      *    *-----------------------------------------------------------*
       vis-dep-sfn-000.
           move      "DS"                 to   v-ope                  .
           move      "T"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-ldt                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-dep-sfn (1)    to   v-txt                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dep-sfn-999.
           exit.

      *    *===========================================================*
      *    * Routine di compattamento righe di testo : max 10 righe    *
      *    *-----------------------------------------------------------*
       cmp-rig-txt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di rivisualizzazione de-   *
      *              * scrizione prodotto                              *
      *              *-------------------------------------------------*
           move      spaces               to   w-cmp-rig-txt-flg      .
      *              *-------------------------------------------------*
      *              * Determinazione numero righe attuale             *
      *              *-------------------------------------------------*
           move      zero                 to   w-cmp-rig-txt-nri      .
           add       1                    to   w-cmp-rig-txt-max
                                        giving w-cmp-rig-txt-nri      .
       cmp-rig-txt-100.
           subtract  1                    from w-cmp-rig-txt-nri      .
           if        w-cmp-rig-txt-rig
                    (w-cmp-rig-txt-nri)   =    spaces
                     go to cmp-rig-txt-100.
      *              *-------------------------------------------------*
      *              * Inizializzazione numero righe descrizione       *
      *              *-------------------------------------------------*
           move      zero                 to   w-cmp-rig-txt-nrd      .
      *              *-------------------------------------------------*
      *              * Ciclo per compattamento                         *
      *              *-------------------------------------------------*
           move      zero                 to   w-cmp-rig-txt-ctr      .
       cmp-rig-txt-200.
           add       1                    to   w-cmp-rig-txt-ctr      .
           if        w-cmp-rig-txt-ctr    >    w-cmp-rig-txt-nri
                     go to cmp-rig-txt-300.
      *                  *---------------------------------------------*
      *                  * Se riga di descrizione a spazi : riciclo    *
      *                  *---------------------------------------------*
           if        w-cmp-rig-txt-rig
                    (w-cmp-rig-txt-ctr)   =    spaces
                     go to cmp-rig-txt-200.
      *                  *---------------------------------------------*
      *                  * Incremento contatore numero righe           *
      *                  *---------------------------------------------*
           add       1                    to   w-cmp-rig-txt-nrd      .
      *                  *---------------------------------------------*
      *                  * Spostamento riga di descrizione             *
      *                  *---------------------------------------------*
           move      w-cmp-rig-txt-rig
                    (w-cmp-rig-txt-ctr)   to   w-cmp-rig-txt-rig
                                              (w-cmp-rig-txt-nrd)     .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     cmp-rig-txt-200.
       cmp-rig-txt-300.
      *              *-------------------------------------------------*
      *              * Abblencamento righe residue                     *
      *              *-------------------------------------------------*
           move      w-cmp-rig-txt-nrd    to   w-cmp-rig-txt-ctr      .
       cmp-rig-txt-320.
           add       1                    to   w-cmp-rig-txt-ctr      .
           if        w-cmp-rig-txt-ctr    >    w-cmp-rig-txt-nri
                     go to cmp-rig-txt-900.
           move      spaces               to   w-cmp-rig-txt-rig
                                              (w-cmp-rig-txt-ctr)     .
           go to     cmp-rig-txt-320.
       cmp-rig-txt-900.
      *              *-------------------------------------------------*
      *              * Determinazione del flag di rivisualizzazione    *
      *              *-------------------------------------------------*
           if        w-cmp-rig-txt-nri    not  = w-cmp-rig-txt-nrd
                     move  "#"            to   w-cmp-rig-txt-flg      .
       cmp-rig-txt-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Riferimento a listino fornitore            *
      *    *-----------------------------------------------------------*
       acc-rif-lst-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-prs-rif-lst        =    "N"
                     go to acc-rif-lst-999.
           if        w-tes-tip-mag        not  = 01
                     go to acc-rif-lst-999.
       acc-rif-lst-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO"                 to   v-pfk (05)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-rif-lst (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-rif-lst-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-rif-lst-999.
      *              *-------------------------------------------------*
      *              * Valore impostato                                *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-rif-lst (1)      .
       acc-rif-lst-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-rif-lst-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-rif-lst-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-rif-lst-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-rif-lst-100.
       acc-rif-lst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Riferimento a listino fornitore   *
      *    *-----------------------------------------------------------*
       vis-rif-lst-000.
      *              *-------------------------------------------------*
      *              * Pre-visualizzazione                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da visualizzare               *
      *                  *---------------------------------------------*
           if        w-prs-rif-lst        =    "N"
                     go to vis-rif-lst-999.
       vis-rif-lst-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-rif-lst (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rif-lst-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Sigla valuta                 *
      *    *-----------------------------------------------------------*
       acc-sgl-vlt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        w-tes-sgl-vlt (1)    not  = spaces
                     go to acc-sgl-vlt-100.
      *                      *-----------------------------------------*
      *                      * Sigla valuta legata al prodotto         *
      *                      *-----------------------------------------*
           if        w-tes-vlt-std (1)    =    spaces
                     go to acc-sgl-vlt-025.
           move      w-tes-vlt-std (1)    to   w-tes-sgl-vlt (1)      .
           go to     acc-sgl-vlt-100.
       acc-sgl-vlt-025.
      *                      *-----------------------------------------*
      *                      * Sigla valuta legata al fornitore        *
      *                      *-----------------------------------------*
           if        w-tes-cod-dcf-vlt    =    spaces
                     go to acc-sgl-vlt-050.
           move      w-tes-cod-dcf-vlt    to   w-tes-sgl-vlt (1)      .
           go to     acc-sgl-vlt-100.
       acc-sgl-vlt-050.
      *                      *-----------------------------------------*
      *                      * Sigla valuta base                       *
      *                      *-----------------------------------------*
           move      c-sgl                to   w-tes-sgl-vlt (1)      .
       acc-sgl-vlt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-zvl-ope      .
           move      w-tes-sgl-vlt (1)    to   w-cod-cod-zvl-cod      .
           move      14                   to   w-cod-cod-zvl-lin      .
           move      30                   to   w-cod-cod-zvl-pos      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "NXSC"               to   v-pfk (08)             .
           perform   cod-cod-zvl-cll-000  thru cod-cod-zvl-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-zvl-foi-000  thru cod-cod-zvl-foi-999    .
       acc-sgl-vlt-110.
           perform   cod-cod-zvl-cll-000  thru cod-cod-zvl-cll-999    .
           if        w-cod-cod-zvl-ope    =    "F+"
                     go to acc-sgl-vlt-115.
           if        w-cod-cod-zvl-ope    =    "AC"
                     go to acc-sgl-vlt-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-sgl-vlt-115.
           perform   cod-cod-zvl-foi-000  thru cod-cod-zvl-foi-999    .
           go to     acc-sgl-vlt-110.
       acc-sgl-vlt-120.
           move      w-cod-cod-zvl-cod    to   v-alf                  .
       acc-sgl-vlt-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-sgl-vlt-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-sgl-vlt-999.
       acc-sgl-vlt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-sgl-vlt (1)      .
       acc-sgl-vlt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella                             *
      *                  *---------------------------------------------*
           move      w-tes-sgl-vlt (1)    to   w-let-arc-zvl-cod      .
           perform   let-arc-zvl-000      thru let-arc-zvl-999        .
           move      w-let-arc-zvl-des    to   w-tes-sgl-vlt-des (1)  .
           move      w-let-arc-zvl-dec    to   w-tes-dec-vlt (1)      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-sgl-vlt-des-000  thru vis-sgl-vlt-des-999    .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zvl-flg    not  = spaces
                     go to acc-sgl-vlt-100.
       acc-sgl-vlt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sgl-vlt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-sgl-vlt-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-sgl-vlt-100.
       acc-sgl-vlt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Sigla valuta              *
      *    *-----------------------------------------------------------*
       vis-sgl-vlt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-sgl-vlt (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sgl-vlt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione valuta        *
      *    *-----------------------------------------------------------*
       vis-sgl-vlt-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-sgl-vlt-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sgl-vlt-des-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Sigla valuta standard per *
      *    * il prodotto                                               *
      *    *-----------------------------------------------------------*
       vis-vlt-std-000.
      *              *-------------------------------------------------*
      *              * Test se da visualizzare                         *
      *              *-------------------------------------------------*
           if        w-tes-vlt-std (1)    =    c-sgl
                     go to vis-vlt-std-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      68                   to   v-pos                  .
           move      "Standard:"          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Valore                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      w-tes-vlt-std (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-vlt-std-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Tipo prezzo d'acquisto       *
      *    *-----------------------------------------------------------*
       acc-tip-pza-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-tip-pza (1)    to   w-sav-tip-pza          .
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-tip-mag        =    01
                     go to acc-tip-pza-100.
      *                      *-----------------------------------------*
      *                      * Se non da accettare : forzatura al va-  *
      *                      * lore 01 e uscita                        *
      *                      *-----------------------------------------*
           move      01                   to   w-tes-tip-pza (1)      .
           go to     acc-tip-pza-999.
       acc-tip-pza-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-pza-lun    to   v-car                  .
           move      w-exp-tip-pza-num    to   v-ldt                  .
           move      "SL#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-pza-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-tip-pza (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-tip-pza-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-tip-pza-999.
       acc-tip-pza-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-tip-pza (1)      .
       acc-tip-pza-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore zero non ammesso, a meno che non si  *
      *                  * sia in Up                                   *
      *                  *---------------------------------------------*
           if        w-tes-tip-pza (1)    not  = zero
                     go to acc-tip-pza-600.
           if        v-key                =    "UP  "
                     go to acc-tip-pza-600
           else      go to acc-tip-pza-100.
       acc-tip-pza-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Trattamento numero decimali prezzo          *
      *                  *---------------------------------------------*
           if        w-tes-tip-pza (1)    not  = 02
                     go to acc-tip-pza-620.
           move      w-tes-num-pro-dep    to   w-tes-dec-prz (1)      .
           perform   vis-dec-prz-000      thru vis-dec-prz-999        .
       acc-tip-pza-620.
      *                  *---------------------------------------------*
      *                  * Trattamento tabella prezzi e sconti         *
      *                  *---------------------------------------------*
           if        w-tes-tip-pza (1)    =    w-sav-tip-pza
                     go to acc-tip-pza-800.
           if        w-tes-tip-pza (1)    not  = 02
                     go to acc-tip-pza-800.
           move      zero                 to   w-tes-prz-pes (1, 1)   .
           move      zero                 to   w-tes-prz-pes (1, 2)   .
           move      zero                 to   w-tes-prz-pes (1, 3)   .
           move      zero                 to   w-tes-prz-pes (1, 4)   .
           move      zero                 to   w-tes-prz-pes (1, 5)   .
           move      zero                 to   w-tes-prz-pes (1, 6)   .
           move      zero                 to   w-tes-psr-pes (1, 1, 1).
           move      zero                 to   w-tes-psr-pes (1, 1, 2).
           move      zero                 to   w-tes-psr-pes (1, 1, 3).
           move      zero                 to   w-tes-psr-pes (1, 1, 4).
           move      zero                 to   w-tes-psr-pes (1, 1, 5).
           move      zero                 to   w-tes-psr-pes (1, 2, 1).
           move      zero                 to   w-tes-psr-pes (1, 2, 2).
           move      zero                 to   w-tes-psr-pes (1, 2, 3).
           move      zero                 to   w-tes-psr-pes (1, 2, 4).
           move      zero                 to   w-tes-psr-pes (1, 2, 5).
           move      zero                 to   w-tes-psr-pes (1, 3, 1).
           move      zero                 to   w-tes-psr-pes (1, 3, 2).
           move      zero                 to   w-tes-psr-pes (1, 3, 3).
           move      zero                 to   w-tes-psr-pes (1, 3, 4).
           move      zero                 to   w-tes-psr-pes (1, 3, 5).
           move      zero                 to   w-tes-psr-pes (1, 4, 1).
           move      zero                 to   w-tes-psr-pes (1, 4, 2).
           move      zero                 to   w-tes-psr-pes (1, 4, 3).
           move      zero                 to   w-tes-psr-pes (1, 4, 4).
           move      zero                 to   w-tes-psr-pes (1, 4, 5).
           move      zero                 to   w-tes-psr-pes (1, 5, 1).
           move      zero                 to   w-tes-psr-pes (1, 5, 2).
           move      zero                 to   w-tes-psr-pes (1, 5, 3).
           move      zero                 to   w-tes-psr-pes (1, 5, 4).
           move      zero                 to   w-tes-psr-pes (1, 5, 5).
           move      zero                 to   w-tes-psr-pes (1, 6, 1).
           move      zero                 to   w-tes-psr-pes (1, 6, 2).
           move      zero                 to   w-tes-psr-pes (1, 6, 3).
           move      zero                 to   w-tes-psr-pes (1, 6, 4).
           move      zero                 to   w-tes-psr-pes (1, 6, 5).
       acc-tip-pza-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-tip-pza-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-tip-pza-100.
       acc-tip-pza-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Tipo prezzo d'acquisto    *
      *    *-----------------------------------------------------------*
       vis-tip-pza-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
           if        w-tes-tip-mag        not  = 01
                     go to vis-tip-pza-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-pza-lun    to   v-car                  .
           move      w-exp-tip-pza-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-pza-tbl    to   v-txt                  .
           move      w-tes-tip-pza (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-pza-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Numero decimali prezzo       *
      *    *-----------------------------------------------------------*
       acc-dec-prz-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-tip-pza (1)    =    02
                     go to acc-dec-prz-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-dec-prz (1)    to   w-sav-dec-prz          .
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Solo se prima volta                     *
      *                      *-----------------------------------------*
           if        w-tes-fda-ndp (1)    not  = spaces
                     go to acc-dec-prz-100.
      *                      *-----------------------------------------*
      *                      * Preparazione                            *
      *                      *-----------------------------------------*
           move      w-tes-ndp-std (1)    to   w-tes-dec-prz (1)      .
       acc-dec-prz-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-dec-prz (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dec-prz-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dec-prz-999.
       acc-dec-prz-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-dec-prz (1)      .
       acc-dec-prz-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Il numero decimali prezzo non puo' essere   *
      *                  * maggiore di 2                               *
      *                  *---------------------------------------------*
           if        w-tes-dec-prz (1)    not  > 2
                     go to acc-dec-prz-600.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Il numero decimali non puo' essere maggiore di 2 !
      -              "      "             to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * Reimpostazione                          *
      *                      *-----------------------------------------*
           go to     acc-dec-prz-100.
       acc-dec-prz-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di accettazione numero decimali prezzo *
      *                  *---------------------------------------------*
           move      "#"                  to   w-tes-fda-ndp (1)      .
      *                  *---------------------------------------------*
      *                  * Se valore attuale come precedente : oltre   *
      *                  *---------------------------------------------*
           if        w-tes-dec-prz (1)    =    w-sav-dec-prz
                     go to acc-dec-prz-800.
      *                  *---------------------------------------------*
      *                  * Trattamento prezzo di acquisto              *
      *                  *---------------------------------------------*
           subtract  w-sav-dec-prz        from w-tes-dec-prz (1)
                                        giving w-acc-dec-prz-wdd      .
      *                      *-----------------------------------------*
      *                      * Ciclo di scansione tabella prezzi e     *
      *                      * sconti                                  *
      *                      *-----------------------------------------*
           move      zero                 to   w-acc-dec-prz-ctr      .
       acc-dec-prz-620.
           add       1                    to   w-acc-dec-prz-ctr      .
           if        w-acc-dec-prz-ctr    >    6
                     go to acc-dec-prz-800.
           if        w-tes-prz-pes
                    (1, w-acc-dec-prz-ctr)
                                          =    zero
                     go to acc-dec-prz-620.
           move      w-tes-prz-pes
                    (1, w-acc-dec-prz-ctr)
                                          to   w-acc-dec-prz-wpz      .
           if        w-acc-dec-prz-wdd    <    zero
                     go to acc-dec-prz-625.
           if        w-acc-dec-prz-wdd    =    1
                     multiply 10          by   w-acc-dec-prz-wpz
           else if   w-acc-dec-prz-wdd    =    2
                     multiply 100         by   w-acc-dec-prz-wpz      .
           go to     acc-dec-prz-630.
       acc-dec-prz-625.
           if        w-acc-dec-prz-wdd    =    -1
                     divide   10          into w-acc-dec-prz-wpz
                                               rounded
           else if   w-acc-dec-prz-wdd    =    -2
                     divide   100         into w-acc-dec-prz-wpz
                                               rounded                .
           go to     acc-dec-prz-630.
       acc-dec-prz-630.
           move      w-acc-dec-prz-wpz    to   w-tes-prz-pes
                                              (1, w-acc-dec-prz-ctr)  .
           go to     acc-dec-prz-620.
       acc-dec-prz-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dec-prz-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dec-prz-100.
       acc-dec-prz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Numero decimali prezzo            *
      *    *-----------------------------------------------------------*
       vis-dec-prz-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-dec-prz (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dec-prz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Numero decimali prezzo    *
      *    * standard il prodotto                                      *
      *    *-----------------------------------------------------------*
       vis-ndp-std-000.
      *              *-------------------------------------------------*
      *              * Prompt                                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      68                   to   v-pos                  .
           move      "Standard:"          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Valore                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      16                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      w-tes-ndp-std (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ndp-std-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Lotto di acquisto            *
      *    *-----------------------------------------------------------*
       acc-lot-acq-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-lot-acq-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           if        w-tes-snx-tum (1)    not  = "S" and
                     w-tes-snx-tum (1)    not  = "P"
                     move  w-tes-num-pro-deq
                                          to   v-dec
           else      move  w-tes-nde-tum (1)
                                          to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-lot-acq (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-lot-acq-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-lot-acq-999.
       acc-lot-acq-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-lot-acq (1)      .
       acc-lot-acq-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-lot-acq-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-lot-acq-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-lot-acq-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-lot-acq-100.
       acc-lot-acq-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Lotto di acquisto                 *
      *    *-----------------------------------------------------------*
       vis-lot-acq-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-lot-acq (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-lot-acq-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Tipo applicazione prezzi e   *
      *    * sconti                                                    *
      *    *-----------------------------------------------------------*
       acc-tap-pes-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-tap-pes (1)    to   w-sav-tap-pes          .
       acc-tap-pes-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tap-pes-lun    to   v-car                  .
           move      w-exp-tap-pes-num    to   v-ldt                  .
           move      "IQ#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tap-pes-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-tap-pes (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-tap-pes-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-tap-pes-999.
       acc-tap-pes-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-tap-pes (1)      .
       acc-tap-pes-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore zero non ammesso, a meno che non si  *
      *                  * sia in Up                                   *
      *                  *---------------------------------------------*
           if        w-tes-tap-pes (1)    not  = zero
                     go to acc-tap-pes-600.
           if        v-key                =    "UP  "
                     go to acc-tap-pes-600
           else      go to acc-tap-pes-100.
       acc-tap-pes-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore attuale come precedente : oltre   *
      *                  *---------------------------------------------*
           if        w-tes-tap-pes (1)    =    w-sav-tap-pes
                     go to acc-tap-pes-800.
      *                  *---------------------------------------------*
      *                  * Se valore attuale 02 : oltre                *
      *                  *---------------------------------------------*
           if        w-tes-tap-pes (1)    not  = 02
                     go to acc-tap-pes-800.
      *                  *---------------------------------------------*
      *                  * Normalizzazione elementi tabella prezzi e   *
      *                  * sconti oltre il primo                       *
      *                  *---------------------------------------------*
           move      zero                 to   w-tes-qta-pes (1, 2)   .
           move      zero                 to   w-tes-qta-pes (1, 3)   .
           move      zero                 to   w-tes-qta-pes (1, 4)   .
           move      zero                 to   w-tes-qta-pes (1, 5)   .
           move      zero                 to   w-tes-qta-pes (1, 6)   .
           move      zero                 to   w-tes-prz-pes (1, 2)   .
           move      zero                 to   w-tes-prz-pes (1, 3)   .
           move      zero                 to   w-tes-prz-pes (1, 4)   .
           move      zero                 to   w-tes-prz-pes (1, 5)   .
           move      zero                 to   w-tes-prz-pes (1, 6)   .
           move      zero                 to   w-tes-psr-pes (1, 2, 1).
           move      zero                 to   w-tes-psr-pes (1, 3, 1).
           move      zero                 to   w-tes-psr-pes (1, 4, 1).
           move      zero                 to   w-tes-psr-pes (1, 5, 1).
           move      zero                 to   w-tes-psr-pes (1, 6, 1).
           move      zero                 to   w-tes-psr-pes (1, 2, 2).
           move      zero                 to   w-tes-psr-pes (1, 3, 2).
           move      zero                 to   w-tes-psr-pes (1, 4, 2).
           move      zero                 to   w-tes-psr-pes (1, 5, 2).
           move      zero                 to   w-tes-psr-pes (1, 6, 2).
           move      zero                 to   w-tes-psr-pes (1, 2, 3).
           move      zero                 to   w-tes-psr-pes (1, 3, 3).
           move      zero                 to   w-tes-psr-pes (1, 4, 3).
           move      zero                 to   w-tes-psr-pes (1, 5, 3).
           move      zero                 to   w-tes-psr-pes (1, 6, 3).
           move      zero                 to   w-tes-psr-pes (1, 2, 4).
           move      zero                 to   w-tes-psr-pes (1, 3, 4).
           move      zero                 to   w-tes-psr-pes (1, 4, 4).
           move      zero                 to   w-tes-psr-pes (1, 5, 4).
           move      zero                 to   w-tes-psr-pes (1, 6, 4).
           move      zero                 to   w-tes-psr-pes (1, 2, 5).
           move      zero                 to   w-tes-psr-pes (1, 3, 5).
           move      zero                 to   w-tes-psr-pes (1, 4, 5).
           move      zero                 to   w-tes-psr-pes (1, 5, 5).
           move      zero                 to   w-tes-psr-pes (1, 6, 5).
       acc-tap-pes-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-tap-pes-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-tap-pes-100.
       acc-tap-pes-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Tipo applicazione prezzi  *
      *    * e sconti                                                  *
      *    *-----------------------------------------------------------*
       vis-tap-pes-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tap-pes-lun    to   v-car                  .
           move      w-exp-tap-pes-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tap-pes-tbl    to   v-txt                  .
           move      w-tes-tap-pes (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tap-pes-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Prezzo di acquisto indipen-  *
      *    * dente dalla quantita'                                     *
      *    *-----------------------------------------------------------*
       acc-prz-pes-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su tipo applicazione prezzi e      *
      *                      * sconti                                  *
      *                      *-----------------------------------------*
           if        w-tes-tap-pes (1)    not  = 01
                     go to acc-prz-pes-999.
      *                      *-----------------------------------------*
      *                      * Se tipo prezzo d'acquisto : stesso      *
      *                      * prezzo del listino base                 *
      *                      *-----------------------------------------*
           if        w-tes-tip-pza (1)    not  = 02
                     go to acc-prz-pes-100.
      *                          *-------------------------------------*
      *                          * Forzatura prezzo di listino base in *
      *                          * prezzo di acquisto                  *
      *                          *-------------------------------------*
           move      w-tes-num-pro-plb    to   w-tes-prz-pes (1, 1)   .
      *                          *-------------------------------------*
      *                          * Visualizzazione  prezzo d'acquisto  *
      *                          *-------------------------------------*
           perform   vis-prz-pes-000      thru vis-prz-pes-999        .
      *                          *-------------------------------------*
      *                          * Normalizzazione prezzo d'acquisto   *
      *                          *-------------------------------------*
           move      zero                 to   w-tes-prz-pes (1, 1)   .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     acc-prz-pes-999.
       acc-prz-pes-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      w-tes-dec-vlt (1)    to   v-dec                  .
           add       w-tes-dec-prz (1)    to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-prz-pes (1, 1) to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-prz-pes-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-prz-pes-999.
       acc-prz-pes-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-prz-pes (1, 1)   .
       acc-prz-pes-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-prz-pes-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-prz-pes-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-prz-pes-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-prz-pes-100.
       acc-prz-pes-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Prezzo di acquisto indipendente   *
      *    * dalla quantita'                                           *
      *    *-----------------------------------------------------------*
       vis-prz-pes-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo applicazione prezzi e sconti   *
      *                  *---------------------------------------------*
           if        w-tes-tap-pes (1)    not  = 01
                     go to vis-prz-pes-999.
      *              *-------------------------------------------------*
      *              * Editing                                         *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      w-tes-dec-vlt (1)    to   v-dec                  .
           add       w-tes-dec-prz (1)    to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      w-tes-prz-pes (1, 1) to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      v-edt                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-prz-pes-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Prezzo di acquisto medio          *
      *    *-----------------------------------------------------------*
       vis-prz-med-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo applicazione prezzi e sconti   *
      *                  *---------------------------------------------*
           if        w-tes-tap-pes (1)    not  = 01
                     go to vis-prz-med-999.
      *              *-------------------------------------------------*
      *              * Preparazione dei valori da visualizzare         *
      *              *-------------------------------------------------*
           if        w-tes-sgl-vlt (1)    not  = w-tes-vlt-std (1)
                     go to vis-prz-med-100.
      *                  *---------------------------------------------*
      *                  * Se valuta uguale a quella media             *
      *                  *---------------------------------------------*
           move      "Medio per     :"    to   v-alf                  .
           move      w-tes-num-pro-umi    to   v-alf (11 : 03)        .
           move      w-tes-prz-med (1)    to   v-num                  .
           go to     vis-prz-med-200.
       vis-prz-med-100.
      *                  *---------------------------------------------*
      *                  * Se valuta diversa da quella media           *
      *                  *---------------------------------------------*
           move      spaces               to   v-alf                  .
           move      zero                 to   v-num                  .
           go to     vis-prz-med-200.
       vis-prz-med-200.
      *              *-------------------------------------------------*
      *              * Prompt                                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      54                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazione campo                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      w-tes-dec-vlt (1)    to   v-dec                  .
           add       w-tes-dec-prz (1)    to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      70                   to   v-pos                  .
           move      v-edt                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-prz-med-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : % di sconto indipendenti dalla quan- *
      *    * tita'                                                     *
      *    *-----------------------------------------------------------*
       acc-psr-pes-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su tipo applicazione prezzi e      *
      *                      * sconti                                  *
      *                      *-----------------------------------------*
           if        w-tes-tap-pes (1)    not  = 01
                     go to acc-psr-pes-999.
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatore 01..05           *
      *                  *---------------------------------------------*
           move      01                   to   w-edt-psr-pes-c01      .
       acc-psr-pes-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BD"                to   v-edm                  .
           move      12                   to   v-lin                  .
           move      w-edt-psr-pes-c01    to   v-pos                  .
           if        w-edt-psr-pes-c01    >    5
                     subtract  5          from v-pos                  .
           multiply  06                   by   v-pos                  .
           add       24                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      "TAB "               to   v-pfk (10)             .
           move      w-tes-psr-pes
                    (1, 1, w-edt-psr-pes-c01)
                                          to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-psr-pes-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-psr-pes-999.
       acc-psr-pes-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-psr-pes
                                              (1, 1, w-edt-psr-pes-c01).
       acc-psr-pes-300.
      *              *-------------------------------------------------*
      *              * Se Return                                       *
      *              *-------------------------------------------------*
           if        v-key                not  = spaces
                     go to acc-psr-pes-320.
           move      "DOWN"               to   v-key                  .
       acc-psr-pes-320.
      *              *-------------------------------------------------*
      *              * Se Tab : come return                            *
      *              *-------------------------------------------------*
           if        v-key                not  = "TAB "
                     go to acc-psr-pes-400.
           move      spaces               to   v-key                  .
       acc-psr-pes-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-psr-pes-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-psr-pes-620.
      *                  *---------------------------------------------*
      *                  * Tasti funzione                              *
      *                  *---------------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-psr-pes-650
           else if   v-key                =    "DOWN"
                     go to acc-psr-pes-700
           else if   v-key                =    "DO  "
                     go to acc-psr-pes-800
           else if   v-key                =    "PRSC"
                     go to acc-psr-pes-999
           else if   v-key                =    "NXSC"
                     go to acc-psr-pes-999
           else      go to acc-psr-pes-710.
       acc-psr-pes-650.
      *              *-------------------------------------------------*
      *              * Se Up                                           *
      *              *-------------------------------------------------*
           if        w-edt-psr-pes-c01    =    1
                     go to acc-psr-pes-999.
           subtract  1                    from w-edt-psr-pes-c01      .
           go to     acc-psr-pes-100.
       acc-psr-pes-700.
      *              *-------------------------------------------------*
      *              * Se Down                                         *
      *              *-------------------------------------------------*
           go to     acc-psr-pes-999.
       acc-psr-pes-710.
      *              *-------------------------------------------------*
      *              * Se Return                                       *
      *              *-------------------------------------------------*
           if        w-edt-psr-pes-c01    =    05
                     go to acc-psr-pes-999.
      *                  *---------------------------------------------*
      *                  * Incremento contatore                        *
      *                  *---------------------------------------------*
           add       1                    to   w-edt-psr-pes-c01      .
           go to     acc-psr-pes-100.
       acc-psr-pes-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-psr-pes-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-psr-pes-100.
       acc-psr-pes-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : % di sconto                       *
      *    *-----------------------------------------------------------*
       vis-psr-pes-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo applicazione prezzi e sconti   *
      *                  *---------------------------------------------*
           if        w-tes-tap-pes (1)    not  = 01
                     go to vis-psr-pes-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-edt-psr-pes-edt      .
           move      zero                 to   w-edt-psr-pes-c01      .
       vis-psr-pes-200.
           add       1                    to   w-edt-psr-pes-c01      .
           if        w-edt-psr-pes-c01    >    5
                     go to vis-psr-pes-500.
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BD"                to   v-edm                  .
           move      w-tes-psr-pes 
                    (1, 1, w-edt-psr-pes-c01)
                                          to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-edt-psr-pes-per
                                              (w-edt-psr-pes-c01)     .
           go to     vis-psr-pes-200.
       vis-psr-pes-500.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-edt-psr-pes-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-psr-pes-999.
           exit.

      *    *===========================================================*
      *    * Accettazione tabella prezzi e sconti                      *
      *    *-----------------------------------------------------------*
       acc-tbl-pes-000.
      *              *-------------------------------------------------*
      *              * Test se tabella da accettare                    *
      *              *-------------------------------------------------*
           if        w-tes-tap-pes (1)    not  = 02
                     go to acc-tbl-pes-999.
      *              *-------------------------------------------------*
      *              * Inizializzazione contatore 1..06                *
      *              *-------------------------------------------------*
           move      1                    to   w-pes-ctr-rig          .
       acc-tbl-pes-020.
      *              *-------------------------------------------------*
      *              * Quantita'                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-pes-ctr-rig        =    1
                     go to acc-tbl-pes-050.
      *                  *---------------------------------------------*
      *                  * Aggiornamento contatore di comodo           *
      *                  *---------------------------------------------*
           move      w-pes-ctr-rig        to   w-pes-ctr-001          .
      *                  *---------------------------------------------*
      *                  * Decremento contatore di comodo              *
      *                  *---------------------------------------------*
           subtract  1                    from w-pes-ctr-001          .
      *                  *---------------------------------------------*
      *                  * Se valore precedente a zero : uscita senza  *
      *                  * accettazione                                *
      *                  *---------------------------------------------*
           if        w-tes-qta-pes
                    (1, w-pes-ctr-001)    not  = zero
                     go to acc-tbl-pes-040.
      *                      *-----------------------------------------*
      *                      * Test su stato della riga                *
      *                      *-----------------------------------------*
           move      w-pes-ctr-001        to   w-pes-ctr-vuo          .
           perform   acc-tbl-pes-850      thru acc-tbl-pes-859        .
           if        w-pes-flg-vuo        not  = spaces
                     go to acc-tbl-pes-720.
      *                      *-----------------------------------------*
      *                      * Prompt per quantita' vuota              *
      *                      *-----------------------------------------*
           subtract  1                    from w-pes-ctr-rig          .
           perform   pmt-qta-vuo-000      thru pmt-qta-vuo-999        .
           go to     acc-tbl-pes-720.
       acc-tbl-pes-040.
      *                  *---------------------------------------------*
      *                  * Test se la riga in corso di trattamento e'  *
      *                  * vuota                                       *
      *                  *---------------------------------------------*
           if        w-tes-qta-pes
                    (1, w-pes-ctr-rig)    not  = zero
                     go to acc-tbl-pes-050.
           move      w-pes-ctr-rig        to   w-pes-ctr-vuo          .
           perform   acc-tbl-pes-850      thru acc-tbl-pes-859        .
           if        w-pes-flg-vuo        not  = spaces
                     go to acc-tbl-pes-052.
       acc-tbl-pes-050.
      *                  *---------------------------------------------*
      *                  * Nessuna azione                              *
      *                  *---------------------------------------------*
       acc-tbl-pes-052.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-qta-pes 
                    (1, w-pes-ctr-rig)    to   w-pes-sav-qta          .
      *                  *---------------------------------------------*
      *                  * Accettazione quantita'                      *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      w-tes-num-pro-deq    to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "GB"                 to   v-edm                  .
           add       11
                     w-pes-ctr-rig      giving v-lin                  .
           move      31                   to   v-pos                  .
      *                      *-----------------------------------------*
      *                      * Tasti funzione                          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se oltre 06                    *
      *                          *-------------------------------------*
           if        w-pes-ctr-rig        >    06
                     go to acc-tbl-pes-720.
      *                          *-------------------------------------*
      *                          * Determinazione numero riga + 1      *
      *                          *-------------------------------------*
           add       1
                     w-pes-ctr-rig      giving w-pes-cts-rig          .
      *                          *-------------------------------------*
      *                          * Up   : sempre ammesso               *
      *                          *-------------------------------------*
           move      "UP  "               to   v-pfk (01)             .
      *                          *-------------------------------------*
      *                          * Down : sempre ammesso               *
      *                          *-------------------------------------*
           move      "DOWN"               to   v-pfk (02)             .
      *                          *-------------------------------------*
      *                          * Insr : sempre ammesso, a meno che   *
      *                          *        non si sia in una riga new   *
      *                          *        oppure manchi spazio per     *
      *                          *        l'inserimento                *
      *                          *-------------------------------------*
           if        w-tes-qta-pes (1, 06)
                                          not  = zero
                     go to acc-tbl-pes-060.
           if        w-pes-ctr-rig        =    06
                     go to acc-tbl-pes-060.
           if        w-pes-sav-qta        =    zero
                     go to acc-tbl-pes-060.
           move      "INSR"               to   v-pfk (04)             .
       acc-tbl-pes-060.
      *                          *-------------------------------------*
      *                          * Do   : sempre ammesso               *
      *                          *-------------------------------------*
           move      "DO  "               to   v-pfk (05)             .
      *                          *-------------------------------------*
      *                          * Remv : sempre ammesso               *
      *                          *-------------------------------------*
           move      "REMV"               to   v-pfk (06)             .
      *                          *-------------------------------------*
      *                          * Prsc : sempre ammesso               *
      *                          *-------------------------------------*
           move      "PRSC"               to   v-pfk (07)             .
      *                          *-------------------------------------*
      *                          * Nxsc : sempre ammesso               *
      *                          *-------------------------------------*
           move      "NXSC"               to   v-pfk (08)             .
      *                          *-------------------------------------*
      *                          * Back : sempre ammesso, a meno che   *
      *                          *        si sia gia' sulla prima riga *
      *                          *-------------------------------------*
           if        w-pes-ctr-rig        not  = 1
                     move  "BACK"         to   v-pfk (09)             .
      *                          *-------------------------------------*
      *                          * Tab  : sempre ammesso               *
      *                          *-------------------------------------*
           move      "TAB "               to   v-pfk (10)             .
      *                      *-----------------------------------------*
      *                      * Valore di accettazione                  *
      *                      *-----------------------------------------*
           move      w-tes-qta-pes 
                    (1, w-pes-ctr-rig)    to   v-num                  .
      *                      *-----------------------------------------*
      *                      * Richiamo subroutine di accettazione     *
      *                      *-----------------------------------------*
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *                  *---------------------------------------------*
      *                  * Se Return                                   *
      *                  *---------------------------------------------*
           if        v-key                =    spaces
                     go to acc-tbl-pes-300.
      *                  *---------------------------------------------*
      *                  * Se Exit                                     *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-tbl-pes-999.
      *                  *---------------------------------------------*
      *                  * Se Delt                                     *
      *                  *---------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-tbl-pes-999.
      *                  *---------------------------------------------*
      *                  * Se premuto un altro tasto funzione non deve *
      *                  * essere avvenuta variazione del campo        *
      *                  *---------------------------------------------*
           if        v-num                not  = w-pes-sav-qta
                     go to acc-tbl-pes-020.
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
           if        v-key                not  = "UP  "
                     go to acc-tbl-pes-080.
      *                      *-----------------------------------------*
      *                      * Eventuale eliminazione prompt quantita' *
      *                      *-----------------------------------------*
           if        w-tes-qta-pes
                    (1, w-pes-ctr-rig)    not  = zero
                     go to acc-tbl-pes-070.
           if        w-pes-ctr-rig        >    06
                     go to acc-tbl-pes-070.
      *                      *-----------------------------------------*
      *                      * Test su stato della riga                *
      *                      *-----------------------------------------*
           move      w-pes-ctr-rig        to   w-pes-ctr-vuo          .
           perform   acc-tbl-pes-850      thru acc-tbl-pes-859        .
           if        w-pes-flg-vuo        =    spaces
                     go to acc-tbl-pes-064.
      *                      *-----------------------------------------*
      *                      * Prompt per literal 'oltre'              *
      *                      *-----------------------------------------*
           perform   pmt-lit-olt-000      thru pmt-lit-olt-999        .
           go to     acc-tbl-pes-070.
       acc-tbl-pes-064.
      *                      *-----------------------------------------*
      *                      * Prompt per quantita' vuota              *
      *                      *-----------------------------------------*
           perform   pmt-qta-vuo-000      thru pmt-qta-vuo-999        .
       acc-tbl-pes-070.
      *                      *-----------------------------------------*
      *                      * Compattamento controllato               *
      *                      *-----------------------------------------*
           perform   acc-tbl-pes-800      thru acc-tbl-pes-809        .
      *                      *-----------------------------------------*
      *                      * Se su riga > 1 : a riga precedente      *
      *                      *-----------------------------------------*
           if        w-pes-ctr-rig        >    1
                     subtract  1          from w-pes-ctr-rig
                     go to     acc-tbl-pes-020.
      *                      *-----------------------------------------*
      *                      * Altrimenti : uscita con Up              *
      *                      *-----------------------------------------*
           move      "UP  "               to   v-key                  .
           go to     acc-tbl-pes-999.
       acc-tbl-pes-080.
      *                  *---------------------------------------------*
      *                  * Se Down                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "DOWN"
                     go to acc-tbl-pes-160.
       acc-tbl-pes-100.
      *                      *-----------------------------------------*
      *                      * Compattamento controllato               *
      *                      *-----------------------------------------*
           perform   acc-tbl-pes-800      thru acc-tbl-pes-809        .
      *                      *-----------------------------------------*
      *                      * Se la quantita' precedentemente salvata *
      *                      * era a zero                              *
      *                      *-----------------------------------------*
           if        w-pes-sav-qta        not  = zero
                     go to acc-tbl-pes-120.
      *                          *-------------------------------------*
      *                          * Se anche dopo il compattamento ci   *
      *                          * si trova su di una quantita' a ze-  *
      *                          * ro : uscita                         *
      *                          *-------------------------------------*
           if        w-tes-qta-pes
                    (1, w-pes-ctr-rig)    =    zero
                     go to acc-tbl-pes-700.
      *                          *-------------------------------------*
      *                          * Altrimenti si ricicla sulla stessa  *
      *                          * riga, in pratica sulla successiva   *
      *                          *-------------------------------------*
           go to     acc-tbl-pes-020.
       acc-tbl-pes-120.
      *                      *-----------------------------------------*
      *                      * Se la quantita' precedentemente salvata *
      *                      * era diversa da zero                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se si e' all'ultima riga : uscita   *
      *                          *-------------------------------------*
           if        w-pes-ctr-rig        =    06
                     go to acc-tbl-pes-700.
      *                      *-----------------------------------------*
      *                      * Altrimenti : a riga successiva          *
      *                      *-----------------------------------------*
           add       1                    to   w-pes-ctr-rig          .
           go to     acc-tbl-pes-020.
       acc-tbl-pes-160.
      *                  *---------------------------------------------*
      *                  * Se Insr                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "INSR"
                     go to acc-tbl-pes-180.
      *                      *-----------------------------------------*
      *                      * Inserimento riga in castelletto         *
      *                      *-----------------------------------------*
           perform   acc-tbl-pes-820      thru acc-tbl-pes-829        .
      *                      *-----------------------------------------*
      *                      * Riciclo sulla stessa riga               *
      *                      *-----------------------------------------*
           go to     acc-tbl-pes-020.
       acc-tbl-pes-180.
      *                  *---------------------------------------------*
      *                  * Se Do                                       *
      *                  *---------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-tbl-pes-200.
      *                      *-----------------------------------------*
      *                      * Compattamento controllato               *
      *                      *-----------------------------------------*
           perform   acc-tbl-pes-800      thru acc-tbl-pes-809        .
      *                      *-----------------------------------------*
      *                      * Controllo riga tabella prezzi e sconti  *
      *                      *-----------------------------------------*
           perform   acc-tbl-pes-980      thru acc-tbl-pes-989        .
           if        w-pes-flg-exi        not  = spaces
                     go to acc-tbl-pes-040.
      *                      *-----------------------------------------*
      *                      * Controllo globali                       *
      *                      *-----------------------------------------*
           perform   cnt-tdo-nok-000      thru cnt-tdo-nok-999        .
      *                      *-----------------------------------------*
      *                      * Test su esito controllo                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se positivo : uscita                *
      *                          *-------------------------------------*
           if        w-cnt-tdo-nok-flg    =    spaces
                     move  "S"            to   w-cnt-tus-acc-tes
                     go to acc-tbl-pes-999.
      *                          *-------------------------------------*
      *                          * Altrimenti si torna alla reimposta- *
      *                          * zione della stessa riga             *
      *                          *-------------------------------------*
           go to     acc-tbl-pes-040.
       acc-tbl-pes-200.
      *                  *---------------------------------------------*
      *                  * Se Remv                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "REMV"
                     go to acc-tbl-pes-220.
       acc-tbl-pes-210.
      *                      *-----------------------------------------*
      *                      * Compattamento non controllato           *
      *                      *-----------------------------------------*
           perform   acc-tbl-pes-810      thru acc-tbl-pes-819        .
      *                      *-----------------------------------------*
      *                      * Reimpostazione della stessa riga        *
      *                      *-----------------------------------------*
           go to     acc-tbl-pes-020.
       acc-tbl-pes-220.
      *                  *---------------------------------------------*
      *                  * Se Prsc                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "PRSC"
                     go to acc-tbl-pes-230.
      *                      *-----------------------------------------*
      *                      * Compattamento controllato               *
      *                      *-----------------------------------------*
           perform   acc-tbl-pes-800      thru acc-tbl-pes-809        .
      *                      *-----------------------------------------*
      *                      * Controllo tabella prezzi e sconti       *
      *                      *-----------------------------------------*
           perform   acc-tbl-pes-980      thru acc-tbl-pes-989        .
           if        w-pes-flg-exi        not  = spaces
                     go to acc-tbl-pes-040.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     acc-tbl-pes-999.
       acc-tbl-pes-230.
      *                  *---------------------------------------------*
      *                  * Se Nxsc                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "NXSC"
                     go to acc-tbl-pes-240.
      *                      *-----------------------------------------*
      *                      * Compattamento controllato               *
      *                      *-----------------------------------------*
           perform   acc-tbl-pes-800      thru acc-tbl-pes-809        .
      *                      *-----------------------------------------*
      *                      * Controllo tabella prezzi e sconti       *
      *                      *-----------------------------------------*
           perform   acc-tbl-pes-980      thru acc-tbl-pes-989        .
           if        w-pes-flg-exi        not  = spaces
                     go to acc-tbl-pes-040.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     acc-tbl-pes-999.
       acc-tbl-pes-240.
      *                  *---------------------------------------------*
      *                  * Se Back                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "BACK"
                     go to acc-tbl-pes-260.
      *                      *-----------------------------------------*
      *                      * Compattamento controllato               *
      *                      *-----------------------------------------*
           perform   acc-tbl-pes-800      thru acc-tbl-pes-809        .
      *                      *-----------------------------------------*
      *                      * Ad impostazione della prima riga        *
      *                      *-----------------------------------------*
           move      1                    to   w-pes-ctr-rig          .
           go to     acc-tbl-pes-020.
       acc-tbl-pes-260.
      *                  *---------------------------------------------*
      *                  * Se Tab                                      *
      *                  *---------------------------------------------*
           if        v-key                not  = "TAB "
                     go to acc-tbl-pes-400.
      *                      *-----------------------------------------*
      *                      * Compattamento controllato               *
      *                      *-----------------------------------------*
           perform   acc-tbl-pes-800      thru acc-tbl-pes-809        .
      *                      *-----------------------------------------*
      *                      * Se la quantita' precedentemente salvata *
      *                      * era a zero                              *
      *                      *-----------------------------------------*
           if        w-pes-sav-qta        not  = zero
                     go to acc-tbl-pes-280.
      *                          *-------------------------------------*
      *                          * Se anche dopo il compattamento ci   *
      *                          * si trova su di una quantita' a ze-  *
      *                          * ro : uscita                         *
      *                          *-------------------------------------*
           if        w-tes-qta-pes
                    (1, w-pes-ctr-rig)    =    zero
                     go to acc-tbl-pes-700.
       acc-tbl-pes-280.
      *                      *-----------------------------------------*
      *                      * Se lo quantita' precedentemente salvata *
      *                      * era diversa da zero ci si posiziona     *
      *                      * dopo l'ultima riga                      *
      *                      *-----------------------------------------*
           add       1                    to   w-pes-ctr-rig          .
      *                          *-------------------------------------*
      *                          * Se sono presenti tutte le righe :   *
      *                          * uscita                              *
      *                          *-------------------------------------*
           if        w-pes-ctr-rig        >    06
                     go to acc-tbl-pes-700.
      *                          *-------------------------------------*
      *                          * Se la riga e' vuota si va ad impo-  *
      *                          * starla                              *
      *                          *-------------------------------------*
           if        w-tes-qta-pes
                    (1, w-pes-ctr-rig)    =    zero
                     go to acc-tbl-pes-020.
      *                          *-------------------------------------*
      *                          * Altrimenti si ricicla per trovare   *
      *                          * una riga vuota                      *
      *                          *-------------------------------------*
           go to     acc-tbl-pes-280.
       acc-tbl-pes-300.
      *                  *---------------------------------------------*
      *                  * Se Return                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Memorizzazione valore impostato         *
      *                      *-----------------------------------------*
           move      v-num                to   w-tes-qta-pes
                                              (1, w-pes-ctr-rig)      .
      *                  *---------------------------------------------*
      *                  * Controlli impostazione quantita'            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se quantita' a zero : controllo del va- *
      *                      * lore precedente                         *
      *                      *-----------------------------------------*
           if        w-tes-qta-pes
                    (1, w-pes-ctr-rig)    >    zero
                     go to acc-tbl-pes-400.
      *                      *-----------------------------------------*
      *                      * Test se prima riga                      *
      *                      *-----------------------------------------*
           if        w-pes-ctr-rig        =    1
                     go to acc-tbl-pes-320.
      *                      *-----------------------------------------*
      *                      * Aggiornamento contatore di comodo       *
      *                      *-----------------------------------------*
           move      w-pes-ctr-rig        to   w-pes-ctr-001          .
      *                      *-----------------------------------------*
      *                      * Decremento contatore di comodo          *
      *                      *-----------------------------------------*
           subtract  1                    from w-pes-ctr-001          .
      *                      *-----------------------------------------*
      *                      * Se valore precedente a zero : uscita    *
      *                      * con pulizia del prompt                  *
      *                      *-----------------------------------------*
           if        w-tes-qta-pes
                    (1, w-pes-ctr-001)    =    zero
                     go to acc-tbl-pes-710.
       acc-tbl-pes-320.
      *                      *-----------------------------------------*
      *                      * Test su riga successiva                 *
      *                      *-----------------------------------------*
           if        w-pes-ctr-rig        =    06
                     go to acc-tbl-pes-400.
      *                      *-----------------------------------------*
      *                      * Aggiornamento contatore di comodo       *
      *                      *-----------------------------------------*
           move      w-pes-ctr-rig        to   w-pes-ctr-001          .
      *                      *-----------------------------------------*
      *                      * Decremento contatore di comodo          *
      *                      *-----------------------------------------*
           add       1                    to   w-pes-ctr-001          .
      *                      *-----------------------------------------*
      *                      * Se valore successivo diverso da zero :  *
      *                      * come con tasto 'Down'                   *
      *                      *-----------------------------------------*
           if        w-tes-qta-pes
                    (1, w-pes-ctr-001)    >    zero
                     go to acc-tbl-pes-100.
      *                      *-----------------------------------------*
      *                      * Prompt per literal 'oltre'              *
      *                      *-----------------------------------------*
           perform   pmt-lit-olt-000      thru pmt-lit-olt-999        .
       acc-tbl-pes-400.
      *              *-------------------------------------------------*
      *              * Accettazione prezzo d'acquisto                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se tipo prezzo d'acquisto : stesso      *
      *                      * prezzo del listino base                 *
      *                      *-----------------------------------------*
           if        w-tes-tip-pza (1)    not  = 02
                     go to acc-tbl-pes-410.
      *                          *-------------------------------------*
      *                          * Forzatura prezzo di listino base in *
      *                          * prezzo di acquisto                  *
      *                          *-------------------------------------*
           move      w-tes-num-pro-plb    to   w-tes-prz-pes
                                              (1, w-pes-ctr-rig)      .
      *                          *-------------------------------------*
      *                          * Visualizzazione  prezzo d'acquisto  *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Editing                         *
      *                              *---------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      w-tes-dec-vlt (1)    to   v-dec                  .
           add       w-tes-dec-prz (1)    to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "BG"                 to   v-edm                  .
           move      w-tes-prz-pes
                    (1, w-pes-ctr-rig)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Visualizzazione                 *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           add       11
                     w-pes-ctr-rig      giving v-lin                  .
           move      43                   to   v-pos                  .
           move      v-edt                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Normalizzazione prezzo d'acquisto   *
      *                          *-------------------------------------*
           move      zero                 to   w-tes-prz-pes
                                              (1, w-pes-ctr-rig)      .
      *                          *-------------------------------------*
      *                          * A prossima accettazione             *
      *                          *-------------------------------------*
           go to     acc-tbl-pes-500.
       acc-tbl-pes-410.
      *                  *---------------------------------------------*
      *                  * Accettazione valore                         *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      w-tes-dec-vlt (1)    to   v-dec                  .
           add       w-tes-dec-prz (1)    to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "BG"                 to   v-edm                  .
           add       11
                     w-pes-ctr-rig      giving v-lin                  .
           move      43                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "REMV"               to   v-pfk (06)             .
           move      w-tes-prz-pes
                    (1, w-pes-ctr-rig)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *                  *---------------------------------------------*
      *                  * Se 'Remv'                                   *
      *                  *---------------------------------------------*
           if        v-key                =    "REMV"
                     go to acc-tbl-pes-210.
      *                  *---------------------------------------------*
      *                  * Se 'Exit'                                   *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT"
                     move  spaces         to   v-key
                     go to acc-tbl-pes-020.
       acc-tbl-pes-420.
      *                  *---------------------------------------------*
      *                  * Valore impostato in campo di destinazione   *
      *                  *---------------------------------------------*
           move      v-num                to   w-tes-prz-pes
                                              (1, w-pes-ctr-rig)      .
       acc-tbl-pes-440.
      *                  *---------------------------------------------*
      *                  * Controllo valore impostato                  *
      *                  *---------------------------------------------*
       acc-tbl-pes-460.
      *                  *---------------------------------------------*
      *                  * Dipendenze dall'impostazione                *
      *                  *---------------------------------------------*
       acc-tbl-pes-480.
      *                  *---------------------------------------------*
      *                  * Se Do                                       *
      *                  *---------------------------------------------*
           if        v-key                =    "DO  "
                     go to acc-tbl-pes-580.
       acc-tbl-pes-500.
      *              *-------------------------------------------------*
      *              * Accettazione percentuali di sconto              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatore 01.05            *
      *                  *---------------------------------------------*
           move      01                   to   w-edt-psr-pes-c01      .
       acc-tbl-pes-510.
      *                  *---------------------------------------------*
      *                  * Accettazione valore                         *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           add       11
                     w-pes-ctr-rig      giving v-lin                  .
           move      w-edt-psr-pes-c01    to   v-pos                  .
           if        w-edt-psr-pes-c01    >    5
                     subtract  5          from v-pos                  .
           multiply  05                   by   v-pos                  .
           add       51                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "REMV"               to   v-pfk (06)             .
           move      "TAB "               to   v-pfk (10)             .
           move      w-tes-psr-pes
                    (1, w-pes-ctr-rig, w-edt-psr-pes-c01)
                                          to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *                  *---------------------------------------------*
      *                  * Se 'Remv'                                   *
      *                  *---------------------------------------------*
           if        v-key                =    "REMV"
                     go to acc-tbl-pes-210.
      *                  *---------------------------------------------*
      *                  * Se 'Exit'                                   *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT"
                     move  spaces         to   v-key
                     go to acc-tbl-pes-020.
       acc-tbl-pes-520.
      *                  *---------------------------------------------*
      *                  * Valore impostato in campo di destinazione   *
      *                  *---------------------------------------------*
           move      v-num                to   w-tes-psr-pes
                                              (1, w-pes-ctr-rig, 
                                                  w-edt-psr-pes-c01)  .
       acc-tbl-pes-530.
      *                  *---------------------------------------------*
      *                  * Se Return                                   *
      *                  *---------------------------------------------*
           if        v-key                not  = spaces
                     go to acc-tbl-pes-532.
           move      "DOWN"               to   v-key                  .
       acc-tbl-pes-532.
      *                  *---------------------------------------------*
      *                  * Se Tab : come return                        *
      *                  *---------------------------------------------*
           if        v-key                not  = "TAB "
                     go to acc-tbl-pes-540.
           move      spaces               to   v-key                  .
       acc-tbl-pes-540.
      *                  *---------------------------------------------*
      *                  * Controllo valore impostato                  *
      *                  *---------------------------------------------*
       acc-tbl-pes-560.
      *                  *---------------------------------------------*
      *                  * Dipendenze dall'impostazione                *
      *                  *---------------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-tbl-pes-562
           else if   v-key                =    "DOWN"
                     go to acc-tbl-pes-564
           else      go to acc-tbl-pes-566.
       acc-tbl-pes-562.
      *                      *-----------------------------------------*
      *                      * Se Up                                   *
      *                      *-----------------------------------------*
           if        w-edt-psr-pes-c01    =    1
                     go to acc-tbl-pes-400.
           subtract  1                    from w-edt-psr-pes-c01      .
           go to     acc-tbl-pes-510.
       acc-tbl-pes-564.
      *                      *-----------------------------------------*
      *                      * Se Down : oltre                         *
      *                      *-----------------------------------------*
           go to     acc-tbl-pes-580.
       acc-tbl-pes-566.
      *                      *-----------------------------------------*
      *                      * Se Return                               *
      *                      *-----------------------------------------*
           if        w-edt-psr-pes-c01    =    05
                     go to acc-tbl-pes-580.
      *                  *---------------------------------------------*
      *                  * Incremento contatore                        *
      *                  *---------------------------------------------*
           add       1                    to   w-edt-psr-pes-c01      .
           go to     acc-tbl-pes-510.
       acc-tbl-pes-580.
      *              *-------------------------------------------------*
      *              * Incremento numero riga                          *
      *              *-------------------------------------------------*
           add       1                    to   w-pes-ctr-rig          .
      *                  *---------------------------------------------*
      *                  * Se fine righe : uscita                      *
      *                  *---------------------------------------------*
           if        w-pes-ctr-rig        >    06
                     go to acc-tbl-pes-720.
      *                  *---------------------------------------------*
      *                  * Altrimenti : a prossimo elemento            *
      *                  *---------------------------------------------*
           go to     acc-tbl-pes-020.
       acc-tbl-pes-700.
      *              *-------------------------------------------------*
      *              * Eventuale eliminazione prompt per quantita'     *
      *              *-------------------------------------------------*
           if        w-tes-qta-pes
                    (1, w-pes-ctr-rig)    not  = zero
                     go to acc-tbl-pes-720.
           if        w-pes-ctr-rig        >    06
                     go to acc-tbl-pes-720.
       acc-tbl-pes-710.
      *                  *---------------------------------------------*
      *                  * Test su stato della riga                    *
      *                  *---------------------------------------------*
           move      w-pes-ctr-rig        to   w-pes-ctr-vuo          .
           perform   acc-tbl-pes-850      thru acc-tbl-pes-859        .
           if        w-pes-flg-vuo        =    spaces
                     go to acc-tbl-pes-715.
      *                      *-----------------------------------------*
      *                      * Prompt per literal 'oltre'              *
      *                      *-----------------------------------------*
           perform   pmt-lit-olt-000      thru pmt-lit-olt-999        .
           go to     acc-tbl-pes-720.
       acc-tbl-pes-715.
      *                      *-----------------------------------------*
      *                      * Prompt per quantita' vuota              *
      *                      *-----------------------------------------*
           perform   pmt-qta-vuo-000      thru pmt-qta-vuo-999        .
       acc-tbl-pes-720.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-tbl-pes-999.
       acc-tbl-pes-800.
      *              *=================================================*
      *              * Subroutine interna di compattamento controllato *
      *              * tabella : solo se la quantita' e' a zero        *
      *              *-------------------------------------------------*
           if        w-tes-qta-pes
                    (1, w-pes-ctr-rig)    not  = zero
                     go to acc-tbl-pes-809.
           if        w-pes-ctr-rig        =    06
                     go to acc-tbl-pes-809.
           if        w-tes-qta-pes
                    (1, w-pes-cts-rig)    =    zero
                     go to acc-tbl-pes-809.
           perform   acc-tbl-pes-810      thru acc-tbl-pes-819        .
       acc-tbl-pes-809.
           exit.
       acc-tbl-pes-810.
      *              *=================================================*
      *              * Subroutine interna di compattamento righe della *
      *              * tabella escludendo la riga w-pes-ctr-rig        *
      *              *-------------------------------------------------*
           move      w-pes-ctr-rig        to   w-pes-ctx-rig          .
           add       1
                     w-pes-ctx-rig    giving   w-pes-cty-rig          .
       acc-tbl-pes-811.
           if        w-pes-ctx-rig        =    06
                     move  zero           to   w-tes-qta-pes
                                              (1, 06)
                     move  zero           to   w-tes-prz-pes
                                              (1, 06)
                     move  zero           to   w-tes-psr-pes
                                              (1, 06, 01)
                     move  zero           to   w-tes-psr-pes
                                              (1, 06, 02)
                     move  zero           to   w-tes-psr-pes
                                              (1, 06, 03)
                     move  zero           to   w-tes-psr-pes
                                              (1, 06, 04)
                     move  zero           to   w-tes-psr-pes
                                              (1, 06, 05)
                     go to acc-tbl-pes-812.
           move      w-tes-ele-pes
                    (1, w-pes-cty-rig)    to   w-tes-ele-pes
                                              (1, w-pes-ctx-rig)      .
           add       1                    to   w-pes-ctx-rig          .
           add       1                    to   w-pes-cty-rig          .
           go to     acc-tbl-pes-811.
       acc-tbl-pes-812.
           move      w-pes-ctr-rig        to   w-pes-ctx-rig          .
           add       11                   to   w-pes-ctx-rig          .
           add       1
                     w-pes-ctx-rig    giving   w-pes-cty-rig          .
       acc-tbl-pes-813.
           if        w-pes-ctx-rig        =    17
                     go to acc-tbl-pes-814.
      *              *-------------------------------------------------*
      *              * Taglio della linea video                        *
      *              *-------------------------------------------------*
           move      "FL"                 to   v-ope                  .
           move      w-pes-cty-rig        to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazione della linea video               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      w-pes-ctx-rig        to   v-lin                  .
           move      01                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazione della linea video vuota         *
      *              *-------------------------------------------------*
           if        v-alf                =    w-pes-imm-lin
                     go to acc-tbl-pes-815.
           add       1                    to   w-pes-ctx-rig          .
           add       1                    to   w-pes-cty-rig          .
           go to     acc-tbl-pes-813.
       acc-tbl-pes-814.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-pes-imm-lin        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-tbl-pes-815.
       acc-tbl-pes-819.
           exit.
       acc-tbl-pes-820.
      *              *=================================================*
      *              * Subroutine interna di inserimento della riga    *
      *              * numero w-pes-ctr-rig nella tabella scaglioni    *
      *              *-------------------------------------------------*
           move      05                   to   w-pes-cty-rig          .
       acc-tbl-pes-821.
           if        w-tes-qta-pes
                    (1, w-pes-cty-rig)    =    zero  and
                     w-tes-prz-pes
                    (1, w-pes-cty-rig)    =    zero  and
                     w-tes-psr-pes
                    (1, w-pes-cty-rig, 01)
                                          =    zero  and
                     w-tes-psr-pes
                    (1, w-pes-cty-rig, 02)
                                          =    zero  and
                     w-tes-psr-pes
                    (1, w-pes-cty-rig, 03)
                                          =    zero  and
                     w-tes-psr-pes
                    (1, w-pes-cty-rig, 04)
                                          =    zero  and
                     w-tes-psr-pes
                    (1, w-pes-cty-rig, 05)
                                          =    zero
                     subtract  1          from w-pes-cty-rig
                     go to     acc-tbl-pes-821.
           move      w-pes-cty-rig        to   w-pes-ctx-rig          .
           add       1                    to   w-pes-cty-rig          .
       acc-tbl-pes-822.
           move      w-tes-ele-pes
                    (1, w-pes-ctx-rig)    to   w-tes-ele-pes
                                              (1, w-pes-cty-rig)      .
           if        w-pes-ctx-rig        not  = w-pes-ctr-rig
                     subtract  1          from w-pes-ctx-rig
                     subtract  1          from w-pes-cty-rig
                     go to acc-tbl-pes-822.
           move      zero                 to   w-tes-qta-pes
                                              (1, w-pes-ctr-rig)      .
           move      zero                 to   w-tes-prz-pes
                                              (1, w-pes-ctr-rig)      .
           move      zero                 to   w-tes-psr-pes
                                              (1, w-pes-ctr-rig, 01)  .
           move      zero                 to   w-tes-psr-pes
                                              (1, w-pes-ctr-rig, 02)  .
           move      zero                 to   w-tes-psr-pes
                                              (1, w-pes-ctr-rig, 03)  .
           move      zero                 to   w-tes-psr-pes
                                              (1, w-pes-ctr-rig, 04)  .
           move      zero                 to   w-tes-psr-pes
                                              (1, w-pes-ctr-rig, 05)  .
           move      06                   to   w-pes-cty-rig          .
       acc-tbl-pes-823.
           if        w-pes-cty-rig        =    w-pes-ctr-rig
                     go to     acc-tbl-pes-824.
           if        w-tes-qta-pes
                    (1, w-pes-cty-rig)    =    zero  and
                     w-tes-prz-pes
                    (1, w-pes-cty-rig)    =    zero  and
                     w-tes-psr-pes
                    (1, w-pes-cty-rig, 01)
                                          =    zero  and
                     w-tes-psr-pes
                    (1, w-pes-cty-rig, 02)
                                          =    zero  and
                     w-tes-psr-pes
                    (1, w-pes-cty-rig, 03)
                                          =    zero  and
                     w-tes-psr-pes
                    (1, w-pes-cty-rig, 04)
                                          =    zero  and
                     w-tes-psr-pes
                    (1, w-pes-cty-rig, 05)
                                          =    zero
                     subtract  1          from w-pes-cty-rig
                     go to     acc-tbl-pes-823.
       acc-tbl-pes-824.
           move      w-pes-cty-rig        to   w-pes-ctx-rig          .
           add       1                    to   w-pes-cty-rig          .
       acc-tbl-pes-825.
      *              *-------------------------------------------------*
      *              * Taglio della linea video                        *
      *              *-------------------------------------------------*
           move      "FL"                 to   v-ope                  .
           add       11
                     w-pes-ctx-rig      giving v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazione della linea video               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           add       11
                     w-pes-cty-rig      giving v-lin                  .
           move      01                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           if        w-pes-ctx-rig        not  = w-pes-ctr-rig
                     subtract  1          from w-pes-ctx-rig
                     subtract  1          from w-pes-cty-rig
                     go to acc-tbl-pes-825.
      *              *-------------------------------------------------*
      *              * Visualizzazione della linea video vuota         *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           add       11
                     w-pes-ctr-rig      giving v-lin                  .
           move      01                   to   v-pos                  .
           move      w-pes-imm-lin        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-tbl-pes-829.
           exit.
       acc-tbl-pes-850.
      *              *=================================================*
      *              * Subroutine interna di determinazione stato del- *
      *              * la riga in corso di trattamento                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione status di uscita            *
      *                  *---------------------------------------------*
           move      spaces               to   w-pes-flg-vuo          .
      *                  *---------------------------------------------*
      *                  * Controllo se la riga e' vuota               *
      *                  *---------------------------------------------*
           if        w-tes-prz-pes 
                    (1, w-pes-ctr-vuo)    =    zero and
                     w-tes-psr-pes 
                    (1, w-pes-ctr-vuo, 01)
                                          =    zero and
                     w-tes-psr-pes 
                    (1, w-pes-ctr-vuo, 02)
                                          =    zero and
                     w-tes-psr-pes 
                    (1, w-pes-ctr-vuo, 03)
                                          =    zero and
                     w-tes-psr-pes 
                    (1, w-pes-ctr-vuo, 04)
                                          =    zero and
                     w-tes-psr-pes 
                    (1, w-pes-ctr-vuo, 05)
                                          =    zero
                     go to acc-tbl-pes-859.
      *                      *-----------------------------------------*
      *                      * Flag di riga non vuota                  *
      *                      *-----------------------------------------*
           move      "#"                  to   w-pes-flg-vuo          .
       acc-tbl-pes-859.
           exit.
       acc-tbl-pes-980.
      *              *=================================================*
      *              * Subroutine interna di esecuzione operazioni per *
      *              * fine impostazione riga tabella prezzi e sconti  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione status di uscita            *
      *                  *---------------------------------------------*
           move      spaces               to   w-pes-flg-exi          .
       acc-tbl-pes-989.
           exit.
       acc-tbl-pes-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione tabella prezzi e sconti                   *
      *    *-----------------------------------------------------------*
       vis-tbl-pes-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
           if        w-tes-tap-pes (1)    not  = 02
                     go to vis-tbl-pes-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatore 1..06            *
      *                  *---------------------------------------------*
           move      zero                 to   w-pes-ctr-rig          .
      *                  *---------------------------------------------*
      *                  * Ciclo 1..06                                 *
      *                  *---------------------------------------------*
       vis-tbl-pes-100.
           add       1                    to   w-pes-ctr-rig          .
           if        w-pes-ctr-rig        >    06
                     go to  vis-tbl-pes-999.
           if        w-tes-qta-pes 
                    (1, w-pes-ctr-rig)    =    zero and
                     w-tes-prz-pes 
                    (1, w-pes-ctr-rig)    =    zero and
                     w-tes-psr-pes 
                    (1, w-pes-ctr-rig, 01)
                                          =    zero and
                     w-tes-psr-pes 
                    (1, w-pes-ctr-rig, 02)
                                          =    zero and
                     w-tes-psr-pes 
                    (1, w-pes-ctr-rig, 03)
                                          =    zero and
                     w-tes-psr-pes 
                    (1, w-pes-ctr-rig, 04)
                                          =    zero and
                     w-tes-psr-pes 
                    (1, w-pes-ctr-rig, 05)
                                          =    zero
                     go to  vis-tbl-pes-999.
      *                      *-----------------------------------------*
      *                      * Quantita'                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se valore a zero : literal 'oltre'  *
      *                          *-------------------------------------*
           if        w-tes-qta-pes
                    (1, w-pes-ctr-rig)    =    zero
                     go to vis-tbl-pes-120.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      w-tes-num-pro-deq    to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "GB"                 to   v-edm                  .
           add       11
                     w-pes-ctr-rig      giving v-lin                  .
           move      31                   to   v-pos                  .
           move      w-tes-qta-pes
                    (1, w-pes-ctr-rig)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     vis-tbl-pes-150.
       vis-tbl-pes-120.
      *                      *-----------------------------------------*
      *                      * Prompt per literal 'oltre'              *
      *                      *-----------------------------------------*
           perform   pmt-lit-olt-000      thru pmt-lit-olt-999        .
           go to     vis-tbl-pes-150.
       vis-tbl-pes-150.
      *                      *-----------------------------------------*
      *                      * Prezzo di acquisto                      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Editing                             *
      *                          *-------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      w-tes-dec-vlt (1)    to   v-dec                  .
           add       w-tes-dec-prz (1)    to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "BG"                 to   v-edm                  .
           move      w-tes-prz-pes
                    (1, w-pes-ctr-rig)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Visualizzazione                     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           add       11
                     w-pes-ctr-rig      giving v-lin                  .
           move      43                   to   v-pos                  .
           move      v-edt                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * 5 percentuali di provvigioni            *
      *                      *-----------------------------------------*
           move      spaces               to   w-edt-psr-pes-edt      .
           move      zero                 to   w-edt-psr-pes-c01      .
       vis-tbl-pes-200.
           add       1                    to   w-edt-psr-pes-c01      .
           if        w-edt-psr-pes-c01    >    05
                     go to vis-tbl-pes-500.
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           move      w-tes-psr-pes 
                    (1, w-pes-ctr-rig, w-edt-psr-pes-c01)
                                          to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-edt-psr-pes-per
                                              (w-edt-psr-pes-c01)     .
           go to     vis-tbl-pes-200.
       vis-tbl-pes-500.
      *                      *-----------------------------------------*
      *                      * Stringa editata                         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           add       11
                     w-pes-ctr-rig      giving v-lin                  .
           move      56                   to   v-pos                  .
           move      w-edt-psr-pes-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-tbl-pes-800.
      *                      *-----------------------------------------*
      *                      * Riciclo a riga castelletto successiva   *
      *                      *-----------------------------------------*
           go to     vis-tbl-pes-100.
       vis-tbl-pes-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt per quantita' a vuoto              *
      *    *-----------------------------------------------------------*
       pmt-qta-vuo-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      11                   to   v-car                  .
           add       11
                     w-pes-ctr-rig      giving v-lin                  .
           move      31                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-qta-vuo-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt 'oltre'                            *
      *    *-----------------------------------------------------------*
       pmt-lit-olt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      11                   to   v-car                  .
           add       11
                     w-pes-ctr-rig      giving v-lin                  .
           move      31                   to   v-pos                  .
           move      "  Oltre    "        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-lit-olt-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Data aggiornamento prezzo    *
      *    *-----------------------------------------------------------*
       acc-uda-pes-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        w-tes-uda-pes (1)    not  = zero
                     go to acc-uda-pes-100.
      *                  *---------------------------------------------*
      *                  * Data di sistema da segreteria               *
      *                  *---------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-tes-uda-pes (1)      .
       acc-uda-pes-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           if        w-tes-tap-pes (1)    =    01
                     move  14             to   v-lin
           else      move  19             to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-uda-pes (1)    to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-uda-pes-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-uda-pes-999.
       acc-uda-pes-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   w-tes-uda-pes (1)      .
       acc-uda-pes-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-uda-pes-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-uda-pes-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-uda-pes-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-uda-pes-100.
       acc-uda-pes-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Data aggiornamento                *
      *    *-----------------------------------------------------------*
       vis-uda-pes-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           if        w-tes-tap-pes (1)    =    01
                     move  14             to   v-lin
           else      move  19             to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-uda-pes (1)    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-uda-pes-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Tempo di consegna            *
      *    *-----------------------------------------------------------*
       acc-tmp-cns-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tmp-cns-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           if        w-tes-tap-pes (1)    =    01
                     move  16             to   v-lin
           else      move  21             to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-tmp-cns (1)    to   v-num                  .
           if        w-prs-mde-tdc        =    1
                     divide   7           into v-num
                                               rounded                .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-tmp-cns-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-tmp-cns-999.
       acc-tmp-cns-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-tmp-cns (1)      .
           if        w-prs-mde-tdc        =    1
                     multiply  7          by   w-tes-tmp-cns (1)      .
       acc-tmp-cns-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tmp-cns-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tmp-cns-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-tmp-cns-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-tmp-cns-100.
       acc-tmp-cns-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tempo di consegna                 *
      *    *-----------------------------------------------------------*
       vis-tmp-cns-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           if        w-tes-tap-pes (1)    =    01
                     move  16             to   v-lin
           else      move  21             to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-tmp-cns (1)    to   v-num                  .
           if        w-prs-mde-tdc        =    1
                     divide   7           into v-num
                                               rounded                .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tmp-cns-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tempo di consegna medio           *
      *    *-----------------------------------------------------------*
       vis-tmp-med-000.
      *              *-------------------------------------------------*
      *              * Se valore a zero : uscita                       *
      *              *-------------------------------------------------*
           if        w-tes-tmp-med (1)    =    zero
                     go to vis-tmp-med-999.
      *              *-------------------------------------------------*
      *              * Prompt                                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      07                   to   v-car                  .
           if        w-tes-tap-pes (1)    =    01
                     move  16             to   v-lin
           else      move  21             to   v-lin                  .
           move      62                   to   v-pos                  .
           move      "Medio :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazione campo                           *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           if        w-tes-tap-pes (1)    =    01
                     move  16             to   v-lin
           else      move  21             to   v-lin                  .
           move      70                   to   v-pos                  .
           move      w-tes-tmp-med (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tmp-med-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Sigla valuta per legame va-  *
      *    * lutario                                                   *
      *    *-----------------------------------------------------------*
       acc-lgv-vlt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-lgv-vlt (1)    to   w-sav-lgv-vlt          .
       acc-lgv-vlt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-zvl-ope      .
           move      w-tes-lgv-vlt (1)    to   w-cod-cod-zvl-cod      .
           move      10                   to   w-cod-cod-zvl-lin      .
           move      30                   to   w-cod-cod-zvl-pos      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           perform   cod-cod-zvl-cll-000  thru cod-cod-zvl-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-zvl-foi-000  thru cod-cod-zvl-foi-999    .
       acc-lgv-vlt-110.
           perform   cod-cod-zvl-cll-000  thru cod-cod-zvl-cll-999    .
           if        w-cod-cod-zvl-ope    =    "F+"
                     go to acc-lgv-vlt-115.
           if        w-cod-cod-zvl-ope    =    "AC"
                     go to acc-lgv-vlt-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-lgv-vlt-115.
           perform   cod-cod-zvl-foi-000  thru cod-cod-zvl-foi-999    .
           go to     acc-lgv-vlt-110.
       acc-lgv-vlt-120.
           move      w-cod-cod-zvl-cod    to   v-alf                  .
       acc-lgv-vlt-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-lgv-vlt-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-lgv-vlt-999.
       acc-lgv-vlt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-lgv-vlt (1)      .
       acc-lgv-vlt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella                             *
      *                  *---------------------------------------------*
           move      w-tes-lgv-vlt (1)    to   w-let-arc-zvl-cod      .
           perform   let-arc-zvl-000      thru let-arc-zvl-999        .
           move      w-let-arc-zvl-des    to   w-tes-lgv-vlt-des (1)  .
           move      w-let-arc-zvl-dec    to   w-tes-lgv-dcv (1)      .
           move      w-let-arc-zvl-tdc    to   w-tes-lgv-tdc (1)      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-lgv-vlt-des-000  thru vis-lgv-vlt-des-999    .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zvl-flg    not  = spaces
                     go to acc-lgv-vlt-100.
      *                  *---------------------------------------------*
      *                  * Se sigla valuta per legame valutario uguale *
      *                  * a sigla valuta : reimpostazione             *
      *                  *---------------------------------------------*
           if        w-tes-lgv-vlt (1)    =    w-tes-sgl-vlt (1)
                     go to acc-lgv-vlt-100.
      *                  *---------------------------------------------*
      *                  * Se sigla valuta per legame valutario uguale *
      *                  * a sigla valuta base : reimpostazione        *
      *                  *---------------------------------------------*
           if        w-tes-lgv-vlt (1)    =    c-sgl
                     go to acc-lgv-vlt-100.
       acc-lgv-vlt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore attuale come precedente : oltre   *
      *                  *---------------------------------------------*
           if        w-tes-lgv-vlt (1)    =    w-sav-lgv-vlt
                     go to acc-lgv-vlt-800.
      *                  *---------------------------------------------*
      *                  * Normalizzazione altri valori associati al   *
      *                  * legame valutario                            *
      *                  *---------------------------------------------*
           move      zero                 to   w-tes-lgv-cdc (1)      .
           move      zero                 to   w-tes-lgv-pdt (1)      .
           perform   vis-lgv-cdc-000      thru vis-lgv-cdc-999        .
           perform   vis-lgv-pdt-000      thru vis-lgv-pdt-999        .
       acc-lgv-vlt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-lgv-vlt-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-lgv-vlt-100.
       acc-lgv-vlt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Sigla valuta per legame   *
      *    * valutario                                                 *
      *    *-----------------------------------------------------------*
       vis-lgv-vlt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-lgv-vlt (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-lgv-vlt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione valuta per    *
      *    * legame valutario                                          *
      *    *-----------------------------------------------------------*
       vis-lgv-vlt-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-lgv-vlt-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-lgv-vlt-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Coefficiente di cambio per   *
      *    * legame valutario                                          *
      *    *-----------------------------------------------------------*
       acc-lgv-cdc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-lgv-vlt (1)    =    spaces
                     go to acc-lgv-cdc-999.
       acc-lgv-cdc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      w-tes-lgv-dcv (1)    to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-lgv-cdc (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-lgv-cdc-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-lgv-cdc-999.
       acc-lgv-cdc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-lgv-cdc (1)      .
       acc-lgv-cdc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore zero non ammesso a meno che non si   *
      *                  * sia in Up                                   *
      *                  *---------------------------------------------*
           if        w-tes-lgv-cdc (1)    not  = zero
                     go to acc-lgv-cdc-600.
           if        v-key                =    "UP  "
                     go to acc-lgv-cdc-600
           else      go to acc-lgv-cdc-100.
       acc-lgv-cdc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-lgv-cdc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-lgv-cdc-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-lgv-cdc-100.
       acc-lgv-cdc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Coefficiente di cambio    *
      *    * per legame valutario                                      *
      *    *-----------------------------------------------------------*
       vis-lgv-cdc-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      w-tes-lgv-dcv (1)    to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-lgv-cdc (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-lgv-cdc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : % di tolleranza per legame   *
      *    * valutario                                                 *
      *    *-----------------------------------------------------------*
       acc-lgv-pdt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-lgv-vlt (1)    =    spaces
                     go to acc-lgv-pdt-999.
       acc-lgv-pdt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      02                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BD"                to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-lgv-pdt (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-lgv-pdt-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-lgv-pdt-999.
       acc-lgv-pdt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-lgv-pdt (1)      .
       acc-lgv-pdt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-lgv-pdt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-lgv-pdt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-lgv-pdt-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-lgv-pdt-100.
       acc-lgv-pdt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : % di tolleranza per lega- *
      *    * me valutario                                              *
      *    *-----------------------------------------------------------*
       vis-lgv-pdt-000.
           move      "DS"                 to   v-ope                  .
           move      01                   to   v-car                  .
           move      02                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BD"                to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-lgv-pdt (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-lgv-pdt-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Si/no trasformazione u.d.m.  *
      *    *-----------------------------------------------------------*
       acc-snx-tum-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione valore di default              *
      *                  *---------------------------------------------*
           if        w-tes-snx-tum (1)    not  = spaces
                     go to acc-snx-tum-100.
           move      "N"                  to   w-tes-snx-tum (1)      .
       acc-snx-tum-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-tum-lun    to   v-car                  .
           move      w-exp-snx-tum-num    to   v-ldt                  .
           move      "NSP#"               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-tum-tbl    to   v-txt                  .
           if        w-tes-snx-tum (1)    =    "N"
                     move  01             to   v-num
           else if   w-tes-snx-tum (1)    =    "S"
                     move  02             to   v-num
           else if   w-tes-snx-tum (1)    =    "P"
                     move  03             to   v-num
           else      move  zero           to   v-num                  .
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
                     go to acc-snx-tum-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-tum-999.
       acc-snx-tum-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "N"            to   w-tes-snx-tum (1)
           else if   v-num                =    02
                     move  "S"            to   w-tes-snx-tum (1)
           else if   v-num                =    03
                     move  "P"            to   w-tes-snx-tum (1)
           else      move  spaces         to   w-tes-snx-tum (1)      .
       acc-snx-tum-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-snx-tum-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Rivisualizzazione propmts coefficienti      *
      *                  *---------------------------------------------*
           perform   pmt-cmo-tum-000      thru pmt-cmo-tum-999        .
           perform   pmt-cdi-tum-000      thru pmt-cdi-tum-999        .
      *                  *---------------------------------------------*
      *                  * Test su tipo trasformazione                 *
      *                  *---------------------------------------------*
           if        w-tes-snx-tum (1)    =    "S" or
                     w-tes-snx-tum (1)    =    "P"
                     go to acc-snx-tum-800.
      *                  *---------------------------------------------*
      *                  * Normalizzazione dei valori per la trasfor-  *
      *                  * mazione dell'unita' di misura               *
      *                  *---------------------------------------------*
           move      spaces               to   w-tes-umf-tum (1)      .
           move      zero                 to   w-tes-nde-tum (1)      .
           move      zero                 to   w-tes-cmo-tum (1)      .
           move      zero                 to   w-tes-cdi-tum (1)      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione dei valori per la trasfor-  *
      *                  * mazione dell'unita' di misura               *
      *                  *---------------------------------------------*
           perform   vis-umf-tum-000      thru vis-umf-tum-999        .
           perform   vis-nde-tum-000      thru vis-nde-tum-999        .
           perform   vis-cmo-tum-000      thru vis-cmo-tum-999        .
           perform   vis-cdi-tum-000      thru vis-cdi-tum-999        .
       acc-snx-tum-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-tum-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-tum-100.
       acc-snx-tum-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Si/no trasformazione udm. *
      *    *-----------------------------------------------------------*
       vis-snx-tum-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-tum-lun    to   v-car                  .
           move      w-exp-snx-tum-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-tum-tbl    to   v-txt                  .
           if        w-tes-snx-tum (1)    =    "N"
                     move  01             to   v-num
           else if   w-tes-snx-tum (1)    =    "S"
                     move  02             to   v-num
           else if   w-tes-snx-tum (1)    =    "P"
                     move  03             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-tum-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : U.d.m. del fornitore         *
      *    *-----------------------------------------------------------*
       acc-umf-tum-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-snx-tum (1)    not  = "S" and
                     w-tes-snx-tum (1)    not  = "P"
                     go to acc-umf-tum-999.
       acc-umf-tum-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-umf-tum (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-umf-tum-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-umf-tum-999.
       acc-umf-tum-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-umf-tum (1)      .
       acc-umf-tum-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a non spaces il primo carattere   *
      *                  * non deve essere a spaces                    *
      *                  *---------------------------------------------*
           if        w-tes-umf-tum (1)    =    spaces
                     go to acc-umf-tum-600.
      *
           if        w-tes-umf-tum (1)
                    (01 : 01)             =    spaces
                     go to acc-umf-tum-100.
       acc-umf-tum-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-umf-tum-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-umf-tum-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-umf-tum-100.
       acc-umf-tum-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : U.d.m. del fornitore      *
      *    *-----------------------------------------------------------*
       vis-umf-tum-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-umf-tum (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-umf-tum-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Decimali per la quantita'    *
      *    *-----------------------------------------------------------*
       acc-nde-tum-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-snx-tum (1)    not  = "S" and
                     w-tes-snx-tum (1)    not  = "P"
                     go to acc-nde-tum-999.
       acc-nde-tum-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-nde-tum (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-nde-tum-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-nde-tum-999.
       acc-nde-tum-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-nde-tum (1)      .
       acc-nde-tum-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore non ammesso : reimpostazione      *
      *                  *---------------------------------------------*
           if        w-tes-nde-tum (1)    >    3
                     go to acc-nde-tum-100.
       acc-nde-tum-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-nde-tum-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-nde-tum-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-nde-tum-100.
       acc-nde-tum-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Decimali per la quantita' *
      *    *-----------------------------------------------------------*
       vis-nde-tum-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-nde-tum (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-nde-tum-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Coefficiente moltiplicatore  *
      *    *-----------------------------------------------------------*
       acc-cmo-tum-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-snx-tum (1)    not  = "S" and
                     w-tes-snx-tum (1)    not  = "P"
                     go to acc-cmo-tum-999.
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        w-tes-cmo-tum (1)    not  = zero
                     go to acc-cmo-tum-100.
           move      1                    to   w-tes-cmo-tum (1)      .
       acc-cmo-tum-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      17                   to   v-lin                  .
           move      70                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-cmo-tum (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cmo-tum-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cmo-tum-999.
       acc-cmo-tum-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cmo-tum (1)      .
       acc-cmo-tum-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cmo-tum-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cmo-tum-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cmo-tum-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cmo-tum-100.
       acc-cmo-tum-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Coefficiente moltiplic.   *
      *    *-----------------------------------------------------------*
       vis-cmo-tum-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      17                   to   v-lin                  .
           move      70                   to   v-pos                  .
           move      w-tes-cmo-tum (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cmo-tum-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Coefficiente divisore        *
      *    *-----------------------------------------------------------*
       acc-cdi-tum-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-snx-tum (1)    not  = "S" and
                     w-tes-snx-tum (1)    not  = "P"
                     go to acc-cdi-tum-999.
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        w-tes-cdi-tum (1)    not  = zero
                     go to acc-cdi-tum-100.
           move      1                    to   w-tes-cdi-tum (1)      .
       acc-cdi-tum-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      18                   to   v-lin                  .
           move      70                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-cdi-tum (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cdi-tum-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cdi-tum-999.
       acc-cdi-tum-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cdi-tum (1)      .
       acc-cdi-tum-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cdi-tum-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cdi-tum-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cdi-tum-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cdi-tum-100.
       acc-cdi-tum-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Coefficiente divisore     *
      *    *-----------------------------------------------------------*
       vis-cdi-tum-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      18                   to   v-lin                  .
           move      70                   to   v-pos                  .
           move      w-tes-cdi-tum (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cdi-tum-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Annotazioni                  *
      *    *-----------------------------------------------------------*
       acc-ann-not-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-ann-not-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "T"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-ldt                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-ann-not (1)    to   v-txt                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-ann-not-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-ann-not-999.
       acc-ann-not-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-txt                to   w-tes-ann-not (1)      .
       acc-ann-not-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : nessun compattamento   *
      *                  *---------------------------------------------*
           if        w-tes-ann-not (1)    not  = spaces
                     go to acc-ann-not-500
           else      go to acc-ann-not-600.
       acc-ann-not-500.
      *                  *---------------------------------------------*
      *                  * Compattamento verso l'alto delle righe di   *
      *                  * descrizione                                 *
      *                  *---------------------------------------------*
           move      w-tes-ann-not (1)    to   w-cmp-rig-txt-txt      .
           move      10                   to   w-cmp-rig-txt-max      .
           perform   cmp-rig-txt-000      thru cmp-rig-txt-999        .
      *                  *---------------------------------------------*
      *                  * Se compattamento avvenuto : reimpostazione  *
      *                  *---------------------------------------------*
           if        w-cmp-rig-txt-flg    =    spaces
                     go to acc-ann-not-600.
           move      w-cmp-rig-txt-txt    to   w-tes-ann-not (1)      .
           go to     acc-ann-not-100.
       acc-ann-not-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-ann-not-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-ann-not-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-ann-not-100.
       acc-ann-not-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Annotazioni               *
      *    *-----------------------------------------------------------*
       vis-ann-not-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-ann-not (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ann-not-999.
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
      *              * Test                                            *
      *              *-------------------------------------------------*
           if        w-tes-cod-dcf        =    zero  or
                     w-tes-tip-mag        =    zero  or
                     w-tes-num-pro        =    zero
                     move  "#"            to   w-cnt-tdo-key-flg
                     go to cnt-tdo-key-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione prompt per tipo prezzo d'acqui- *
      *              * sto                                             *
      *              *-------------------------------------------------*
           perform   pmt-tip-pza-000      thru pmt-tip-pza-999        .
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
           if        w-tes-cod-dcf        =    zero
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
       cnt-tdo-nok-030.
      *              *-------------------------------------------------*
      *              * Controllo su sigla valuta                       *
      *              *-------------------------------------------------*
           if        w-tes-sgl-vlt (1)    not  = spaces
                     go to cnt-tdo-nok-050.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Manca la sigla valuta !"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-050.
      *              *-------------------------------------------------*
      *              * Controllo decimali quantita' per lotto di ac-   *
      *              * quisto                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se necessario                          *
      *                  *---------------------------------------------*
           if        w-tes-lot-acq (1)    =    zero
                     go to cnt-tdo-nok-100.
      *                  *---------------------------------------------*
      *                  * Determinazione decimali per quantita' in    *
      *                  * lotto di acquisto                           *
      *                  *---------------------------------------------*
           if        w-tes-snx-tum (1)    not  = "S" and
                     w-tes-snx-tum (1)    not  = "P"
                     move  w-tes-num-pro-deq
                                          to   w-cnt-tdo-nok-wnd
           else      move  w-tes-nde-tum (1)
                                          to   w-cnt-tdo-nok-wnd      .
      *                  *---------------------------------------------*
      *                  * Quantita' per lotto di acquisto in work ri- *
      *                  * definita                                    *
      *                  *---------------------------------------------*
           move      w-tes-lot-acq (1)    to   w-cnt-tdo-nok-wrq
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-cnt-tdo-nok-wnd    =    0
                     if    w-cnt-tdo-nok-w1d
                                          =    zero and
                           w-cnt-tdo-nok-w2d
                                          =    zero and
                           w-cnt-tdo-nok-w3d
                                          =    zero
                           go to cnt-tdo-nok-100
                     else  go to cnt-tdo-nok-080
           else if   w-cnt-tdo-nok-wnd    =    1
                     if    w-cnt-tdo-nok-w2d
                                          =    zero and
                           w-cnt-tdo-nok-w3d
                                          =    zero
                           go to cnt-tdo-nok-100
                     else  go to cnt-tdo-nok-080
           else if   w-cnt-tdo-nok-wnd    =    2
                     if    w-cnt-tdo-nok-w3d
                                          =    zero
                           go to cnt-tdo-nok-100
                     else  go to cnt-tdo-nok-080
           else      go to cnt-tdo-nok-100.
       cnt-tdo-nok-080.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Numero decimali per lotto di acquisto errato !"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-100.
      *              *-------------------------------------------------*
      *              * Controlli su tabella prezzi e sconti            *
      *              *-------------------------------------------------*
           if        w-tes-tap-pes (1)    not  = 02
                     go to cnt-tdo-nok-200.
      *                  *---------------------------------------------*
      *                  * Normalizzazione contatore                   *
      *                  *---------------------------------------------*
           move      zero                 to   w-cnt-tdo-nok-c01      .
       cnt-tdo-nok-120.
      *                  *---------------------------------------------*
      *                  * Incremento contatore                        *
      *                  *---------------------------------------------*
           add       1                    to   w-cnt-tdo-nok-c01      .
      *                  *---------------------------------------------*
      *                  * Test se oltre il massimo                    *
      *                  *---------------------------------------------*
           if        w-cnt-tdo-nok-c01    >    06
                     go to cnt-tdo-nok-200.
      *                  *---------------------------------------------*
      *                  * Se elemento in corso di trattamento a zero  *
      *                  * si esce                                     *
      *                  *---------------------------------------------*
           if        w-tes-qta-pes
                    (1, w-cnt-tdo-nok-c01)
                                          =    zero
                     go to cnt-tdo-nok-200.
      *                  *---------------------------------------------*
      *                  * Se elemento in corso di trattamento diverso *
      *                  * da zero si controlla che i valori associa-  *
      *                  * ti non siano tutti a zero                   *
      *                  *---------------------------------------------*
           if        w-tes-prz-pes
                    (1, w-cnt-tdo-nok-c01)
                                          not  = zero or
                     w-tes-psr-pes
                    (1, w-cnt-tdo-nok-c01, 1)
                                          not  = zero or
                     w-tes-psr-pes
                    (1, w-cnt-tdo-nok-c01, 2)
                                          not  = zero or
                     w-tes-psr-pes
                    (1, w-cnt-tdo-nok-c01, 3)
                                          not  = zero or
                     w-tes-psr-pes
                    (1, w-cnt-tdo-nok-c01, 4)
                                          not  = zero or
                     w-tes-psr-pes
                    (1, w-cnt-tdo-nok-c01, 5)
                                          not  = zero
                     go to cnt-tdo-nok-140.
           move      "Mancano elementi in tabella prezzi e sconti !   "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-140.
      *                  *---------------------------------------------*
      *                  * Aggiornamento secondo contatore             *
      *                  *---------------------------------------------*
           move      w-cnt-tdo-nok-c01    to   w-cnt-tdo-nok-c02      .
      *                  *---------------------------------------------*
      *                  * Incremento secondo contatore                *
      *                  *---------------------------------------------*
           add       1                    to   w-cnt-tdo-nok-c02      .
      *                  *---------------------------------------------*
      *                  * Test se oltre il massimo                    *
      *                  *---------------------------------------------*
           if        w-cnt-tdo-nok-c02    >    06
                     move  w-cnt-tdo-nok-c02
                                          to   w-cnt-tdo-nok-c01
                     go to cnt-tdo-nok-200.
      *                  *---------------------------------------------*
      *                  * Se elemento successivo a quello in corso di *
      *                  * trattamento a zero : fine ciclo             *
      *                  *---------------------------------------------*
           if        w-tes-qta-pes
                    (1, w-cnt-tdo-nok-c02)
                                          =    zero
                     move  w-cnt-tdo-nok-c02
                                          to   w-cnt-tdo-nok-c01
                     go to cnt-tdo-nok-200.
      *                  *---------------------------------------------*
      *                  * Se elemento successivo maggiore di quello   *
      *                  * in corso di trattamento : errore            *
      *                  *---------------------------------------------*
           if        w-tes-qta-pes
                    (1, w-cnt-tdo-nok-c02)
                                          >    w-tes-qta-pes
                                              (1, w-cnt-tdo-nok-c01)
                     go to cnt-tdo-nok-120.
           move      "Valori non progressivi in tabella prezzi/sconti "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-200.
      *              *-------------------------------------------------*
      *              * Test su coefficiente di cambio per legame valu- *
      *              * tario                                           *
      *              *-------------------------------------------------*
           if        w-tes-lgv-vlt (1)    =    spaces
                     go to cnt-tdo-nok-300.
           if        w-tes-lgv-cdc (1)    not  = zero
                     go to cnt-tdo-nok-300.
           move      "Manca il coefficiente di cambio !               "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-300.
      *              *-------------------------------------------------*
      *              * Test su descrizione prodotto per il fornitore   *
      *              *-------------------------------------------------*
           if        w-acc-opz-des-acc    =    01
                     go to cnt-tdo-nok-800.
           if        w-tes-dep-sfn (1)    not  = spaces
                     go to cnt-tdo-nok-800.
           move      "Manca la descrizione prodotto per il fornitore !"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-800.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo prezzo d'acquisto                      *
      *                  *---------------------------------------------*
           if        w-tes-tip-pza (1)    =    zero
                     move  01             to   w-tes-tip-pza (1)      .
      *                  *---------------------------------------------*
      *                  * Tipo applicazione prezzi e sconti           *
      *                  *---------------------------------------------*
           if        w-tes-tap-pes (1)    =    zero
                     move  01             to   w-tes-tap-pes (1)      .
       cnt-tdo-nok-820.
      *                  *---------------------------------------------*
      *                  * Coefficienti per la trasformazione          *
      *                  *---------------------------------------------*
           if        w-tes-snx-tum (1)    not  = "S" and
                     w-tes-snx-tum (1)    not  = "P"
                     go to cnt-tdo-nok-840.
           if        w-tes-cmo-tum (1)    =    zero
                     move  1              to   w-tes-cmo-tum (1)      .
           if        w-tes-cdi-tum (1)    =    zero
                     move  1              to   w-tes-cdi-tum (1)      .
       cnt-tdo-nok-840.
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
           move      zero                 to   w-tes-cod-dcf          .
           move      spaces               to   w-tes-cod-dcf-rag      .
           move      spaces               to   w-tes-cod-dcf-vlt      .
           move      zero                 to   w-tes-tip-mag          .
           move      zero                 to   w-tes-num-pro          .
           move      spaces               to   w-tes-alf-pro          .
           move      spaces               to   w-tes-num-pro-des      .
           move      spaces               to   w-tes-num-pro-umi      .
           move      zero                 to   w-tes-num-pro-plb      .
           move      zero                 to   w-tes-num-pro-deq      .
           move      zero                 to   w-tes-num-pro-dep      .
           move      spaces               to   w-tes-fda-pif          .
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *-----------------------------------------------------------*
       nor-nok-tes-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione                                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-tes-ide-dat (1)      .
           move      spaces               to   w-tes-ide-ute (1)      .
           move      spaces               to   w-tes-ide-fas (1)      .
           move      spaces               to   w-tes-cop-sfn (1)      .
           move      spaces               to   w-tes-dep-sfn (1)      .
           move      zero                 to   w-tes-xdp-sfn (1)      .
           move      spaces               to   w-tes-snx-tum (1)      .
           move      spaces               to   w-tes-umf-tum (1)      .
           move      zero                 to   w-tes-nde-tum (1)      .
           move      zero                 to   w-tes-cmo-tum (1)      .
           move      zero                 to   w-tes-cdi-tum (1)      .
           move      spaces               to   w-tes-dpz-dcf (1)      .
           move      spaces               to   w-tes-ann-not (1)      .
           move      zero                 to   w-tes-tmp-cns (1)      .
           move      spaces               to   w-tes-sgl-vlt (1)      .
           move      spaces               to   w-tes-sgl-vlt-des (1)  .
           move      zero                 to   w-tes-dec-vlt (1)      .
           move      spaces               to   w-tes-fda-ndp (1)      .
           move      zero                 to   w-tes-dec-prz (1)      .
           move      zero                 to   w-tes-tip-pza (1)      .
           move      zero                 to   w-tes-lot-acq (1)      .
           move      zero                 to   w-tes-tap-pes (1)      .
           move      zero                 to   w-tes-uda-pes (1)      .
           move      spaces               to   w-tes-lgv-vlt (1)      .
           move      spaces               to   w-tes-lgv-vlt-des (1)  .
           move      zero                 to   w-tes-lgv-dcv (1)      .
           move      zero                 to   w-tes-lgv-cdc (1)      .
           move      spaces               to   w-tes-lgv-tdc (1)      .
           move      zero                 to   w-tes-lgv-pdt (1)      .
           move      spaces               to   w-tes-vlt-std (1)      .
           move      zero                 to   w-tes-ndp-std (1)      .
           move      zero                 to   w-tes-prz-med (1)      .
           move      zero                 to   w-tes-tmp-med (1)      .
           move      zero                 to   w-tes-qta-pes (1, 1)   .
           move      zero                 to   w-tes-qta-pes (1, 2)   .
           move      zero                 to   w-tes-qta-pes (1, 3)   .
           move      zero                 to   w-tes-qta-pes (1, 4)   .
           move      zero                 to   w-tes-qta-pes (1, 5)   .
           move      zero                 to   w-tes-qta-pes (1, 6)   .
           move      zero                 to   w-tes-prz-pes (1, 1)   .
           move      zero                 to   w-tes-prz-pes (1, 2)   .
           move      zero                 to   w-tes-prz-pes (1, 3)   .
           move      zero                 to   w-tes-prz-pes (1, 4)   .
           move      zero                 to   w-tes-prz-pes (1, 5)   .
           move      zero                 to   w-tes-prz-pes (1, 6)   .
           move      zero                 to   w-tes-csr-pes (1, 1)   .
           move      zero                 to   w-tes-csr-pes (1, 2)   .
           move      zero                 to   w-tes-csr-pes (1, 3)   .
           move      zero                 to   w-tes-csr-pes (1, 4)   .
           move      zero                 to   w-tes-csr-pes (1, 5)   .
           move      zero                 to   w-tes-csr-pes (1, 6)   .
           move      zero                 to   w-tes-psr-pes (1, 1, 1).
           move      zero                 to   w-tes-psr-pes (1, 1, 2).
           move      zero                 to   w-tes-psr-pes (1, 1, 3).
           move      zero                 to   w-tes-psr-pes (1, 1, 4).
           move      zero                 to   w-tes-psr-pes (1, 1, 5).
           move      zero                 to   w-tes-psr-pes (1, 2, 1).
           move      zero                 to   w-tes-psr-pes (1, 2, 2).
           move      zero                 to   w-tes-psr-pes (1, 2, 3).
           move      zero                 to   w-tes-psr-pes (1, 2, 4).
           move      zero                 to   w-tes-psr-pes (1, 2, 5).
           move      zero                 to   w-tes-psr-pes (1, 3, 1).
           move      zero                 to   w-tes-psr-pes (1, 3, 2).
           move      zero                 to   w-tes-psr-pes (1, 3, 3).
           move      zero                 to   w-tes-psr-pes (1, 3, 4).
           move      zero                 to   w-tes-psr-pes (1, 3, 5).
           move      zero                 to   w-tes-psr-pes (1, 4, 1).
           move      zero                 to   w-tes-psr-pes (1, 4, 2).
           move      zero                 to   w-tes-psr-pes (1, 4, 3).
           move      zero                 to   w-tes-psr-pes (1, 4, 4).
           move      zero                 to   w-tes-psr-pes (1, 4, 5).
           move      zero                 to   w-tes-psr-pes (1, 5, 1).
           move      zero                 to   w-tes-psr-pes (1, 5, 2).
           move      zero                 to   w-tes-psr-pes (1, 5, 3).
           move      zero                 to   w-tes-psr-pes (1, 5, 4).
           move      zero                 to   w-tes-psr-pes (1, 5, 5).
           move      zero                 to   w-tes-psr-pes (1, 6, 1).
           move      zero                 to   w-tes-psr-pes (1, 6, 2).
           move      zero                 to   w-tes-psr-pes (1, 6, 3).
           move      zero                 to   w-tes-psr-pes (1, 6, 4).
           move      zero                 to   w-tes-psr-pes (1, 6, 5).
           move      zero                 to   w-tes-per-mpa (1)      .
           move      spaces               to   w-tes-rif-lst (1)      .
           move      zero                 to   w-tes-per-ric (1)      .
           move      spaces               to   w-tes-alx-exp (1)      .
      *              *-------------------------------------------------*
      *              * Normalizzazione comodi di accettazione          *
      *              *-------------------------------------------------*
           move      zero                 to   w-acc-opz-des-acc      .
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
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *              *-------------------------------------------------*
      *              * Lettura                                         *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "PRFNFM    "         to   f-key                  .
           move      w-tes-tip-mag        to   rf-aaf-tip-mag         .
           move      w-tes-num-pro        to   rf-aaf-num-pro         .
           move      w-tes-cod-dcf        to   rf-aaf-cod-dcf         .
           move      w-tes-fda-pif        to   rf-aaf-fda-pif         .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
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
      *                      * Attivazione flag di accettazione numero *
      *                      * decimali prezzo                         *
      *                      *-----------------------------------------*
           move      "#"                  to   w-tes-fda-ndp (1)      .
      *                          *-------------------------------------*
      *                          * Valori contenuti direttamente in    *
      *                          * record [aaf]                        *
      *                          *-------------------------------------*
           perform   cmp-rec-tes-000      thru cmp-rec-tes-999        .
       rou-let-reg-250.
      *                          *-------------------------------------*
      *                          * Valori contenuti indirettamente in  *
      *                          * record [aaf]                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Opzione per la descrizione      *
      *                              *---------------------------------*
           if        rf-aaf-dep-sfn       =    spaces
                     move  01             to   w-acc-opz-des-acc
           else      move  02             to   w-acc-opz-des-acc      .
       rou-let-reg-300.
      *                              *---------------------------------*
      *                              * Lettura archivio [pdx] per la   *
      *                              * descrizione                     *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Test su flag di presenza de-*
      *                                  * scrizione estesa            *
      *                                  *-----------------------------*
           if        w-tes-xdp-sfn (1)    =    0
                     go to rou-let-reg-340.
      *                                  *-----------------------------*
      *                                  * Lettura [pdx]               *
      *                                  *-----------------------------*
           if        w-tes-tip-mag        =    01
                     move  13             to   w-det-txt-pdx-tip
           else if   w-tes-tip-mag        =    03
                     move  33             to   w-det-txt-pdx-tip
           else if   w-tes-tip-mag        =    04
                     move  43             to   w-det-txt-pdx-tip      .
      *
           move      w-tes-cod-dcf        to   w-det-txt-pdx-arc      .
           move      w-tes-num-pro        to   w-det-txt-pdx-mag      .
           move      w-tes-fda-pif        to   w-det-txt-pdx-fda      .
           perform   det-txt-pdx-000      thru det-txt-pdx-999        .
           move      w-det-txt-pdx-txt    to   w-tes-dep-sfn (1)      .
       rou-let-reg-340.
      *                              *---------------------------------*
      *                              * Lettura archivio [pdx] per le   *
      *                              * annotazioni                     *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Lettura [pdx]               *
      *                                  *-----------------------------*
           if        w-tes-tip-mag        =    01
                     move  14             to   w-det-txt-pdx-tip
           else if   w-tes-tip-mag        =    03
                     move  34             to   w-det-txt-pdx-tip
           else if   w-tes-tip-mag        =    04
                     move  44             to   w-det-txt-pdx-tip      .
      *
           move      w-tes-cod-dcf        to   w-det-txt-pdx-arc      .
           move      w-tes-num-pro        to   w-det-txt-pdx-mag      .
           move      w-tes-fda-pif        to   w-det-txt-pdx-fda      .
           perform   det-txt-pdx-000      thru det-txt-pdx-999        .
           move      w-det-txt-pdx-txt    to   w-tes-ann-not (1)      .
      *                                  *-----------------------------*
      *                                  * Eventuale recupero del va-  *
      *                                  * lore nel campo note prece-  *
      *                                  * dente                       *
      *                                  *-----------------------------*
           if        rf-aaf-ann-not       not  = spaces  and
                     w-tes-ann-not (1)    =    spaces
                     move  rf-aaf-ann-not to   w-tes-ann-not (1)      .
       rou-let-reg-380.
      *                              *---------------------------------*
      *                              * Descrizione per sigla valuta    *
      *                              *---------------------------------*
           move      w-tes-sgl-vlt (1)    to   w-let-arc-zvl-cod      .
           perform   let-arc-zvl-000      thru let-arc-zvl-999        .
           move      w-let-arc-zvl-des    to   w-tes-sgl-vlt-des (1)  .
      *                              *---------------------------------*
      *                              * Descrizione per sigla valuta di *
      *                              * riferimento per legame valutario*
      *                              *---------------------------------*
           move      w-tes-lgv-vlt (1)    to   w-let-arc-zvl-cod      .
           perform   let-arc-zvl-000      thru let-arc-zvl-999        .
           move      w-let-arc-zvl-des    to   w-tes-lgv-vlt-des (1)  .
      *                          *-------------------------------------*
      *                          * Valori precedenti anagrafica        *
      *                          *-------------------------------------*
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
      *    * Composizione area dati di testata                         *
      *    *-----------------------------------------------------------*
       cmp-rec-tes-000.
      *              *-------------------------------------------------*
      *              * Composizione                                    *
      *              *-------------------------------------------------*
           move      rf-aaf-ide-dat       to   w-tes-ide-dat (1)      .
           move      rf-aaf-ide-ute       to   w-tes-ide-ute (1)      .
           move      rf-aaf-ide-fas       to   w-tes-ide-fas (1)      .
           move      rf-aaf-cop-sfn       to   w-tes-cop-sfn (1)      .
           move      rf-aaf-dep-sfn       to   w-tes-dep-sfn (1)      .
           move      rf-aaf-xdp-sfn       to   w-tes-xdp-sfn (1)      .
           move      rf-aaf-snx-tum       to   w-tes-snx-tum (1)      .
           move      rf-aaf-umf-tum       to   w-tes-umf-tum (1)      .
           move      rf-aaf-nde-tum       to   w-tes-nde-tum (1)      .
           move      rf-aaf-cmo-tum       to   w-tes-cmo-tum (1)      .
           move      rf-aaf-cdi-tum       to   w-tes-cdi-tum (1)      .
           move      rf-aaf-dpz-dcf       to   w-tes-dpz-dcf (1)      .
           move      rf-aaf-tmp-cns       to   w-tes-tmp-cns (1)      .
           move      rf-aaf-sgl-vlt       to   w-tes-sgl-vlt (1)      .
           move      rf-aaf-dec-vlt       to   w-tes-dec-vlt (1)      .
           move      rf-aaf-dec-prz       to   w-tes-dec-prz (1)      .
           move      rf-aaf-tip-pza       to   w-tes-tip-pza (1)      .
           move      rf-aaf-lot-acq       to   w-tes-lot-acq (1)      .
           move      rf-aaf-tap-pes       to   w-tes-tap-pes (1)      .
           move      rf-aaf-uda-pes       to   w-tes-uda-pes (1)      .
           move      rf-aaf-lgv-vlt       to   w-tes-lgv-vlt (1)      .
           move      rf-aaf-lgv-dcv       to   w-tes-lgv-dcv (1)      .
           move      rf-aaf-lgv-tdc       to   w-tes-lgv-tdc (1)      .
           move      rf-aaf-lgv-cdc       to   w-tes-lgv-cdc (1)      .
           move      rf-aaf-lgv-pdt       to   w-tes-lgv-pdt (1)      .
           move      rf-aaf-qta-pes (1)   to   w-tes-qta-pes (1, 1)   .
           move      rf-aaf-qta-pes (2)   to   w-tes-qta-pes (1, 2)   .
           move      rf-aaf-qta-pes (3)   to   w-tes-qta-pes (1, 3)   .
           move      rf-aaf-qta-pes (4)   to   w-tes-qta-pes (1, 4)   .
           move      rf-aaf-qta-pes (5)   to   w-tes-qta-pes (1, 5)   .
           move      rf-aaf-qta-pes (6)   to   w-tes-qta-pes (1, 6)   .
           move      rf-aaf-prz-pes (1)   to   w-tes-prz-pes (1, 1)   .
           move      rf-aaf-prz-pes (2)   to   w-tes-prz-pes (1, 2)   .
           move      rf-aaf-prz-pes (3)   to   w-tes-prz-pes (1, 3)   .
           move      rf-aaf-prz-pes (4)   to   w-tes-prz-pes (1, 4)   .
           move      rf-aaf-prz-pes (5)   to   w-tes-prz-pes (1, 5)   .
           move      rf-aaf-prz-pes (6)   to   w-tes-prz-pes (1, 6)   .
           move      rf-aaf-csr-pes (1)   to   w-tes-csr-pes (1, 1)   .
           move      rf-aaf-csr-pes (2)   to   w-tes-csr-pes (1, 2)   .
           move      rf-aaf-csr-pes (3)   to   w-tes-csr-pes (1, 3)   .
           move      rf-aaf-csr-pes (4)   to   w-tes-csr-pes (1, 4)   .
           move      rf-aaf-csr-pes (5)   to   w-tes-csr-pes (1, 5)   .
           move      rf-aaf-csr-pes (6)   to   w-tes-csr-pes (1, 6)   .
           move      rf-aaf-psr-pes (1, 1)
                                          to   w-tes-psr-pes (1, 1, 1).
           move      rf-aaf-psr-pes (1, 2)
                                          to   w-tes-psr-pes (1, 1, 2).
           move      rf-aaf-psr-pes (1, 3)
                                          to   w-tes-psr-pes (1, 1, 3).
           move      rf-aaf-psr-pes (1, 4)
                                          to   w-tes-psr-pes (1, 1, 4).
           move      rf-aaf-psr-pes (1, 5)
                                          to   w-tes-psr-pes (1, 1, 5).
           move      rf-aaf-psr-pes (2, 1)
                                          to   w-tes-psr-pes (1, 2, 1).
           move      rf-aaf-psr-pes (2, 2)
                                          to   w-tes-psr-pes (1, 2, 2).
           move      rf-aaf-psr-pes (2, 3)
                                          to   w-tes-psr-pes (1, 2, 3).
           move      rf-aaf-psr-pes (2, 4)
                                          to   w-tes-psr-pes (1, 2, 4).
           move      rf-aaf-psr-pes (2, 5)
                                          to   w-tes-psr-pes (1, 2, 5).
           move      rf-aaf-psr-pes (3, 1)
                                          to   w-tes-psr-pes (1, 3, 1).
           move      rf-aaf-psr-pes (3, 2)
                                          to   w-tes-psr-pes (1, 3, 2).
           move      rf-aaf-psr-pes (3, 3)
                                          to   w-tes-psr-pes (1, 3, 3).
           move      rf-aaf-psr-pes (3, 4)
                                          to   w-tes-psr-pes (1, 3, 4).
           move      rf-aaf-psr-pes (3, 5)
                                          to   w-tes-psr-pes (1, 3, 5).
           move      rf-aaf-psr-pes (4, 1)
                                          to   w-tes-psr-pes (1, 4, 1).
           move      rf-aaf-psr-pes (4, 2)
                                          to   w-tes-psr-pes (1, 4, 2).
           move      rf-aaf-psr-pes (4, 3)
                                          to   w-tes-psr-pes (1, 4, 3).
           move      rf-aaf-psr-pes (4, 4)
                                          to   w-tes-psr-pes (1, 4, 4).
           move      rf-aaf-psr-pes (4, 5)
                                          to   w-tes-psr-pes (1, 4, 5).
           move      rf-aaf-psr-pes (5, 1)
                                          to   w-tes-psr-pes (1, 5, 1).
           move      rf-aaf-psr-pes (5, 2)
                                          to   w-tes-psr-pes (1, 5, 2).
           move      rf-aaf-psr-pes (5, 3)
                                          to   w-tes-psr-pes (1, 5, 3).
           move      rf-aaf-psr-pes (5, 4)
                                          to   w-tes-psr-pes (1, 5, 4).
           move      rf-aaf-psr-pes (5, 5)
                                          to   w-tes-psr-pes (1, 5, 5).
           move      rf-aaf-psr-pes (6, 1)
                                          to   w-tes-psr-pes (1, 6, 1).
           move      rf-aaf-psr-pes (6, 2)
                                          to   w-tes-psr-pes (1, 6, 2).
           move      rf-aaf-psr-pes (6, 3)
                                          to   w-tes-psr-pes (1, 6, 3).
           move      rf-aaf-psr-pes (6, 4)
                                          to   w-tes-psr-pes (1, 6, 4).
           move      rf-aaf-psr-pes (6, 5)
                                          to   w-tes-psr-pes (1, 6, 5).
           move      rf-aaf-per-mpa       to   w-tes-per-mpa (1)      .
           move      rf-aaf-rif-lst       to   w-tes-rif-lst (1)      .
           move      rf-aaf-alx-exp       to   w-tes-alx-exp (1)      .
      *              *-------------------------------------------------*
      *              * Normalizzazione campi aggiunti                  *
      *              *-------------------------------------------------*
           if        rf-aaf-per-ric       not  numeric
                     move  zero           to   w-tes-per-ric (1)      .
       cmp-rec-tes-999.
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
      *              * Eventuali default da formato base               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se formato non base                    *
      *                  *---------------------------------------------*
           if        w-tes-fda-pif        =    spaces
                     go to pre-acc-ins-400.
      *                  *---------------------------------------------*
      *                  * Test su personalizzazioni                   *
      *                  *---------------------------------------------*
           if        w-prs-def-ins-fmb    not  = 01
                     go to pre-acc-ins-400.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *                  *---------------------------------------------*
      *                  * Lettura formato base                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "PRFNFM    "         to   f-key                  .
           move      w-tes-tip-mag        to   rf-aaf-tip-mag         .
           move      w-tes-num-pro        to   rf-aaf-num-pro         .
           move      w-tes-cod-dcf        to   rf-aaf-cod-dcf         .
           move      spaces               to   rf-aaf-fda-pif         .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *                      *-----------------------------------------*
      *                      * Se record non trovato: a default        *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to pre-acc-ins-400.
      *                  *---------------------------------------------*
      *                  * Preparazione default                        *
      *                  *---------------------------------------------*
           perform   cmp-rec-tes-000      thru cmp-rec-tes-999        .
      *                  *---------------------------------------------*
      *                  * Normalizzazione status visualizzazione dati *
      *                  *---------------------------------------------*
           move      spaces               to   w-cnt-sts-vis-pte      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione testata per default         *
      *                  *---------------------------------------------*
           perform   vis-tes-reg-000      thru vis-tes-reg-999        .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pre-acc-ins-900.
       pre-acc-ins-400.
      *              *-------------------------------------------------*
      *              * Sigla valuta legata al fornitore                *
      *              *-------------------------------------------------*
           move      w-tes-cod-dcf-vlt    to   w-tes-sgl-vlt (1)      .
           move      w-tes-sgl-vlt (1)    to   w-let-arc-zvl-cod      .
           perform   let-arc-zvl-000      thru let-arc-zvl-999        .
           move      w-let-arc-zvl-des    to   w-tes-sgl-vlt-des (1)  .
           move      w-let-arc-zvl-dec    to   w-tes-dec-vlt (1)      .
      *              *-------------------------------------------------*
      *              * Preparazione default per tipo prezzo acquisto   *
      *              *-------------------------------------------------*
           move      01                   to   w-tes-tip-pza (1)      .
      *              *-------------------------------------------------*
      *              * Preparazione default per tipo applicazione      *
      *              * prezzi                                          *
      *              *-------------------------------------------------*
           move      01                   to   w-tes-tap-pes (1)      .
      *              *-------------------------------------------------*
      *              * Decimali prezzo                                 *
      *              *-------------------------------------------------*
           move      w-tes-ndp-std (1)    to   w-tes-dec-prz (1)      .
      *              *-------------------------------------------------*
      *              * Visualizzazione testata per default             *
      *              *-------------------------------------------------*
           perform   vis-tes-reg-000      thru vis-tes-reg-999        .
      *              *-------------------------------------------------*
      *              * Preparazione default per descrizione prodotto   *
      *              * per il fornitore                                *
      *              *-------------------------------------------------*
           move      01                   to   w-acc-opz-des-acc      .
           move      spaces               to   w-tes-dep-sfn (1)      .
      *              *-------------------------------------------------*
      *              * Prompt descrizione per il fornitore             *
      *              *-------------------------------------------------*
           perform   pmt-dep-sfn-000      thru pmt-dep-sfn-999        .
       pre-acc-ins-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pre-acc-ins-999.
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
      *              * Prompt descrizione per il fornitore             *
      *              *-------------------------------------------------*
           perform   pmt-dep-sfn-000      thru pmt-dep-sfn-999        .
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
      *              * Prompt descrizione per il fornitore             *
      *              *-------------------------------------------------*
           perform   pmt-dep-sfn-000      thru pmt-dep-sfn-999        .
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
       pos-cnf-mod-100.
      *              *-------------------------------------------------*
      *              * Se il fornitore in corso di trattamento e'      *
      *              * quello definito preferenziale nella scheda dati *
      *              * generali di acquisto della voce di magazzino    *
      *              * trattata, si chiede conferma per l'aggiornamen- *
      *              * to                                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su valori significativi                *
      *                  *---------------------------------------------*
           if        w-tes-prz-pes (1, 1) =    w-tes-prz-pes (2, 1) and
                     w-tes-sgl-vlt (1)    =    w-tes-sgl-vlt (2)    and
                     w-tes-dec-vlt (1)    =    w-tes-dec-vlt (2)    and
                     w-tes-dec-prz (1)    =    w-tes-dec-prz (2)    and
                     w-tes-tmp-cns (1)    =    w-tes-tmp-cns (2)    and
                     w-tes-lot-acq (1)    =    w-tes-lot-acq (2)
                     go to pos-cnf-mod-900.
      *                  *---------------------------------------------*
      *                  * Normalizzazione [aaq]                       *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *                  *---------------------------------------------*
      *                  * Lettura [aaq]                               *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO"             to   f-key                  .
           move      w-tes-tip-mag        to   rf-aaq-tip-mag         .
           move      w-tes-num-pro        to   rf-aaq-num-pro         .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato : uscita              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to pos-cnf-mod-900.
      *                  *---------------------------------------------*
      *                  * Se record trovato, ma non fornitore prefe-  *
      *                  * renziale : uscita                           *
      *                  *---------------------------------------------*
           if        rf-aaq-dcf-pfz       not  = w-tes-cod-dcf
                     go to pos-cnf-mod-900.
      *                      *-----------------------------------------*
      *                      * Box di richiesta per avvenuta variazio- *
      *                      * ne                                      *
      *                      *-----------------------------------------*
           perform   pos-cnf-mod-aaq-000  thru pos-cnf-mod-aaq-999    .
      *                          *-------------------------------------*
      *                          * Deviazione a seconda della risposta *
      *                          *-------------------------------------*
           if        w-exp-agg-aaq-sce    not  = 01
                     go to pos-cnf-mod-900.
      *                      *-----------------------------------------*
      *                      * Aggiornamento [aaq]                     *
      *                      *-----------------------------------------*
           perform   upd-rec-aaq-000      thru upd-rec-aaq-999        .
       pos-cnf-mod-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pos-cnf-mod-999.
       pos-cnf-mod-999.
           exit.

      *    *===========================================================*
      *    * Routine post-conferma di modifica                         *
      *    *                                                           *
      *    * Box per eventuale aggiornamento [aaq]                     *
      *    *-----------------------------------------------------------*
       pos-cnf-mod-aaq-000.
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
           move      "Attenzione : sono state effettuate variazioni rela
      -              "tive al       "     to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      64                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "             fornitore preferenziale              
      -              "              "     to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
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
       pos-cnf-mod-aaq-200.
      *              *-------------------------------------------------*
      *              * Accettazione risposta                           *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-agg-aaq-lun    to   v-car                  .
           move      w-exp-agg-aaq-num    to   v-ldt                  .
           move      "AN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      13                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-exp-agg-aaq-tbl    to   v-txt                  .
           move      zero                 to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-exp-agg-aaq-sce      .
      *              *-------------------------------------------------*
      *              * Controllo risposta                              *
      *              *-------------------------------------------------*
           if        w-exp-agg-aaq-sce    not  = 01 and
                     w-exp-agg-aaq-sce    not  = 02
                     go to pos-cnf-mod-aaq-200.
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pos-cnf-mod-aaq-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pos-cnf-mod-aaq-999.
       pos-cnf-mod-aaq-999.
           exit.

      *    *===========================================================*
      *    * Aggiornamento record [aaq]                                *
      *    *-----------------------------------------------------------*
       upd-rec-aaq-000.
      *              *-------------------------------------------------*
      *              * Lettura, con lock, del record                   *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "NUMPRO"             to   f-key                  .
           move      w-tes-tip-mag        to   rf-aaq-tip-mag         .
           move      w-tes-num-pro        to   rf-aaq-num-pro         .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda dell'esito della lettura   *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to upd-rec-aaq-250.
       upd-rec-aaq-200.
      *              *-------------------------------------------------*
      *              * Se record [aaq] non esistente                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     upd-rec-aaq-900.
       upd-rec-aaq-250.
      *              *-------------------------------------------------*
      *              * Se record [aaq] esistente                       *
      *              *-------------------------------------------------*
       upd-rec-aaq-300.
      *                  *---------------------------------------------*
      *                  * Aggiornamento identificativi                *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-aaq-ide-dat         .
           move      s-ute                to   rf-aaq-ide-ute         .
           move      s-fas                to   rf-aaq-ide-fas         .
      *                  *---------------------------------------------*
      *                  * Data aggiornamento prezzi d'acquisto        *
      *                  *---------------------------------------------*
           move      w-tes-uda-pes (1)    to   rf-aaq-uda-par         .
      *                  *---------------------------------------------*
      *                  * Prezzo medio d'acquisto                     *
      *                  *---------------------------------------------*
           move      w-tes-prz-pes (1, 1) to   rf-aaq-prz-acr         .
      *                  *---------------------------------------------*
      *                  * Sigla valuta                                *
      *                  *---------------------------------------------*
           move      w-tes-sgl-vlt (1)    to   rf-aaq-sgl-vlt         .
      *                  *---------------------------------------------*
      *                  * Decimali valuta                             *
      *                  *---------------------------------------------*
           move      w-tes-dec-vlt (1)    to   rf-aaq-dec-vlt         .
      *                  *---------------------------------------------*
      *                  * Decimali prezzo                             *
      *                  *---------------------------------------------*
           move      w-tes-dec-prz (1)    to   rf-aaq-dec-prz         .
      *                  *---------------------------------------------*
      *                  * Tempo di approvigionamento medio            *
      *                  *---------------------------------------------*
           move      w-tes-tmp-cns (1)    to   rf-aaq-tmp-apv         .
      *                  *---------------------------------------------*
      *                  * Lotto di acquisto di riferimento            *
      *                  *---------------------------------------------*
           move      w-tes-lot-acq (1)    to   rf-aaq-lot-acq         .
       upd-rec-aaq-400.
      *                  *---------------------------------------------*
      *                  * Update record                               *
      *                  *---------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
       upd-rec-aaq-500.
      *                  *---------------------------------------------*
      *                  * Release                                     *
      *                  *---------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
       upd-rec-aaq-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     upd-rec-aaq-999.
       upd-rec-aaq-999.
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
      *              * Trattamento file [aaf]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-500.
      *                      *-----------------------------------------*
      *                      * Write record [aaf]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-aaf-000      thru wrt-rec-aaf-999        .
      *                      *-----------------------------------------*
      *                      * Write record [pdx]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-pdx-000      thru wrt-rec-pdx-999        .
      *                      *-----------------------------------------*
      *                      * Write record [pdx] per le note          *
      *                      *-----------------------------------------*
           perform   wrt-rec-pdx-not-000  thru wrt-rec-pdx-not-999    .
      *                      *-----------------------------------------*
      *                      * Aggiornamento record [dcp]              *
      *                      *-----------------------------------------*
           perform   agg-rec-dcp-000      thru agg-rec-dcp-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     scr-mov-fil-999.
       scr-mov-fil-500.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rewrite record [aaf]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-aaf-000      thru rew-rec-aaf-999        .
      *                      *-----------------------------------------*
      *                      * Rewrite record [pdx]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-pdx-000      thru rew-rec-pdx-999        .
      *                      *-----------------------------------------*
      *                      * Rewrite record [pdx] per le note        *
      *                      *-----------------------------------------*
           perform   rew-rec-pdx-not-000  thru rew-rec-pdx-not-999    .
      *                      *-----------------------------------------*
      *                      * Aggiornamento record [dcp]              *
      *                      *-----------------------------------------*
           perform   agg-rec-dcp-000      thru agg-rec-dcp-999        .
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *-----------------------------------------------------------*
       del-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Delete record [aaf]                             *
      *              *-------------------------------------------------*
           perform   del-rec-aaf-000      thru del-rec-aaf-999        .
      *              *-------------------------------------------------*
      *              * Delete record [pdx]                             *
      *              *-------------------------------------------------*
           perform   del-rec-pdx-000      thru del-rec-pdx-999        .
      *              *-------------------------------------------------*
      *              * Delete record [pdx] per le note                 *
      *              *-------------------------------------------------*
           perform   del-rec-pdx-not-000  thru del-rec-pdx-not-999    .
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [aaf]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-aaf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-tes-tip-mag        to   rf-aaf-tip-mag         .
           move      w-tes-num-pro        to   rf-aaf-num-pro         .
           move      w-tes-cod-dcf        to   rf-aaf-cod-dcf         .
           move      w-tes-fda-pif        to   rf-aaf-fda-pif         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-aaf-ide-dat         .
           move      s-ute                to   rf-aaf-ide-ute         .
           move      s-fas                to   rf-aaf-ide-fas         .
           move      w-tes-cop-sfn (1)    to   rf-aaf-cop-sfn         .
           move      w-tes-dep-sfn (1)    to   rf-aaf-dep-sfn         .
           move      w-tes-xdp-sfn (1)    to   rf-aaf-xdp-sfn         .
           move      w-tes-snx-tum (1)    to   rf-aaf-snx-tum         .
           move      w-tes-umf-tum (1)    to   rf-aaf-umf-tum         .
           move      w-tes-nde-tum (1)    to   rf-aaf-nde-tum         .
           move      w-tes-cmo-tum (1)    to   rf-aaf-cmo-tum         .
           move      w-tes-cdi-tum (1)    to   rf-aaf-cdi-tum         .
           move      w-tes-dpz-dcf (1)    to   rf-aaf-dpz-dcf         .
           move      w-tes-tmp-cns (1)    to   rf-aaf-tmp-cns         .
           move      w-tes-sgl-vlt (1)    to   rf-aaf-sgl-vlt         .
           move      w-tes-dec-vlt (1)    to   rf-aaf-dec-vlt         .
           move      w-tes-dec-prz (1)    to   rf-aaf-dec-prz         .
           move      w-tes-tip-pza (1)    to   rf-aaf-tip-pza         .
           move      w-tes-lot-acq (1)    to   rf-aaf-lot-acq         .
           move      w-tes-tap-pes (1)    to   rf-aaf-tap-pes         .
           move      w-tes-uda-pes (1)    to   rf-aaf-uda-pes         .
           move      w-tes-lgv-vlt (1)    to   rf-aaf-lgv-vlt         .
           move      w-tes-lgv-dcv (1)    to   rf-aaf-lgv-dcv         .
           move      w-tes-lgv-tdc (1)    to   rf-aaf-lgv-tdc         .
           move      w-tes-lgv-cdc (1)    to   rf-aaf-lgv-cdc         .
           move      w-tes-lgv-pdt (1)    to   rf-aaf-lgv-pdt         .
           move      w-tes-qta-pes (1, 1) to   rf-aaf-qta-pes (1)     .
           move      w-tes-qta-pes (1, 2) to   rf-aaf-qta-pes (2)     .
           move      w-tes-qta-pes (1, 3) to   rf-aaf-qta-pes (3)     .
           move      w-tes-qta-pes (1, 4) to   rf-aaf-qta-pes (4)     .
           move      w-tes-qta-pes (1, 5) to   rf-aaf-qta-pes (5)     .
           move      w-tes-qta-pes (1, 6) to   rf-aaf-qta-pes (6)     .
           move      w-tes-prz-pes (1, 1) to   rf-aaf-prz-pes (1)     .
           move      w-tes-prz-pes (1, 2) to   rf-aaf-prz-pes (2)     .
           move      w-tes-prz-pes (1, 3) to   rf-aaf-prz-pes (3)     .
           move      w-tes-prz-pes (1, 4) to   rf-aaf-prz-pes (4)     .
           move      w-tes-prz-pes (1, 5) to   rf-aaf-prz-pes (5)     .
           move      w-tes-prz-pes (1, 6) to   rf-aaf-prz-pes (6)     .
           move      w-tes-csr-pes (1, 1) to   rf-aaf-csr-pes (1)     .
           move      w-tes-csr-pes (1, 2) to   rf-aaf-csr-pes (2)     .
           move      w-tes-csr-pes (1, 3) to   rf-aaf-csr-pes (3)     .
           move      w-tes-csr-pes (1, 4) to   rf-aaf-csr-pes (4)     .
           move      w-tes-csr-pes (1, 5) to   rf-aaf-csr-pes (5)     .
           move      w-tes-csr-pes (1, 6) to   rf-aaf-csr-pes (6)     .
           move      w-tes-psr-pes (1, 1, 1)
                                          to   rf-aaf-psr-pes (1, 1)  .
           move      w-tes-psr-pes (1, 1, 2)
                                          to   rf-aaf-psr-pes (1, 2)  .
           move      w-tes-psr-pes (1, 1, 3)
                                          to   rf-aaf-psr-pes (1, 3)  .
           move      w-tes-psr-pes (1, 1, 4)
                                          to   rf-aaf-psr-pes (1, 4)  .
           move      w-tes-psr-pes (1, 1, 5)
                                          to   rf-aaf-psr-pes (1, 5)  .
           move      w-tes-psr-pes (1, 2, 1)
                                          to   rf-aaf-psr-pes (2, 1)  .
           move      w-tes-psr-pes (1, 2, 2)
                                          to   rf-aaf-psr-pes (2, 2)  .
           move      w-tes-psr-pes (1, 2, 3)
                                          to   rf-aaf-psr-pes (2, 3)  .
           move      w-tes-psr-pes (1, 2, 4)
                                          to   rf-aaf-psr-pes (2, 4)  .
           move      w-tes-psr-pes (1, 2, 5)
                                          to   rf-aaf-psr-pes (2, 5)  .
           move      w-tes-psr-pes (1, 3, 1)
                                          to   rf-aaf-psr-pes (3, 1)  .
           move      w-tes-psr-pes (1, 3, 2)
                                          to   rf-aaf-psr-pes (3, 2)  .
           move      w-tes-psr-pes (1, 3, 3)
                                          to   rf-aaf-psr-pes (3, 3)  .
           move      w-tes-psr-pes (1, 3, 4)
                                          to   rf-aaf-psr-pes (3, 4)  .
           move      w-tes-psr-pes (1, 3, 5)
                                          to   rf-aaf-psr-pes (3, 5)  .
           move      w-tes-psr-pes (1, 4, 1)
                                          to   rf-aaf-psr-pes (4, 1)  .
           move      w-tes-psr-pes (1, 4, 2)
                                          to   rf-aaf-psr-pes (4, 2)  .
           move      w-tes-psr-pes (1, 4, 3)
                                          to   rf-aaf-psr-pes (4, 3)  .
           move      w-tes-psr-pes (1, 4, 4)
                                          to   rf-aaf-psr-pes (4, 4)  .
           move      w-tes-psr-pes (1, 4, 5)
                                          to   rf-aaf-psr-pes (4, 5)  .
           move      w-tes-psr-pes (1, 5, 1)
                                          to   rf-aaf-psr-pes (5, 1)  .
           move      w-tes-psr-pes (1, 5, 2)
                                          to   rf-aaf-psr-pes (5, 2)  .
           move      w-tes-psr-pes (1, 5, 3)
                                          to   rf-aaf-psr-pes (5, 3)  .
           move      w-tes-psr-pes (1, 5, 4)
                                          to   rf-aaf-psr-pes (5, 4)  .
           move      w-tes-psr-pes (1, 5, 5)
                                          to   rf-aaf-psr-pes (5, 5)  .
           move      w-tes-psr-pes (1, 6, 1)
                                          to   rf-aaf-psr-pes (6, 1)  .
           move      w-tes-psr-pes (1, 6, 2)
                                          to   rf-aaf-psr-pes (6, 2)  .
           move      w-tes-psr-pes (1, 6, 3)
                                          to   rf-aaf-psr-pes (6, 3)  .
           move      w-tes-psr-pes (1, 6, 4)
                                          to   rf-aaf-psr-pes (6, 4)  .
           move      w-tes-psr-pes (1, 6, 5)
                                          to   rf-aaf-psr-pes (6, 5)  .
           move      w-tes-per-mpa (1)    to   rf-aaf-per-mpa         .
           move      w-tes-rif-lst (1)    to   rf-aaf-rif-lst         .
           move      w-tes-per-ric (1)    to   rf-aaf-per-ric         .
           move      w-tes-alx-exp (1)    to   rf-aaf-alx-exp         .
       cmp-rec-aaf-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [aaf]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-aaf-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-aaf-000      thru cmp-rec-aaf-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
       wrt-rec-aaf-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [aaf]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-aaf-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-aaf-000      thru cmp-rec-aaf-999        .
      *              *-------------------------------------------------*
      *              * Forced put record                               *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
       rew-rec-aaf-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [aaf]                                *
      *    *-----------------------------------------------------------*
       del-rec-aaf-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-aaf-000      thru cmp-rec-aaf-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
       del-rec-aaf-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [pdx]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-pdx-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           if        w-tes-tip-mag        =    01
                     move  13             to   rf-pdx-tip-rec
           else if   w-tes-tip-mag        =    03
                     move  33             to   rf-pdx-tip-rec
           else if   w-tes-tip-mag        =    04
                     move  43             to   rf-pdx-tip-rec         .
           move      w-tes-cod-dcf        to   rf-pdx-cod-arc         .
           move      spaces               to   rf-pdx-cod-lng         .
           move      w-tes-num-pro        to   rf-pdx-cod-num         .
           move      w-tes-fda-pif        to   rf-pdx-for-mat         .
           move      w-cix-ctr-001        to   rf-pdx-num-prg         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      w-tes-dep-rig
                    (1, w-cix-ctr-001)    to   rf-pdx-des-pro         .
       cmp-rec-pdx-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [pdx]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-pdx-000.
      *              *-------------------------------------------------*
      *              * Ciclo per scrittura fino a 10 righe di descri-  *
      *              * zione prodotto                                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-cix-ctr-001          .
      *                  *---------------------------------------------*
      *                  * Test se record da scrivere                  *
      *                  *---------------------------------------------*
           if        w-tes-xdp-sfn (1)    =    0
                     go to wrt-rec-pdx-500.
       wrt-rec-pdx-100.
           add       1                    to   w-cix-ctr-001          .
           if        w-cix-ctr-001        >    10
                     go to wrt-rec-pdx-999.
           if        w-tes-dep-rig
                    (1, w-cix-ctr-001)    =    spaces
                     go to wrt-rec-pdx-500.
      *                  *---------------------------------------------*
      *                  * Composizione record [pdx]                   *
      *                  *---------------------------------------------*
           perform   cmp-rec-pdx-000      thru cmp-rec-pdx-999        .
      *                  *---------------------------------------------*
      *                  * Put record                                  *
      *                  *---------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     wrt-rec-pdx-100.
       wrt-rec-pdx-500.
      *              *-------------------------------------------------*
      *              * Riposizionamento contatore                      *
      *              *-------------------------------------------------*
           subtract  1                    from w-cix-ctr-001          .
      *              *-------------------------------------------------*
      *              * Ciclo per cancellazione righe di descrizione    *
      *              * prodotto rimanenti                              *
      *              *-------------------------------------------------*
       wrt-rec-pdx-520.
           add       1                    to   w-cix-ctr-001          .
           if        w-cix-ctr-001        >    10
                     go to wrt-rec-pdx-999.
      *                  *---------------------------------------------*
      *                  * Composizione campi chiave record            *
      *                  *---------------------------------------------*
           if        w-tes-tip-mag        =    01
                     move  13             to   rf-pdx-tip-rec
           else if   w-tes-tip-mag        =    03
                     move  33             to   rf-pdx-tip-rec
           else if   w-tes-tip-mag        =    04
                     move  43             to   rf-pdx-tip-rec         .
           move      w-tes-cod-dcf        to   rf-pdx-cod-arc         .
           move      spaces               to   rf-pdx-cod-lng         .
           move      w-tes-num-pro        to   rf-pdx-cod-num         .
           move      w-tes-fda-pif        to   rf-pdx-for-mat         .
           move      w-cix-ctr-001        to   rf-pdx-num-prg         .
      *                  *---------------------------------------------*
      *                  * Delete record                               *
      *                  *---------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     wrt-rec-pdx-520.
       wrt-rec-pdx-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [pdx]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-pdx-000.
      *              *-------------------------------------------------*
      *              * Ciclo per riscrittura fino a 10 righe di descri-*
      *              * zione prodotto                                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-cix-ctr-001          .
      *                  *---------------------------------------------*
      *                  * Test se record da riscrivere                *
      *                  *---------------------------------------------*
           if        w-tes-xdp-sfn (1)    =    0
                     go to rew-rec-pdx-500.
       rew-rec-pdx-100.
           add       1                    to   w-cix-ctr-001          .
           if        w-cix-ctr-001        >    10
                     go to rew-rec-pdx-999.
           if        w-tes-dep-rig
                    (1, w-cix-ctr-001)    =    spaces
                     go to rew-rec-pdx-500.
      *                  *---------------------------------------------*
      *                  * Composizione record [pdx]                   *
      *                  *---------------------------------------------*
           perform   cmp-rec-pdx-000      thru cmp-rec-pdx-999        .
      *                  *---------------------------------------------*
      *                  * Forced put record                           *
      *                  *---------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     rew-rec-pdx-100.
       rew-rec-pdx-500.
      *              *-------------------------------------------------*
      *              * Riposizionamento contatore                      *
      *              *-------------------------------------------------*
           subtract  1                    from w-cix-ctr-001          .
      *              *-------------------------------------------------*
      *              * Ciclo per cancellazione righe di descrizione    *
      *              * prodotto rimanenti                              *
      *              *-------------------------------------------------*
       rew-rec-pdx-520.
           add       1                    to   w-cix-ctr-001          .
           if        w-cix-ctr-001        >    10
                     go to rew-rec-pdx-999.
      *                  *---------------------------------------------*
      *                  * Composizione campi chiave record            *
      *                  *---------------------------------------------*
           if        w-tes-tip-mag        =    01
                     move  13             to   rf-pdx-tip-rec
           else if   w-tes-tip-mag        =    03
                     move  33             to   rf-pdx-tip-rec
           else if   w-tes-tip-mag        =    04
                     move  43             to   rf-pdx-tip-rec         .
           move      w-tes-cod-dcf        to   rf-pdx-cod-arc         .
           move      spaces               to   rf-pdx-cod-lng         .
           move      w-tes-num-pro        to   rf-pdx-cod-num         .
           move      w-tes-fda-pif        to   rf-pdx-for-mat         .
           move      w-cix-ctr-001        to   rf-pdx-num-prg         .
      *                  *---------------------------------------------*
      *                  * Delete record                               *
      *                  *---------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     rew-rec-pdx-520.
       rew-rec-pdx-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [pdx]                                *
      *    *-----------------------------------------------------------*
       del-rec-pdx-000.
      *              *-------------------------------------------------*
      *              * Ciclo per cancellazione fino a 10 righe di de-  *
      *              * scrizione prodotto                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-cix-ctr-001          .
       del-rec-pdx-100.
           add       1                    to   w-cix-ctr-001          .
           if        w-cix-ctr-001        >    10
                     go to del-rec-pdx-999.
      *                  *---------------------------------------------*
      *                  * Composizione record [pdx]                   *
      *                  *---------------------------------------------*
           perform   cmp-rec-pdx-000      thru cmp-rec-pdx-999        .
      *                  *---------------------------------------------*
      *                  * Delete record                               *
      *                  *---------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     del-rec-pdx-100.
       del-rec-pdx-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [pdx] per le annotazioni              *
      *    *-----------------------------------------------------------*
       cmp-rec-pdx-not-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           if        w-tes-tip-mag        =    01
                     move  14             to   rf-pdx-tip-rec
           else if   w-tes-tip-mag        =    03
                     move  34             to   rf-pdx-tip-rec
           else if   w-tes-tip-mag        =    04
                     move  44             to   rf-pdx-tip-rec         .
           move      w-tes-cod-dcf        to   rf-pdx-cod-arc         .
           move      spaces               to   rf-pdx-cod-lng         .
           move      w-tes-num-pro        to   rf-pdx-cod-num         .
           move      w-tes-fda-pif        to   rf-pdx-for-mat         .
           move      w-cix-ctr-001        to   rf-pdx-num-prg         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      w-tes-ann-rig
                    (1, w-cix-ctr-001)    to   rf-pdx-des-pro         .
       cmp-rec-pdx-not-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [pdx]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-pdx-not-000.
      *              *-------------------------------------------------*
      *              * Test se record da scrivere                      *
      *              *-------------------------------------------------*
           if        w-tes-ann-not (1)    =    spaces
                     go to wrt-rec-pdx-not-999.
       wrt-rec-pdx-not-050.
      *              *-------------------------------------------------*
      *              * Ciclo per scrittura fino a 10 righe di descri-  *
      *              * zione prodotto                                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-cix-ctr-001          .
       wrt-rec-pdx-not-100.
           add       1                    to   w-cix-ctr-001          .
           if        w-cix-ctr-001        >    10
                     go to wrt-rec-pdx-not-999.
           if        w-tes-ann-rig
                    (1, w-cix-ctr-001)    =    spaces
                     go to wrt-rec-pdx-not-500.
      *                  *---------------------------------------------*
      *                  * Composizione record [pdx]                   *
      *                  *---------------------------------------------*
           perform   cmp-rec-pdx-not-000  thru cmp-rec-pdx-not-999    .
      *                  *---------------------------------------------*
      *                  * Put record                                  *
      *                  *---------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     wrt-rec-pdx-not-100.
       wrt-rec-pdx-not-500.
      *              *-------------------------------------------------*
      *              * Riposizionamento contatore                      *
      *              *-------------------------------------------------*
           subtract  1                    from w-cix-ctr-001          .
      *              *-------------------------------------------------*
      *              * Ciclo per cancellazione righe di descrizione    *
      *              * prodotto rimanenti                              *
      *              *-------------------------------------------------*
       wrt-rec-pdx-not-520.
           add       1                    to   w-cix-ctr-001          .
           if        w-cix-ctr-001        >    10
                     go to wrt-rec-pdx-not-999.
      *                  *---------------------------------------------*
      *                  * Composizione campi chiave record            *
      *                  *---------------------------------------------*
           if        w-tes-tip-mag        =    01
                     move  14             to   rf-pdx-tip-rec
           else if   w-tes-tip-mag        =    03
                     move  34             to   rf-pdx-tip-rec
           else if   w-tes-tip-mag        =    04
                     move  44             to   rf-pdx-tip-rec         .
           move      w-tes-cod-dcf        to   rf-pdx-cod-arc         .
           move      spaces               to   rf-pdx-cod-lng         .
           move      w-tes-num-pro        to   rf-pdx-cod-num         .
           move      w-tes-fda-pif        to   rf-pdx-for-mat         .
           move      w-cix-ctr-001        to   rf-pdx-num-prg         .
      *                  *---------------------------------------------*
      *                  * Delete record                               *
      *                  *---------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     wrt-rec-pdx-not-520.
       wrt-rec-pdx-not-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [pdx]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-pdx-not-000.
      *              *-------------------------------------------------*
      *              * Test se record da riscrivere                    *
      *              *-------------------------------------------------*
           if        w-tes-ann-not (1)    =    spaces
                     move  1              to   w-cix-ctr-001
                     go to rew-rec-pdx-not-500.
       rew-rec-pdx-not-050.
      *              *-------------------------------------------------*
      *              * Ciclo di riscrittura fino a 10 righe di descri- *
      *              * zione prodotto                                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-cix-ctr-001          .
       rew-rec-pdx-not-100.
           add       1                    to   w-cix-ctr-001          .
           if        w-cix-ctr-001        >    10
                     go to rew-rec-pdx-not-999.
           if        w-tes-ann-rig
                    (1, w-cix-ctr-001)    =    spaces
                     go to rew-rec-pdx-not-500.
      *                  *---------------------------------------------*
      *                  * Composizione record [pdx]                   *
      *                  *---------------------------------------------*
           perform   cmp-rec-pdx-not-000  thru cmp-rec-pdx-not-999    .
      *                  *---------------------------------------------*
      *                  * Forced put record                           *
      *                  *---------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     rew-rec-pdx-not-100.
       rew-rec-pdx-not-500.
      *              *-------------------------------------------------*
      *              * Riposizionamento contatore                      *
      *              *-------------------------------------------------*
           subtract  1                    from w-cix-ctr-001          .
      *              *-------------------------------------------------*
      *              * Ciclo per cancellazione righe di descrizione    *
      *              * prodotto rimanenti                              *
      *              *-------------------------------------------------*
       rew-rec-pdx-not-520.
           add       1                    to   w-cix-ctr-001          .
           if        w-cix-ctr-001        >    10
                     go to rew-rec-pdx-not-999.
      *                  *---------------------------------------------*
      *                  * Composizione campi chiave record            *
      *                  *---------------------------------------------*
           if        w-tes-tip-mag        =    01
                     move  14             to   rf-pdx-tip-rec
           else if   w-tes-tip-mag        =    03
                     move  34             to   rf-pdx-tip-rec
           else if   w-tes-tip-mag        =    04
                     move  44             to   rf-pdx-tip-rec         .
           move      w-tes-cod-dcf        to   rf-pdx-cod-arc         .
           move      spaces               to   rf-pdx-cod-lng         .
           move      w-tes-num-pro        to   rf-pdx-cod-num         .
           move      w-tes-fda-pif        to   rf-pdx-for-mat         .
           move      w-cix-ctr-001        to   rf-pdx-num-prg         .
      *                  *---------------------------------------------*
      *                  * Delete record                               *
      *                  *---------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     rew-rec-pdx-not-520.
       rew-rec-pdx-not-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [pdx]                                *
      *    *-----------------------------------------------------------*
       del-rec-pdx-not-000.
      *              *-------------------------------------------------*
      *              * Ciclo per cancellazione fino a 10 righe di de-  *
      *              * scrizione prodotto                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-cix-ctr-001          .
       del-rec-pdx-not-100.
           add       1                    to   w-cix-ctr-001          .
           if        w-cix-ctr-001        >    10
                     go to del-rec-pdx-not-999.
      *                  *---------------------------------------------*
      *                  * Composizione record [pdx]                   *
      *                  *---------------------------------------------*
           perform   cmp-rec-pdx-not-000  thru cmp-rec-pdx-not-999    .
      *                  *---------------------------------------------*
      *                  * Delete record                               *
      *                  *---------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     del-rec-pdx-not-100.
       del-rec-pdx-not-999.
           exit.

      *    *===========================================================*
      *    * Aggiornamento record [dcp]                                *
      *    *                                                           *
      *    * N.B.: per aggiornare il codice del prodotto secondo il    *
      *    *       fornitore preferenziale, riportato nel record [dcp] *
      *    *       per comodita' nell'uso dei filtri di selezione      *
      *    *-----------------------------------------------------------*
       agg-rec-dcp-000.
      *              *-------------------------------------------------*
      *              * Test se aggiornamento necessario                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice numerico prodotto             *
      *                  *---------------------------------------------*
           if        w-tes-num-pro        =    zero
                     go to agg-rec-dcp-999.
      *                  *---------------------------------------------*
      *                  * Test su codice fornitore                    *
      *                  *---------------------------------------------*
           if        w-tes-cod-dcf        =    zero
                     go to agg-rec-dcp-999.
      *                  *---------------------------------------------*
      *                  * Test se fornitore preferenziale             *
      *                  *---------------------------------------------*
           move      w-tes-tip-mag        to   w-let-arc-aaq-tip      .
           move      w-tes-num-pro        to   w-let-arc-aaq-num      .
           perform   let-arc-aaq-000      thru let-arc-aaq-999        .
           if        w-let-arc-aaq-pfz    not  = w-tes-cod-dcf
                     go to agg-rec-dcp-999.
       agg-rec-dcp-050.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [dcp]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
       agg-rec-dcp-100.
      *              *-------------------------------------------------*
      *              * Lettura record [dcp]                            *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "NUMPRO    "         to   f-key                  .
           move      w-tes-num-pro        to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
           if        f-sts                not  = e-not-err
                     go to agg-rec-dcp-999.
       agg-rec-dcp-200.
      *              *-------------------------------------------------*
      *              * Aggiornamento record [dcp]                      *
      *              *-------------------------------------------------*
           move      w-tes-cop-sfn (1)    to   rf-dcp-cop-sfn         .
       agg-rec-dcp-400.
      *              *-------------------------------------------------*
      *              * Update record                                   *
      *              *-------------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * Release                                         *
      *              *-------------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
       agg-rec-dcp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     agg-rec-dcp-999.
       agg-rec-dcp-999.
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
      *    * Determinazione testo contenuto in [pdx]                   *
      *    *-----------------------------------------------------------*
       det-txt-pdx-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di esito                   *
      *              *-------------------------------------------------*
           move      "N"                  to   w-det-txt-pdx-snx      .
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-txt-pdx-ctr      .
      *              *-------------------------------------------------*
      *              * Normalizzazione valore di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-txt-pdx-txt      .
       det-txt-pdx-100.
      *              *-------------------------------------------------*
      *              * Start su file [pdx]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "TRCLNG    "         to   f-key                  .
           move      w-det-txt-pdx-tip    to   rf-pdx-tip-rec         .
           move      w-det-txt-pdx-arc    to   rf-pdx-cod-arc         .
           move      spaces               to   rf-pdx-cod-lng         .
           move      w-det-txt-pdx-mag    to   rf-pdx-cod-num         .
           move      w-det-txt-pdx-fda    to   rf-pdx-for-mat         .
           move      zero                 to   rf-pdx-num-prg         .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-txt-pdx-999.
       det-txt-pdx-200.
      *              *-------------------------------------------------*
      *              * Next su [pdx]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : a test di uscita              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-txt-pdx-800.
       det-txt-pdx-300.
      *              *-------------------------------------------------*
      *              * Max su [pdx]                                    *
      *              *-------------------------------------------------*
           if        rf-pdx-tip-rec       not  = w-det-txt-pdx-tip or
                     rf-pdx-cod-arc       not  = w-det-txt-pdx-arc or
                     rf-pdx-cod-lng       not  = spaces            or
                     rf-pdx-cod-num       not  = w-det-txt-pdx-mag or
                     rf-pdx-for-mat       not  = w-det-txt-pdx-fda
                     go to det-txt-pdx-800.
       det-txt-pdx-400.
      *              *-------------------------------------------------*
      *              * Selezioni su [pdx]                              *
      *              *-------------------------------------------------*
       det-txt-pdx-600.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   w-det-txt-pdx-ctr      .
           if        w-det-txt-pdx-ctr    >    10
                     move  10             to   w-det-txt-pdx-ctr      .
      *              *-------------------------------------------------*
      *              * Bufferizzazione                                 *
      *              *-------------------------------------------------*
           move      rf-pdx-des-pro       to   w-det-txt-pdx-rig
                                              (w-det-txt-pdx-ctr)     .
       det-txt-pdx-700.
      *              *-------------------------------------------------*
      *              * Riciclo a record [pdx] successivo               *
      *              *-------------------------------------------------*
           go to     det-txt-pdx-200.
       det-txt-pdx-800.
      *              *-------------------------------------------------*
      *              * Determinazione finale                           *
      *              *-------------------------------------------------*
           if        w-det-txt-pdx-ctr    >    zero
                     go to det-txt-pdx-900
           else      go to det-txt-pdx-999.
       det-txt-pdx-900.
      *              *-------------------------------------------------*
      *              * Uscita per righe trovate                        *
      *              *-------------------------------------------------*
           move      "S"                  to   w-det-txt-pdx-snx      .
       det-txt-pdx-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [aaq]                         *
      *    *-----------------------------------------------------------*
       let-arc-aaq-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-aaq-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-aaq-tip    =    zero and
                     w-let-arc-aaq-num    =    zero
                     go to let-arc-aaq-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice e tipo                       *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO"             to   f-key                  .
           move      w-let-arc-aaq-tip    to   rf-aaq-tip-mag         .
           move      w-let-arc-aaq-num    to   rf-aaq-num-pro         .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-aaq-400.
       let-arc-aaq-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-aaq-sgl-vlt       to   w-let-arc-aaq-vlt      .
           move      rf-aaq-dec-vlt       to   w-let-arc-aaq-ndv      .
           move      rf-aaq-dec-prz       to   w-let-arc-aaq-ndp      .
           move      rf-aaq-prz-acr       to   w-let-arc-aaq-prz      .
           move      rf-aaq-tmp-apv       to   w-let-arc-aaq-tmp      .
           move      rf-aaq-dcf-pfz       to   w-let-arc-aaq-pfz      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-aaq-999.
       let-arc-aaq-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-aaq-flg      .
           go to     let-arc-aaq-600.
       let-arc-aaq-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
       let-arc-aaq-600.
           move      spaces               to   w-let-arc-aaq-vlt      .
           move      zero                 to   w-let-arc-aaq-ndv      .
           move      zero                 to   w-let-arc-aaq-ndp      .
           move      zero                 to   w-let-arc-aaq-prz      .
           move      zero                 to   w-let-arc-aaq-tmp      .
           move      zero                 to   w-let-arc-aaq-pfz      .
       let-arc-aaq-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [dcp]                         *
      *    *-----------------------------------------------------------*
       let-arc-dcp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcp-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-dcp-num    =    zero  
                     go to let-arc-dcp-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO"             to   f-key                  .
           move      w-let-arc-dcp-num    to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-dcp-400.
       let-arc-dcp-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           if        rf-dcp-tip-pro       =    01
                     move  01             to   w-let-arc-dcp-tip
           else if   rf-dcp-tip-pro       =    02
                     move  02             to   w-let-arc-dcp-tip
           else if   rf-dcp-tip-pro       =    03
                     move  03             to   w-let-arc-dcp-tip
           else if   rf-dcp-tip-pro       =    09
                     move  04             to   w-let-arc-dcp-tip
           else      move  zero           to   w-let-arc-dcp-tip      .
      *
           move      rf-dcp-alf-pro       to   w-let-arc-dcp-alf      .
           move      rf-dcp-des-pro       to   w-let-arc-dcp-des      .
           move      rf-dcp-umi-ven       to   w-let-arc-dcp-umi      .
           move      rf-dcp-dec-qta       to   w-let-arc-dcp-deq      .
           move      rf-dcp-dec-prz       to   w-let-arc-dcp-dep      .
           move      rf-dcp-cod-iva       to   w-let-arc-dcp-civ      .
           move      rf-dcp-prz-lst       to   w-let-arc-dcp-plb      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dcp-999.
       let-arc-dcp-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dcp-flg      .
           move      all   "."            to   w-let-arc-dcp-des      .
           go to     let-arc-dcp-600.
       let-arc-dcp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcp-des      .
       let-arc-dcp-600.
           move      spaces               to   w-let-arc-dcp-alf      .
           move      zero                 to   w-let-arc-dcp-tip      .
           move      spaces               to   w-let-arc-dcp-umi      .
           move      zero                 to   w-let-arc-dcp-deq      .
           move      zero                 to   w-let-arc-dcp-dep      .
           move      zero                 to   w-let-arc-dcp-civ      .
           move      zero                 to   w-let-arc-dcp-plb      .
       let-arc-dcp-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [dpm]                         *
      *    *-----------------------------------------------------------*
       let-arc-dpm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dpm-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-dpm-num    =    zero  
                     go to let-arc-dpm-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMMAP"             to   f-key                  .
           move      w-let-arc-dpm-num    to   rf-dpm-num-map         .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-dpm-400.
       let-arc-dpm-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-dpm-alf-map       to   w-let-arc-dpm-alf      .
           move      rf-dpm-des-map       to   w-let-arc-dpm-des      .
           move      rf-dpm-umi-prd       to   w-let-arc-dpm-umi      .
           move      rf-dpm-dec-qta       to   w-let-arc-dpm-deq      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dpm-999.
       let-arc-dpm-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dpm-flg      .
           move      all   "."            to   w-let-arc-dpm-des      .
           go to     let-arc-dpm-600.
       let-arc-dpm-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dpm-des      .
       let-arc-dpm-600.
           move      spaces               to   w-let-arc-dpm-alf      .
           move      spaces               to   w-let-arc-dpm-umi      .
           move      zero                 to   w-let-arc-dpm-deq      .
       let-arc-dpm-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [mtv]                         *
      *    *-----------------------------------------------------------*
       let-arc-mtv-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-mtv-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-mtv-num    =    zero  
                     go to let-arc-mtv-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMMTV"             to   f-key                  .
           move      w-let-arc-mtv-num    to   rf-mtv-num-mtv         .
           move      "pgm/mtv/fls/ioc/obj/iofmtv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-mtv-400.
       let-arc-mtv-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-mtv-alf-mtv       to   w-let-arc-mtv-alf      .
           move      rf-mtv-des-mtv       to   w-let-arc-mtv-des      .
           move      rf-mtv-umi-gst       to   w-let-arc-mtv-umi      .
           move      rf-mtv-dec-qta       to   w-let-arc-mtv-deq      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-mtv-999.
       let-arc-mtv-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-mtv-flg      .
           move      all   "."            to   w-let-arc-mtv-des      .
           go to     let-arc-mtv-600.
       let-arc-mtv-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-mtv-des      .
       let-arc-mtv-600.
           move      spaces               to   w-let-arc-mtv-alf      .
           move      spaces               to   w-let-arc-mtv-umi      .
           move      zero                 to   w-let-arc-mtv-deq      .
       let-arc-mtv-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zvl]                             *
      *    *-----------------------------------------------------------*
       let-arc-zvl-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zvl-flg      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del valore della sigla va- *
      *              * luta                                            *
      *              *-------------------------------------------------*
           if        w-let-arc-zvl-cod    =    c-sgl
                     go to let-arc-zvl-200
           else if   w-let-arc-zvl-cod    =    spaces
                     go to let-arc-zvl-400
           else      go to let-arc-zvl-600.
       let-arc-zvl-200.
      *              *-------------------------------------------------*
      *              * Se sigla valuta pari alla valuta base           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Descrizione valuta in lingua italiana       *
      *                  *---------------------------------------------*
           move      c-des                to   w-let-arc-zvl-des      .
      *                  *---------------------------------------------*
      *                  * Descrizione valuta internazionale           *
      *                  *---------------------------------------------*
           move      c-din                to   w-let-arc-zvl-din      .
      *                  *---------------------------------------------*
      *                  * Numero decimali valuta                      *
      *                  *---------------------------------------------*
           move      c-dec                to   w-let-arc-zvl-dec      .
      *                  *---------------------------------------------*
      *                  * Tipo di coefficiente per cambio valuta      *
      *                  *---------------------------------------------*
           move      c-tdc                to   w-let-arc-zvl-tdc      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-zvl-999.
       let-arc-zvl-400.
      *              *-------------------------------------------------*
      *              * Se sigla valuta a Spaces                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Descrizione valuta in lingua italiana       *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-zvl-des      .
      *                  *---------------------------------------------*
      *                  * Descrizione valuta internazionale           *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-zvl-din      .
      *                  *---------------------------------------------*
      *                  * Numero decimali valuta                      *
      *                  *---------------------------------------------*
           move      zero                 to   w-let-arc-zvl-dec      .
      *                  *---------------------------------------------*
      *                  * Tipo di coefficiente per cambio valuta      *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-zvl-tdc      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-zvl-999.
       let-arc-zvl-600.
      *              *-------------------------------------------------*
      *              * Se sigla valuta diversa dalla valuta base ed    *
      *              * anche diversa da Spaces                         *
      *              *-------------------------------------------------*
       let-arc-zvl-650.
      *                  *---------------------------------------------*
      *                  * Lettura per sigla valuta                    *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODVLT    "         to   f-key                  .
           move      w-let-arc-zvl-cod    to   rf-zvl-sgl-vlt         .
           move      "pgm/dcc/fls/ioc/obj/iofzvl"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvl                 .
      *                  *---------------------------------------------*
      *                  * Deviazione secondo l'esito della lettura    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-arc-zvl-750.
       let-arc-zvl-700.
      *                  *---------------------------------------------*
      *                  * Se record esistente                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione valuta in lingua italiana   *
      *                      *-----------------------------------------*
           move      rf-zvl-des-ita       to   w-let-arc-zvl-des      .
      *                      *-----------------------------------------*
      *                      * Descrizione valuta internazionale       *
      *                      *-----------------------------------------*
           move      rf-zvl-des-int       to   w-let-arc-zvl-din      .
      *                      *-----------------------------------------*
      *                      * Numero decimali valuta                  *
      *                      *-----------------------------------------*
           move      rf-zvl-num-dec       to   w-let-arc-zvl-dec      .
      *                      *-----------------------------------------*
      *                      * Tipo di coefficiente per cambio valuta  *
      *                      *-----------------------------------------*
           move      rf-zvl-def-tdc       to   w-let-arc-zvl-tdc      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-arc-zvl-999.
       let-arc-zvl-750.
      *                  *---------------------------------------------*
      *                  * Se record non esistente                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita ad errore                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-let-arc-zvl-flg      .
      *                      *-----------------------------------------*
      *                      * Descrizione valuta in lingua italiana   *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-zvl-des      .
      *                      *-----------------------------------------*
      *                      * Descrizione valuta internazionale       *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-zvl-din      .
      *                      *-----------------------------------------*
      *                      * Numero decimali valuta                  *
      *                      *-----------------------------------------*
           move      zero                 to   w-let-arc-zvl-dec      .
      *                      *-----------------------------------------*
      *                      * Tipo di coefficiente per cambio valuta  *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-zvl-tdc      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-arc-zvl-999.
       let-arc-zvl-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio [fnt]                            *
      *    *-----------------------------------------------------------*
       let-arc-fnt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-fnt-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice fornitore a zero                 *
      *              *-------------------------------------------------*
           if        w-let-arc-fnt-cod    =    zero
                     go to let-arc-fnt-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFNT    "         to   f-key                  .
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
           go to     let-arc-fnt-600.
       let-arc-fnt-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-fnt-rag      .
       let-arc-fnt-600.
           move      spaces               to   w-let-arc-fnt-via      .
           move      spaces               to   w-let-arc-fnt-loc      .
           move      zero                 to   w-let-arc-fnt-piv      .
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
      *              * Test se codice fornitore a zero                 *
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
           move      rf-dcf-cod-vlt       to   w-let-arc-dcf-vlt      .
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
           go to     let-arc-dcf-600.
       let-arc-dcf-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcf-rag      .
       let-arc-dcf-600.
           move      spaces               to   w-let-arc-dcf-vlt      .
       let-arc-dcf-999.
           exit.

      *    *===========================================================*
      *    * Determinazione tipi magazzino ammessi                     *
      *    *-----------------------------------------------------------*
       det-tpm-amm-000.
      *              *-------------------------------------------------*
      *              * Preparazione area w-exp limitata ai soli tipi   *
      *              * magazzino previsti                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo magazzino 'Prodotto di vendita' :      *
      *                  * - Sempre ammesso                            *
      *                  *---------------------------------------------*
           move      01                   to   w-exp-tpm-amm-tpm (1)  .
      *                  *---------------------------------------------*
      *                  * Tipo magazzino 'Materia Prima' :            *
      *                  * - Se gestione materie prime attiva lo si    *
      *                  *   inserisce nella lista, altrimenti no      *
      *                  *---------------------------------------------*
           if        w-prs-dpm-snx        =    "S"
                     move  03             to   w-exp-tpm-amm-tpm (2)
           else      move  zero           to   w-exp-tpm-amm-tpm (2)  .
      *                  *---------------------------------------------*
      *                  * Tipo magazzino 'Materiale Vario' :          *
      *                  * - Se gestione materiale vario attiva lo si  *
      *                  *   inserisce nella lista, altrimenti no      *
      *                  *---------------------------------------------*
           if        w-prs-mtv-snx        =    "S"
                     move  04             to   w-exp-tpm-amm-tpm (3)
           else      move  zero           to   w-exp-tpm-amm-tpm (3)  .
      *              *-------------------------------------------------*
      *              * Compattamento della lista dei tipi magazzino    *
      *              * ammessi                                         *
      *              *-------------------------------------------------*
           if        w-exp-tpm-amm-tpm (1)
                                          =    zero
                     move  w-exp-tpm-amm-tpm (2)
                                          to   w-exp-tpm-amm-tpm (1)
                     move  w-exp-tpm-amm-tpm (3)
                                          to   w-exp-tpm-amm-tpm (2)
                     move  zero           to   w-exp-tpm-amm-tpm (3)  .
           if        w-exp-tpm-amm-tpm (2)
                                          =    zero
                     move  w-exp-tpm-amm-tpm (3)
                                          to   w-exp-tpm-amm-tpm (2)
                     move  zero           to   w-exp-tpm-amm-tpm (3)  .
      *              *-------------------------------------------------*
      *              * Preparazione del numero di elementi in tabella  *
      *              * tipi magazzino ammessi                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-exp-tpm-amm-num      .
       det-tpm-amm-020.
           add       1                    to   w-exp-tpm-amm-num      .
           if        w-exp-tpm-amm-num    >    3
                     move  3              to   w-exp-tpm-amm-num
                     go to det-tpm-amm-040.
           if        w-exp-tpm-amm-tpm
                    (w-exp-tpm-amm-num)   not  = zero
                     go to det-tpm-amm-020.
           subtract  1                    from w-exp-tpm-amm-num      .
       det-tpm-amm-040.
      *              *-------------------------------------------------*
      *              * Preparazione delle descrizioni relative ai tipi *
      *              * magazzino ammessi                               *
      *              *-------------------------------------------------*
           move      zero                 to   w-exp-tpm-amm-c01      .
       det-tpm-amm-100.
           add       1                    to   w-exp-tpm-amm-c01      .
           if        w-exp-tpm-amm-c01    >    w-exp-tip-mag-num
                     go to det-tpm-amm-999.
           move      zero                 to   w-exp-tpm-amm-c02      .
       det-tpm-amm-120.
           add       1                    to   w-exp-tpm-amm-c02      .
           if        w-exp-tpm-amm-c02    >    3
                     go to det-tpm-amm-100.
           if        w-exp-tpm-amm-tpm
                    (w-exp-tpm-amm-c01)   not  = w-exp-tip-mag-tpm
                                                (w-exp-tpm-amm-c02)
                     go to det-tpm-amm-120.
           move      w-exp-tip-mag-ele
                    (w-exp-tpm-amm-c02)   to   w-exp-tpm-amm-ele
                                              (w-exp-tpm-amm-c01)     .
           go to     det-tpm-amm-100.
       det-tpm-amm-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per accettazione codice prodotto d'acquisto   *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acodaaq0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice valuta          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acodzvl0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice fornitore com-  *
      *    * merciale                                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmndcf0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione formato di acquisizione    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acodaaf0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

