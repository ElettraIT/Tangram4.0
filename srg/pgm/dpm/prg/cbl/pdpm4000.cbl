       Identification Division.
       Program-Id.                                 pdpm4000           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dpm                 *
      *                                Settore:    prd                 *
      *                                   Fase:    dpm400              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 07/06/91    *
      *                       Ultima revisione:    NdK del 15/10/18    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione anagrafica materie prime           *
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
                     "dpm"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "prd"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "dpm400"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pdpm4000"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "     DATI ANAGRAFICI MATERIE PRIME      "       .

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
      *    * Area di definizione della valuta base                     *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/c"                                  .

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
      *            *---------------------------------------------------*
      *            * Numero relativo pagine testata gestite            *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-ntt      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Numero relativo pagina testata visualizzata       *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-nrt      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Comodi per numeri pagine testata gestite          *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-ntw      pic  9(02)                  .
               10  w-cnt-sts-imp-nts      pic  9(02)                  .
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
      *        * [dpm]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dpm/fls/rec/rfdpm"                          .
      *        *-------------------------------------------------------*
      *        * [zm1]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dpm/fls/rec/rfzm1"                          .
      *        *-------------------------------------------------------*
      *        * [zm2]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dpm/fls/rec/rfzm2"                          .
      *        *-------------------------------------------------------*
      *        * [zm3]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dpm/fls/rec/rfzm3"                          .
      *        *-------------------------------------------------------*
      *        * [zum]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzum"                          .
      *        *-------------------------------------------------------*
      *        * [pdx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfpdx"                          .
      *        *-------------------------------------------------------*
      *        * [ztv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfztv"                          .
      *        *-------------------------------------------------------*
      *        * [zms]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dpm/fls/rec/rfzms"                          .
      *        *-------------------------------------------------------*
      *        * [pdc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfpdc"                         .
      *        *-------------------------------------------------------*
      *        * [zci]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzci"                          .
      *        *-------------------------------------------------------*
      *        * [aaf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaf"                          .
      *        *-------------------------------------------------------*
      *        * [aaq]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaq"                          .
      *        *-------------------------------------------------------*
      *        * [fnt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rffnt"                          .
      *        *-------------------------------------------------------*
      *        * [dcf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfdcf"                          .
      *        *-------------------------------------------------------*
      *        * [pdt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfpdt"                          .
      *        *-------------------------------------------------------*
      *        * [zvl]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzvl"                          .
      *        *-------------------------------------------------------*
      *        * [fbs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fab/fls/rec/rffbs"                          .

      *    *===========================================================*
      *    * Work-area per bufferizzazione testata                     *
      *    *-----------------------------------------------------------*
       01  w-tes.
      *        *-------------------------------------------------------*
      *        * Valori chiave                                         *
      *        *-------------------------------------------------------*
           05  w-tes-val-key.
               10  w-tes-tip-mag          pic  9(02)                  .
               10  w-tes-num-map          pic  9(07)                  .
               10  w-tes-num-map-aut      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
               10  w-tes-ide-dat          pic  9(07)                  .
               10  w-tes-ide-ute          pic  x(08)                  .
               10  w-tes-ide-fas          pic  x(06)                  .
               10  w-tes-alf-map          pic  x(14)                  .
               10  w-tes-syn-map          pic  x(13)                  .
               10  w-tes-des-key          pic  x(40)                  .
               10  w-tes-des-int          pic  x(40)                  .
               10  w-tes-cla-map          pic  9(05)                  .
               10  w-tes-cla-map-des      pic  x(40)                  .
               10  w-tes-cla-map-sud      pic  9(02)                  .
               10  w-tes-cla-map-umi      pic  x(03)                  .
               10  w-tes-gru-map          pic  9(05)                  .
               10  w-tes-gru-map-des      pic  x(40)                  .
               10  w-tes-gru-map-sud      pic  9(02)                  .
               10  w-tes-gru-map-umi      pic  x(03)                  .
               10  w-tes-sgr-map          pic  9(05)                  .
               10  w-tes-sgr-map-des      pic  x(40)                  .
               10  w-tes-sgr-map-sud      pic  9(02)                  .
               10  w-tes-sgr-map-umi      pic  x(03)                  .
               10  w-tes-tip-map          pic  9(02)                  .
               10  w-tes-tip-cfz          pic  9(02)                  .
               10  w-tes-qta-cfz          pic  9(06)v9(03)            .
               10  w-tes-pes-uni          pic  9(06)v9(03)            .
               10  w-tes-pes-tar          pic  9(06)v9(03)            .
               10  w-tes-vol-uni          pic  9(06)v9(03)            .
               10  w-tes-dim-map.
                   15  w-tes-dim-lar      pic  9(06)v9(03)            .
                   15  w-tes-dim-alt      pic  9(06)v9(03)            .
                   15  w-tes-dim-prf      pic  9(06)v9(03)            .
               10  w-tes-pcl-fis          pic  x(10)                  .
               10  w-tes-coe-mol          pic  9(04)v9(03)            .
               10  w-tes-coe-div          pic  9(04)v9(03)            .
               10  w-tes-sco-stg occurs 4.
                   15  w-tes-sco-min      pic  9(10)v9(03)            .
                   15  w-tes-dua-min      pic  9(07)                  .
                   15  w-tes-sco-sic      pic  9(10)v9(03)            .
                   15  w-tes-dua-sic      pic  9(07)                  .
                   15  w-tes-sco-max      pic  9(10)v9(03)            .
                   15  w-tes-dua-max      pic  9(07)                  .
               10  w-tes-umi-prd          pic  x(03)                  .
               10  w-tes-umi-prd-des      pic  x(20)                  .
               10  w-tes-dec-qta          pic  9(01)                  .
               10  w-tes-snx-2qt          pic  9(01)                  .
               10  w-tes-dec-2qt          pic  9(01)                  .
               10  w-tes-snx-3qt          pic  9(01)                  .
               10  w-tes-dec-3qt          pic  9(01)                  .
               10  w-tes-tip-vpr          pic  x(03)                  .
               10  w-tes-tip-vpr-des      pic  x(10)                  .
               10  w-tes-cod-s01          pic  9(05)                  .
               10  w-tes-cod-s01-des      pic  x(20)                  .
               10  w-tes-cod-s02          pic  9(05)                  .
               10  w-tes-cod-s02-des      pic  x(20)                  .
               10  w-tes-cod-s03          pic  9(05)                  .
               10  w-tes-cod-s03-des      pic  x(20)                  .
               10  w-tes-cla-bdg          pic  9(05)                  .
               10  w-tes-dat-iim          pic  9(07)                  .
               10  w-tes-sta-tus          pic  9(02)                  .
               10  w-tes-sta-tud          pic  9(07)                  .
               10  w-tes-sta-tuc          pic  9(07)                  .
               10  w-tes-sta-tuc-alf      pic  x(14)                  .
               10  w-tes-sta-tuc-des      pic  x(40)                  .
               10  w-tes-sta-tux          pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Dati relativi alla gestione degli acquisti        *
      *            *---------------------------------------------------*
               10  w-tes-des-doc.
                   15  w-tes-ded-rig occurs 10
                                          pic  x(40)                  .
               10  w-tes-iva-acq          pic  9(05)                  .
               10  w-tes-iva-acq-des      pic  x(15)                  .
               10  w-tes-ctp-acq          pic  9(07)                  .
               10  w-tes-ctp-acq-des      pic  x(40)                  .
               10  w-tes-cod-pdt          pic  9(07)                  .
               10  w-tes-cod-pdt-rag      pic  x(40)                  .
               10  w-tes-cdp-pdt          pic  x(40)                  .
               10  w-tes-tmp-cns          pic  9(03)                  .
               10  w-tes-tmp-cnm          pic  9(03)                  .
               10  w-tes-lot-acq          pic  9(10)v9(03)            .
               10  w-tes-dcf-pfz          pic  9(07)                  .
               10  w-tes-dpz-pfz          pic  x(04)                  .
               10  w-tes-dcf-pfz-rag      pic  x(40)                  .
               10  w-tes-cop-sfn          pic  x(14)                  .
               10  w-tes-dep-sfn.
                   15  w-tes-dep-rig occurs 10
                                          pic  x(40)                  .
               10  w-tes-xdp-sfn          pic  9(01)                  .
               10  w-tes-dpr-acq          pic  9(01)                  .
               10  w-tes-tip-pza          pic  9(02)                  .
               10  w-tes-sgl-vlt          pic  x(03)                  .
               10  w-tes-sgl-vlt-des      pic  x(20)                  .
               10  w-tes-sgl-vlt-dec      pic  9(01)                  .
               10  w-tes-prz-pes          pic  9(09)                  .
               10  w-tes-psr-pes occurs 05
                                          pic  9(02)v9(01)            .
               10  w-tes-cod-iva          pic  9(05)                  .
               10  w-tes-cod-iva-des      pic  x(15)                  .
               10  w-tes-snx-tum          pic  x(01)                  .
               10  w-tes-umf-tum          pic  x(03)                  .
               10  w-tes-nde-tum          pic  9(01)                  .
               10  w-tes-cmo-tum          pic  9(06)v9(03)            .
               10  w-tes-cdi-tum          pic  9(06)v9(03)            .
               10  w-tes-uda-pes          pic  9(07)                  .
               10  w-tes-per-mpa          pic  9(02)v9(01)            .
               10  w-tes-lgv-vlt          pic  x(03)                  .
               10  w-tes-lgv-vlt-des      pic  x(20)                  .
               10  w-tes-lgv-dcv          pic  9(01)                  .
               10  w-tes-lgv-tdc          pic  x(01)                  .
               10  w-tes-lgv-cdc          pic  9(06)v9(05)            .
               10  w-tes-lgv-pdt          pic  9(01)v9(02)            .
      *            *---------------------------------------------------*
      *            * Area libera                                       *
      *            *---------------------------------------------------*
               10  w-tes-alx-exp.
                   15  filler occurs 55   pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dpm.
               10  w-let-arc-dpm-flg      pic  x(01)                  .
               10  w-let-arc-dpm-num      pic  9(07)                  .
               10  w-let-arc-dpm-alf      pic  x(14)                  .
               10  w-let-arc-dpm-des      pic  x(40)                  .
               10  w-let-arc-dpm-umi      pic  x(03)                  .
               10  w-let-arc-dpm-deq      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zm1]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zm1.
               10  w-let-arc-zm1-flg      pic  x(01)                  .
               10  w-let-arc-zm1-cla      pic  9(05)                  .
               10  w-let-arc-zm1-des      pic  x(40)                  .
               10  w-let-arc-zm1-sud      pic  9(02)                  .
               10  w-let-arc-zm1-umi      pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zm2]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zm2.
               10  w-let-arc-zm2-flg      pic  x(01)                  .
               10  w-let-arc-zm2-cla      pic  9(05)                  .
               10  w-let-arc-zm2-gru      pic  9(05)                  .
               10  w-let-arc-zm2-des      pic  x(40)                  .
               10  w-let-arc-zm2-sud      pic  9(02)                  .
               10  w-let-arc-zm2-umi      pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zm3]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zm3.
               10  w-let-arc-zm3-flg      pic  x(01)                  .
               10  w-let-arc-zm3-cla      pic  9(05)                  .
               10  w-let-arc-zm3-gru      pic  9(05)                  .
               10  w-let-arc-zm3-sgr      pic  9(05)                  .
               10  w-let-arc-zm3-des      pic  x(40)                  .
               10  w-let-arc-zm3-sud      pic  9(02)                  .
               10  w-let-arc-zm3-umi      pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zum]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zum.
               10  w-let-arc-zum-flg      pic  x(01)                  .
               10  w-let-arc-zum-cod      pic  x(03)                  .
               10  w-let-arc-zum-des      pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [pdx]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-pdx.
               10  w-let-arc-pdx-flg      pic  x(01)                  .
               10  w-let-arc-pdx-rec      pic  9(02)                  .
               10  w-let-arc-pdx-arc      pic  9(07)                  .
               10  w-let-arc-pdx-lng      pic  x(03)                  .
               10  w-let-arc-pdx-pro      pic  9(07)                  .
               10  w-let-arc-pdx-fmt      pic  x(14)                  .
               10  w-let-arc-pdx-ctr      pic  9(02)                  .
               10  w-let-arc-pdx-des.
                   15  w-let-arc-pdx-rig  occurs 10
                                          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [ztv]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-ztv.
               10  w-let-arc-ztv-flg      pic  x(01)                  .
               10  w-let-arc-ztv-cod      pic  x(03)                  .
               10  w-let-arc-ztv-des      pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zms]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zms.
               10  w-let-arc-zms-flg      pic  x(01)                  .
               10  w-let-arc-zms-tip      pic  9(02)                  .
               10  w-let-arc-zms-cod      pic  9(05)                  .
               10  w-let-arc-zms-des      pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [pdc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-pdc.
               10  w-let-arc-pdc-flg      pic  x(01)                  .
               10  w-let-arc-pdc-cod      pic  9(07)                  .
               10  w-let-arc-pdc-des      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [pdt]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-pdt.
               10  w-let-arc-pdt-flg      pic  x(01)                  .
               10  w-let-arc-pdt-cod      pic  9(07)                  .
               10  w-let-arc-pdt-rag      pic  x(40)                  .
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
               10  w-let-arc-dcf-lng      pic  x(03)                  .
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

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Numero livelli del piano dei conti                    *
      *        *-------------------------------------------------------*
           05  w-prs-liv-pdc              pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazioni relative all'accettazione campi per *
      *        * gestioni specifiche                                   *
      *        *-------------------------------------------------------*
      *            *---------------------------------------------------*
      *            * Stringa contenente le personalizzazioni           *
      *            *---------------------------------------------------*
           05  w-prs-sna-igs              pic  x(20)                  .
           05  w-prs-sna-igs-r            redefines
               w-prs-sna-igs.
      *            *---------------------------------------------------*
      *            * Si/no accettazione dati generali di acquisto      *
      *            *                                                   *
      *            * Default : 'N'                                     *
      *            *---------------------------------------------------*
               10  w-prs-sna-igs-aaq      pic  x(01)                  .
               10  filler                 pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/no accettazione dati di acquisto relativi al   *
      *            * fornitore preferenziale                           *
      *            *                                                   *
      *            * Default : 'N'                                     *
      *            *---------------------------------------------------*
               10  w-prs-sna-igs-aaf      pic  x(01)                  .
               10  filler                 pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/no accettazione dati relativi alla scorta mi-  *
      *            * nima                                              *
      *            *                                                   *
      *            * Default : 'N'                                     *
      *            *---------------------------------------------------*
               10  w-prs-sna-igs-fbs      pic  x(01)                  .
               10  filler                 pic  x(15)                  .
      *        *-------------------------------------------------------*
      *        * Modalita' di espressione del tempo di consegna        *
      *        *                                                       *
      *        *  - 0 : In giorni                                      *
      *        *  - 1 : In settimane                                   *
      *        *-------------------------------------------------------*
           05  w-prs-mde-tdc              pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/no richiesta di storicizzazione variazioni prezzo  *
      *        *-------------------------------------------------------*
           05  w-prs-sns-vpl              pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Ctl                               *
      *    *-----------------------------------------------------------*
       01  w-ctl.
      *        *-------------------------------------------------------*
      *        * Work per Ctl su unicita' codice materia prima  alfa-  *
      *        * numerica                                              *
      *        *-------------------------------------------------------*
           05  w-ctl-uni-alf.
               10  w-ctl-uni-alf-flg      pic  x(01)                  .
               10  w-ctl-uni-alf-alf      pic  x(14)                  .
               10  w-ctl-uni-alf-num      pic  9(07)                  .
               10  w-ctl-uni-alf-ctr      pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per contatori e indici                          *
      *    *-----------------------------------------------------------*
       01  w-cix.
      *        *-------------------------------------------------------*
      *        * Contatore per percentuali                             *
      *        *-------------------------------------------------------*
           05  w-cix-ctr-001              pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Salvataggio per : Descrizione interna                 *
      *        *-------------------------------------------------------*
           05  w-sav-des-int              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Salvataggio per : Classe merceologica                 *
      *        *-------------------------------------------------------*
           05  w-sav-cla-map              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Salvataggio per : Gruppo merceologico                 *
      *        *-------------------------------------------------------*
           05  w-sav-gru-map              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Salvataggio per : Sottoruppo merceologico             *
      *        *-------------------------------------------------------*
           05  w-sav-sgr-map              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Status                                                *
      *        *-------------------------------------------------------*
           05  w-sav-sta-tus              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Codice materia prima di riferimento                   *
      *        *-------------------------------------------------------*
           05  w-sav-sta-tuc              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Tipo prezzo di acquisto                               *
      *        *-------------------------------------------------------*
           05  w-sav-tip-pza              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Decimali prezzo di acquisto                           *
      *        *-------------------------------------------------------*
           05  w-sav-dpr-acq              pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Codice fornitore preferenziale                        *
      *        *-------------------------------------------------------*
           05  w-sav-dcf-pfz              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Scorta minima, se gestita                             *
      *        *-------------------------------------------------------*
           05  w-sav-sco-min              pic  9(10)v9(03)            .
      *        *-------------------------------------------------------*
      *        * Scorta di sicurezza, se gestita                       *
      *        *-------------------------------------------------------*
           05  w-sav-sco-sic              pic  9(10)v9(03)            .
      *        *-------------------------------------------------------*
      *        * Scorta massima, se gestita                            *
      *        *-------------------------------------------------------*
           05  w-sav-sco-max              pic  9(10)v9(03)            .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Si/no in confezione                        *
      *        *-------------------------------------------------------*
           05  w-exp-tip-cfz.
               10  w-exp-tip-cfz-num      pic  9(02)       value 02   .
               10  w-exp-tip-cfz-lun      pic  9(02)       value 02   .
               10  w-exp-tip-cfz-tbl.
                   15  filler             pic  x(02) value "No"       .
                   15  filler             pic  x(02) value "Si"       .
      *        *-------------------------------------------------------*
      *        * Work per : Status materia prima                       *
      *        *-------------------------------------------------------*
           05  w-exp-sta-tus.
               10  w-exp-sta-tus-num      pic  9(02)       value 7    .
               10  w-exp-sta-tus-lun      pic  9(02)       value 40   .
               10  w-exp-sta-tus-tbl.
                   15  filler             pic  x(40) value
                           "Normale                                 " .
                   15  filler             pic  x(40) value
                           "Ad esaurimento                          " .
                   15  filler             pic  x(40) value
                           "Sostituita da ns. nuovo codice          " .
                   15  filler             pic  x(40) value
                           "Cessato acquisto                        " .
                   15  filler             pic  x(40) value
                           "Cessato acquisto, ma sost. da nuovo cod." .
                   15  filler             pic  x(40) value
                           "Obsoleta                                " .
                   15  filler             pic  x(40) value
                           "Obsoleta, ma sost. da nuovo codice      " .
      *        *-------------------------------------------------------*
      *        * Work per : Opzione per la descrizione                 *
      *        *-------------------------------------------------------*
           05  w-exp-opz-des.
               10  w-exp-opz-des-num      pic  9(02)       value 2    .
               10  w-exp-opz-des-lun      pic  9(02)       value 45   .
               10  w-exp-opz-des-tbl.
                   15  filler             pic  x(45) value
                       "Quella della scheda anagrafica prodotto      ".
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

      *    *===========================================================*
      *    * Work-area generica                                        *
      *    *-----------------------------------------------------------*
       01  w-wrk.
      *        *-------------------------------------------------------*
      *        * Comodo per ridefinizione tabella status               *
      *        *-------------------------------------------------------*
           05  w-wrk-sta-tus.
               10  w-wrk-sta-tus-tbl.
                   15  w-wrk-sta-tus-ele occurs 7
                                          pic  x(40)                  .
               10  w-wrk-sta-tus-pnt      pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di Acc                               *
      *    *-----------------------------------------------------------*
       01  w-acc.
      *        *-------------------------------------------------------*
      *        * Work per accettazione decimali prezzo di acquisto     *
      *        *-------------------------------------------------------*
           05  w-acc-dpr-acq.
               10  w-acc-dpr-acq-wdd      pic s9(01)                  .
               10  w-acc-dpr-acq-wpz      pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Work per accettazione codice iva di acquisto          *
      *        *-------------------------------------------------------*
           05  w-acc-iva-acq.
      *            *---------------------------------------------------*
      *            * Tipo prodotto                                     *
      *            *---------------------------------------------------*
               10  w-acc-iva-acq-tpr      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per opzione descrizione                          *
      *        *-------------------------------------------------------*
           05  w-acc-opz-des.
      *            *---------------------------------------------------*
      *            * Comodi                                            *
      *            *---------------------------------------------------*
               10  w-acc-opz-des-acc      pic  9(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per Err su controllo tasto Do non chiave         *
      *        *-------------------------------------------------------*
           05  w-err-box-err.
               10  w-err-box-err-msg      pic  x(65)                  .
               10  w-err-box-err-m02      pic  x(65)                  .

      *    *===========================================================*
      *    * Work per subroutines di editing                           *
      *    *-----------------------------------------------------------*
       01  w-edt.
      *        *-------------------------------------------------------*
      *        * Area editata per 5 percentuali di sconto in riga      *
      *        *-------------------------------------------------------*
           05  w-edt-per-scr.
               10  w-edt-cst-psr occurs 5.
                   15  w-edt-psr-ele      pic  x(04)                  .
                   15  filler             pic  x(02)                  .
           05  w-edt-cst-psr-ctr          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Area editata per 5 percentuali di sconto in acquisto  *
      *        *-------------------------------------------------------*
           05  w-edt-psr-pes.
               10  w-edt-psr-pes-edt.
                   15  w-edt-psr-pes-ele occurs 05.
                       20  w-edt-psr-pes-per
                                          pic  x(04)                  .
                       20  filler         pic  x(02)                  .
               10  w-edt-psr-pes-c01      pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di editing codice sottoconto         *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtpdc0.wkl"                   .

      *    *===========================================================*
      *    * Work per subroutines di editing codice iva                *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtzci0.wkl"                   .

      *    *===========================================================*
      *    * Work per subroutines di compattamento                     *
      *    *-----------------------------------------------------------*
       01  w-cmp.
      *        *-------------------------------------------------------*
      *        * Work per compattamento righe descrizione prodotto     *
      *        *-------------------------------------------------------*
           05  w-cmp-rig-des.
               10  w-cmp-rig-des-flg      pic  x(01)                  .
               10  w-cmp-rig-des-nri      pic  9(02)                  .
               10  w-cmp-rig-des-nrd      pic  9(02)                  .
               10  w-cmp-rig-des-ctr      pic  9(02)                  .
               10  w-cmp-rig-des-des.
                   15  w-cmp-rig-des-rig  occurs 10
                                          pic  x(40)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice materia prima           *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/acoddpm0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice classe materia prima    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/acmnzm10.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice gruppo materia prima    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/acmnzm20.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice sottogruppo mat. prima  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/acmnzm30.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione unita' di misura               *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acodzum0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione tipo variante                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acodztv0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice statistico 1            *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/acmnzms1.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice statistico 2            *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/acmnzms2.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice statistico 3            *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/acmnzms3.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice commerciale fornitore   *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmndcf0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice casa produttrice        *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmnpdt0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice valuta                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acodzvl0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice sottoconto              *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnpdc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice Iva                     *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnzci0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione coefficiente cambio valuta     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoecmb0.acl"                   .

      *    *===========================================================*
      *    * Work per Let su archivio [zci]                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/larczci0.ltw"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazioni su codice Iva    *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/dimpiva0.dtl"                   .

      *    *===========================================================*
      *    * Work per attribuzione e ripristino codice automatico      *
      *    *-----------------------------------------------------------*
       01  w-enc-dpm.
      *        *-------------------------------------------------------*
      *        * Valore pre incremento                                 *
      *        *-------------------------------------------------------*
           05  w-enc-dpm-val-pre          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Valore post incremento                                *
      *        *-------------------------------------------------------*
           05  w-enc-dpm-val-pos          pic  9(07)                  .

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
       vis-tit-pgm-100.
      *              *-------------------------------------------------*
      *              * Erase video                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se impostazione chiave effettuata      *
      *                  *---------------------------------------------*
           if        w-cnt-tus-acc-key    not  = spaces
                     go to vis-tit-pgm-200.
      *                  *---------------------------------------------*
      *                  * Erase video                                 *
      *                  *---------------------------------------------*
           move      "ER"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tit-pgm-200.
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
           perform   vis-tit-pgm-des-000  thru vis-tit-pgm-des-999    .
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
      *    * Visualizzazione titolo programma                          *
      *    *                                                           *
      *    * Trattamento descrizione del programma                     *
      *    *-----------------------------------------------------------*
       vis-tit-pgm-des-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del numero pagina        *
      *              *-------------------------------------------------*
           if        w-cnt-sts-imp-npt    >    1
                     go to vis-tit-pgm-des-500.
       vis-tit-pgm-des-200.
      *              *-------------------------------------------------*
      *              * Prima pagina                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Descrizione del programma                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      i-ide-des            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     vis-tit-pgm-des-900.
       vis-tit-pgm-des-500.
      *              *-------------------------------------------------*
      *              * Pagine successive                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero di pagine gestite                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Concatenamento iniziale                 *
      *                      *-----------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      i-ide-des            to   w-all-str-cat (1)      .
           move      "- ["                to   w-all-str-cat (2)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                      *-----------------------------------------*
      *                      * Editing numero pagina relativa          *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-cnt-sts-imp-nrt    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-all-str-cat (2)      .
      *                      *-----------------------------------------*
      *                      * Editing numero totale pagine relative   *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-cnt-sts-imp-ntt    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-all-str-cat (4)      .
      *                      *-----------------------------------------*
      *                      * Concatenamento finale                   *
      *                      *-----------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      05                   to   w-all-str-num          .
           move      w-all-str-alf        to   w-all-str-cat (1)      .
           move      "/"                  to   w-all-str-cat (3)      .
           move      "]"                  to   w-all-str-cat (5)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *                      *-----------------------------------------*
      *                      * Allineamento al centro descrizione      *
      *                      *-----------------------------------------*
           move      40                   to   w-all-str-lun          .
           perform   all-str-cen-000      thru all-str-cen-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      w-all-str-alf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     vis-tit-pgm-des-900.
       vis-tit-pgm-des-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-tit-pgm-des-999.
       vis-tit-pgm-des-999.
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
      *              * Normalizzazione segnale di duplicazione         *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-dup-rec-flg      .
       pre-exe-pgm-200.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero livelli del piano dei conti          *
      *                  *---------------------------------------------*
           perform   prs-liv-pdc-000      thru prs-liv-pdc-999        .
      *                  *---------------------------------------------*
      *                  * Informazioni per gestioni specifiche        *
      *                  *---------------------------------------------*
           perform   prs-sna-igs-000      thru prs-sna-igs-999        .
      *                  *---------------------------------------------*
      *                  * Modalita' di espressione tempo di consegna  *
      *                  *---------------------------------------------*
           perform   prs-mde-tdc-000      thru prs-mde-tdc-999        .
      *                  *---------------------------------------------*
      *                  * Storicizzazione variazioni prezzo listino   *
      *                  *---------------------------------------------*
           perform   prs-sns-vpl-000      thru prs-sns-vpl-999        .
       pre-exe-pgm-300.
      *              *-------------------------------------------------*
      *              * Lettura referenze                               *
      *              *-------------------------------------------------*
       pre-exe-pgm-400.
      *              *-------------------------------------------------*
      *              * Open moduli di accettazione                     *
      *              *-------------------------------------------------*
           perform   opn-mdl-acc-000      thru opn-mdl-acc-999        .
       pre-exe-pgm-999.
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
      *    * Lettura personalizzazione : Accettazione valori per ges-  *
      *    * tioni specifiche                                          *
      *    *-----------------------------------------------------------*
       prs-sna-igs-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dpm/dpm400[sna-igs]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       prs-sna-igs-010.
      *              *-------------------------------------------------*
      *              * Se personalizzazione non esistente              *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     go to prs-sna-igs-050.
      *                  *---------------------------------------------*
      *                  * Normalizzazione al valore di default        *
      *                  *---------------------------------------------*
           move      "N"                  to   w-prs-sna-igs-aaq      .
           move      "N"                  to   w-prs-sna-igs-aaf      .
           move      "N"                  to   w-prs-sna-igs-fbs      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     prs-sna-igs-999.
       prs-sna-igs-050.
      *              *-------------------------------------------------*
      *              * Valore letto in work personalizzazioni          *
      *              *-------------------------------------------------*
           move      s-alf                to   w-prs-sna-igs          .
       prs-sna-igs-100.
      *              *-------------------------------------------------*
      *              * Controllo valori letti                          *
      *              *-------------------------------------------------*
       prs-sna-igs-150.
      *                  *---------------------------------------------*
      *                  * Si/no Accettazione dei valori per la        *
      *                  * gestione dati generali di acquisto          *
      *                  *---------------------------------------------*
           if        w-prs-sna-igs-aaq    not  = "S" and
                     w-prs-sna-igs-aaq    not  = "N"
                     move  "N"            to   w-prs-sna-igs-aaq      .
       prs-sna-igs-160.
      *                  *---------------------------------------------*
      *                  * Si/no Accettazione dei valori per la        *
      *                  * gestione dati di acquisto fornitore prefe-  *
      *                  * renziale                                    *
      *                  *---------------------------------------------*
           if        w-prs-sna-igs-aaq    =   "N"
                     move  "N"            to   w-prs-sna-igs-aaf      .
           if        w-prs-sna-igs-aaf    not  = "S" and
                     w-prs-sna-igs-aaf    not  = "N"
                     move  "N"            to   w-prs-sna-igs-aaf      .
       prs-sna-igs-170.
      *                  *---------------------------------------------*
      *                  * Si/no Accettazione dei valori per la        *
      *                  * gestione della scorta materie prime         *
      *                  *---------------------------------------------*
           if        w-prs-sna-igs-fbs    not  = "S" and
                     w-prs-sna-igs-fbs    not  = "N"
                     move  "N"            to   w-prs-sna-igs-fbs      .
       prs-sna-igs-999.
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
      *    * Lettura personalizzazione relativa alla storicizzazione   *
      *    * delle variazioni prezzo di listino                        *
      *    *-----------------------------------------------------------*
       prs-sns-vpl-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dcp[sns-vpl]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-sns-vpl
           else      move  spaces         to   w-prs-sns-vpl          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-sns-vpl        not  = "S"
                     move  "N"            to   w-prs-sns-vpl          .
       prs-sns-vpl-999.
           exit.

      *    *===========================================================*
      *    * Open moduli di accettazione                               *
      *    *-----------------------------------------------------------*
       opn-mdl-acc-000.
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice materia prima   *
      *              *-------------------------------------------------*
           perform   cod-cod-dpm-opn-000  thru cod-cod-dpm-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice classe          *
      *              *-------------------------------------------------*
           perform   cod-mne-zm1-opn-000  thru cod-mne-zm1-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice gruppo          *
      *              *-------------------------------------------------*
           perform   cod-mne-zm2-opn-000  thru cod-mne-zm2-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice sottogruppo     *
      *              *-------------------------------------------------*
           perform   cod-mne-zm3-opn-000  thru cod-mne-zm3-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione unita' di misura       *
      *              *-------------------------------------------------*
           perform   cod-cod-zum-opn-000  thru cod-cod-zum-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione tipo variante          *
      *              *-------------------------------------------------*
           perform   cod-cod-ztv-opn-000  thru cod-cod-ztv-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice statistico 1    *
      *              *-------------------------------------------------*
           perform   cmn-zms-001-opn-000  thru cmn-zms-001-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice statistico 2    *
      *              *-------------------------------------------------*
           perform   cmn-zms-002-opn-000  thru cmn-zms-002-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice statistico 3    *
      *              *-------------------------------------------------*
           perform   cmn-zms-003-opn-000  thru cmn-zms-003-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice sottoconto      *
      *              *-------------------------------------------------*
           perform   cod-mne-pdc-opn-000  thru cod-mne-pdc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open moduli di accettazione per gestioni spe-   *
      *              * cifiche                                         *
      *              *-------------------------------------------------*
           perform   opn-mdl-acc-spc-000  thru opn-mdl-acc-spc-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione coefficiente di cambio *
      *              * valuta                                          *
      *              *-------------------------------------------------*
           perform   coe-cmb-vlt-opn-000  thru coe-cmb-vlt-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice Iva             *
      *              *-------------------------------------------------*
           perform   cod-mne-zci-opn-000  thru cod-mne-zci-opn-999    .
       opn-mdl-acc-999.
           exit.

      *    *===========================================================*
      *    * Open moduli di accettazione per gestioni specifiche       *
      *    *-----------------------------------------------------------*
       opn-mdl-acc-spc-000.
      *              *-------------------------------------------------*
      *              * Per gestione condizioni di acquisto             *
      *              *-------------------------------------------------*
           if        w-prs-sna-igs-aaq    not  = "S"
                     go to opn-mdl-acc-spc-900.
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice fornitore com-  *
      *              * merciale                                        *
      *              *-------------------------------------------------*
           perform   cod-mne-dcf-opn-000  thru cod-mne-dcf-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice casa produttr.  *
      *              *-------------------------------------------------*
           perform   cod-mne-pdt-opn-000  thru cod-mne-pdt-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice valuta          *
      *              *-------------------------------------------------*
           perform   cod-cod-zvl-opn-000  thru cod-cod-zvl-opn-999    .
       opn-mdl-acc-spc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     opn-mdl-acc-spc-999.
       opn-mdl-acc-spc-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Close moduli di accettazione                    *
      *              *-------------------------------------------------*
           perform   cls-mdl-acc-000      thru cls-mdl-acc-999        .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Close moduli di accettazione                              *
      *    *-----------------------------------------------------------*
       cls-mdl-acc-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice materia prima  *
      *              *-------------------------------------------------*
           perform   cod-cod-dpm-cls-000  thru cod-cod-dpm-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice classe         *
      *              *-------------------------------------------------*
           perform   cod-mne-zm1-cls-000  thru cod-mne-zm1-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice gruppo         *
      *              *-------------------------------------------------*
           perform   cod-mne-zm2-cls-000  thru cod-mne-zm2-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice sottogruppo    *
      *              *-------------------------------------------------*
           perform   cod-mne-zm3-cls-000  thru cod-mne-zm3-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione unita' di misura      *
      *              *-------------------------------------------------*
           perform   cod-cod-zum-cls-000  thru cod-cod-zum-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione tipo variante         *
      *              *-------------------------------------------------*
           perform   cod-cod-ztv-cls-000  thru cod-cod-ztv-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice statistico 1   *
      *              *-------------------------------------------------*
           perform   cmn-zms-001-cls-000  thru cmn-zms-001-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice statistico 2   *
      *              *-------------------------------------------------*
           perform   cmn-zms-002-cls-000  thru cmn-zms-002-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice statistico 3   *
      *              *-------------------------------------------------*
           perform   cmn-zms-003-cls-000  thru cmn-zms-003-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice sottoconto     *
      *              *-------------------------------------------------*
           perform   cod-mne-pdc-cls-000  thru cod-mne-pdc-cls-999    .
      *              *-------------------------------------------------*
      *              * Close moduli di accettazione per gestioni spe-  *
      *              * cifiche                                         *
      *              *-------------------------------------------------*
           perform   cls-mdl-acc-spc-000  thru cls-mdl-acc-spc-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione coefficiente di cam-  *
      *              * bio valuta                                      *
      *              *-------------------------------------------------*
           perform   coe-cmb-vlt-cls-000  thru coe-cmb-vlt-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice Iva            *
      *              *-------------------------------------------------*
           perform   cod-mne-zci-cls-000  thru cod-mne-zci-cls-999    .
       cls-mdl-acc-999.
           exit.

      *    *===========================================================*
      *    * Close moduli di accettazione per gestioni specifiche      *
      *    *-----------------------------------------------------------*
       cls-mdl-acc-spc-000.
      *              *-------------------------------------------------*
      *              * Per gestione condizioni di acquisto             *
      *              *-------------------------------------------------*
           if        w-prs-sna-igs-aaq    not  = "S"
                     go to cls-mdl-acc-spc-999.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice fornitore com- *
      *              * merciale                                        *
      *              *-------------------------------------------------*
           perform   cod-mne-dcf-cls-000  thru cod-mne-dcf-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice casa produttr. *
      *              *-------------------------------------------------*
           perform   cod-mne-pdt-cls-000  thru cod-mne-pdt-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice valuta         *
      *              *-------------------------------------------------*
           perform   cod-cod-zvl-cls-000  thru cod-cod-zvl-cls-999    .
       cls-mdl-acc-spc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cls-mdl-acc-spc-999.
       cls-mdl-acc-spc-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * [dpm]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
      *              *-------------------------------------------------*
      *              * [zm1]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofzm1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zm1                 .
      *              *-------------------------------------------------*
      *              * [zm2]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofzm2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zm2                 .
      *              *-------------------------------------------------*
      *              * [zm3]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofzm3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zm3                 .
      *              *-------------------------------------------------*
      *              * [zum]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzum"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zum                 .
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
      *              * [ztv]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofztv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ztv                 .
      *              *-------------------------------------------------*
      *              * [zms]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofzms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zms                 .
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
      *              * Open modulo determinazioni su codice Iva        *
      *              *-------------------------------------------------*
           move      "OP"                 to   d-imp-iva-tip-ope      .
           move      "pgm/cge/prg/obj/dimpiva0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-imp-iva              .
      *              *-------------------------------------------------*
      *              * Open files per gestioni specifiche              *
      *              *-------------------------------------------------*
           perform   rou-opn-fls-spc-000  thru rou-opn-fls-spc-999    .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Open files per gestioni specifiche                        *
      *    *-----------------------------------------------------------*
       rou-opn-fls-spc-000.
      *              *-------------------------------------------------*
      *              * Per gestione sottoscorta                        *
      *              *-------------------------------------------------*
           if        w-prs-sna-igs-fbs    not  = "S"
                     go to rou-opn-fls-spc-200.
      *              *-------------------------------------------------*
      *              * [fbs]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/fab/fls/ioc/obj/ioffbs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fbs                 .
       rou-opn-fls-spc-200.
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
      *              * Per gestione condizioni di acquisto             *
      *              *-------------------------------------------------*
           if        w-prs-sna-igs-aaq    not  = "S"
                     go to rou-opn-fls-spc-900.
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
      *              * [zvl]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzvl"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvl                 .
       rou-opn-fls-spc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-opn-fls-spc-999.
       rou-opn-fls-spc-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [dpm]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
      *              *-------------------------------------------------*
      *              * [zm1]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofzm1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zm1                 .
      *              *-------------------------------------------------*
      *              * [zm2]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofzm2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zm2                 .
      *              *-------------------------------------------------*
      *              * [zm3]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofzm3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zm3                 .
      *              *-------------------------------------------------*
      *              * [zum]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzum"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zum                 .
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
      *              * [ztv]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofztv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ztv                 .
      *              *-------------------------------------------------*
      *              * [zms]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofzms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zms                 .
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
      *              * Close modulo determinazioni su codice Iva       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiusura modulo                             *
      *                  *---------------------------------------------*
           move      "CL"                 to   d-imp-iva-tip-ope      .
           move      "pgm/cge/prg/obj/dimpiva0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-imp-iva              .
      *                  *---------------------------------------------*
      *                  * Test se cancellabile                        *
      *                  *---------------------------------------------*
           move      "C?"                 to   d-imp-iva-tip-ope      .
           move      "pgm/cge/prg/obj/dimpiva0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-imp-iva              .
           if        d-imp-iva-exi-sts    not  = spaces
                     go to rou-cls-fls-900.
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo                        *
      *                  *---------------------------------------------*
           move      "pgm/cge/prg/obj/dimpiva0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       rou-cls-fls-900.
      *              *-------------------------------------------------*
      *              * Close files per gestioni specifiche             *
      *              *-------------------------------------------------*
           perform   rou-cls-fls-spc-000  thru rou-cls-fls-spc-999    .
       rou-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per gestioni specifiche                       *
      *    *-----------------------------------------------------------*
       rou-cls-fls-spc-000.
      *              *-------------------------------------------------*
      *              * Per gestione sottoscorta                        *
      *              *-------------------------------------------------*
           if        w-prs-sna-igs-fbs    not  = "S"
                     go to rou-cls-fls-spc-200.
      *              *-------------------------------------------------*
      *              * [fbs]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/fab/fls/ioc/obj/ioffbs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fbs                 .
       rou-cls-fls-spc-200.
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
      *              * Per gestione condizioni di acquisto             *
      *              *-------------------------------------------------*
           if        w-prs-sna-igs-aaq    not  = "S"
                     go to rou-cls-fls-spc-999.
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
      *              * [zvl]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzvl"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvl                 .
       rou-cls-fls-spc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-cls-fls-spc-999.
       rou-cls-fls-spc-999.
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
           move      1                    to   w-cnt-sts-imp-nrt      .
      *
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
      *                  * Numero progressivo interno                  *
      *                  *---------------------------------------------*
           perform   acc-prg-map-000      thru acc-prg-map-999        .
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
      *              * Deviazione in funzione del numero pagina        *
      *              *-------------------------------------------------*
           if        w-cnt-sts-imp-npt    >    1
                     go to vis-key-reg-200.
       vis-key-reg-100.
      *              *-------------------------------------------------*
      *              * Se prima pagina                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero progressivo interno                  *
      *                  *---------------------------------------------*
           perform   vis-prg-map-000      thru vis-prg-map-999        .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     vis-key-reg-900.
       vis-key-reg-200.
      *              *-------------------------------------------------*
      *              * Se pagina oltre la prima                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico                         *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      19                   to   v-pos                  .
           move      w-tes-alf-map (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Descrizione interna                         *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-tes-des-int (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
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
      *              * Erase linee impegnate                           *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      05                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
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
      *                  * Numero progressivo interno                  *
      *                  *---------------------------------------------*
           perform   pmt-prg-map-000      thru pmt-prg-map-999        .
      *                  *---------------------------------------------*
      *                  * A linea di trattini                         *
      *                  *---------------------------------------------*
           go to     pmt-key-reg-800.
       pmt-key-reg-200.
      *              *-------------------------------------------------*
      *              * Se pagina oltre la prima                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice prodotto                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Materia prima   :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * A linea di trattini                         *
      *                  *---------------------------------------------*
           go to     pmt-key-reg-800.
       pmt-key-reg-800.
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
      *    * Visualizzazione prompts per Numero progressivo interno    *
      *    *-----------------------------------------------------------*
       pmt-prg-map-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero progressivo interno :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-prg-map-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Numero progressivo interno    *
      *    *                             per la materia prima          *
      *    *-----------------------------------------------------------*
       acc-prg-map-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-prg-map-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-dpm-ope      .
           move      "N"                  to   w-cod-cod-dpm-tac      .
           move      w-tes-num-map        to   w-cod-cod-dpm-num      .
           move      spaces               to   w-cod-cod-dpm-alf      .
           move      04                   to   w-cod-cod-dpm-lin      .
           move      30                   to   w-cod-cod-dpm-pos      .
           move      08                   to   w-cod-cod-dpm-dln      .
           move      30                   to   w-cod-cod-dpm-dps      .
           move      spaces               to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-cod-dpm-cll-000  thru cod-cod-dpm-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-dpm-foi-000  thru cod-cod-dpm-foi-999    .
       acc-prg-map-110.
           perform   cod-cod-dpm-cll-000  thru cod-cod-dpm-cll-999    .
           if        w-cod-cod-dpm-ope    =    "F+"
                     go to acc-prg-map-115.
           if        w-cod-cod-dpm-ope    =    "AC"
                     go to acc-prg-map-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-prg-map-115.
           perform   cod-cod-dpm-foi-000  thru cod-cod-dpm-foi-999    .
           go to     acc-prg-map-110.
       acc-prg-map-120.
           move      w-cod-cod-dpm-num    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-prg-map-999.
       acc-prg-map-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-num-map          .
       acc-prg-map-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se il codice impostato *
      *                  * e' zero oppure diverso da zero              *
      *                  *---------------------------------------------*
           if        w-tes-num-map        =    zero
                     go to acc-prg-map-450
           else      go to acc-prg-map-600.
       acc-prg-map-450.
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
                     go to acc-prg-map-452.
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
           go to     acc-prg-map-100.
       acc-prg-map-452.
      *                      *-----------------------------------------*
      *                      * Attribuzione codice automatico progres- *
      *                      * sivo                                    *
      *                      *-----------------------------------------*
           perform   att-cod-aut-000      thru att-cod-aut-999        .
      *                      *-----------------------------------------*
      *                      * Codice automatico in campo di destina-  *
      *                      * zione                                   *
      *                      *-----------------------------------------*
           move      w-enc-dpm-val-pos    to   w-tes-num-map          .
      *                      *-----------------------------------------*
      *                      * Segnale di attribuzione codice esegui-  *
      *                      * ta automaticamente                      *
      *                      *-----------------------------------------*
           move      "#"                  to   w-tes-num-map-aut      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione del codice              *
      *                      *-----------------------------------------*
           perform   vis-prg-map-000      thru vis-prg-map-999        .
      *                      *-----------------------------------------*
      *                      * Prosecuzione                            *
      *                      *-----------------------------------------*
           go to     acc-prg-map-600.
       acc-prg-map-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-prg-map-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-prg-map-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-prg-map-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-prg-map-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-prg-map-999.
       acc-prg-map-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Numero progressivo interno *
      *    *                                per la materia prima       *
      *    *-----------------------------------------------------------*
       vis-prg-map-000.
      *              *-------------------------------------------------*
      *              * Se valore a zero : tutto a Spaces               *
      *              *-------------------------------------------------*
           if        w-tes-num-map        not  = zero
                     go to vis-prg-map-500.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     vis-prg-map-999.
       vis-prg-map-500.
      *              *-------------------------------------------------*
      *              * Se valore a non-zero : editing tra ()           *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-tes-num-map        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           string    "("        delimited by   size
                     v-edt      delimited by   spaces
                     ")"        delimited by   size
                                          into v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-prg-map-999.
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
      *                  * Visualizzazione titolo programma            *
      *                  *---------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
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
      *                          * Decremento numero pagina relativa   *
      *                          *-------------------------------------*
           if        w-cnt-sts-imp-nrt    >    1
                     subtract  1          from w-cnt-sts-imp-nrt      .
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
      *                          * Incremento numero pagina relativa   *
      *                          *-------------------------------------*
           add       1                    to   w-cnt-sts-imp-nrt      .
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
      *              * La testata e' composta di nr. 6 pagine          *
      *              *-------------------------------------------------*
           move      6                    to   w-cnt-sts-imp-mpt      .
      *              *-------------------------------------------------*
      *              * Determinazione delle pagine relative gestite    *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-sts-imp-ntt      .
           move      zero                 to   w-cnt-sts-imp-ntw      .
      *              *-------------------------------------------------*
      *              * Salvataggio numero pagina in trattamento        *
      *              *-------------------------------------------------*
           move      w-cnt-sts-imp-npt    to   w-cnt-sts-imp-nts      .
       dmp-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Calcolo numero relativo pagine                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento contatore di comodo              *
      *                  *---------------------------------------------*
           add       1                    to   w-cnt-sts-imp-ntw      .
           if        w-cnt-sts-imp-ntw    >    w-cnt-sts-imp-mpt
                     go to dmp-tes-reg-900.
      *                  *---------------------------------------------*
      *                  * Test se pagina da trattare                  *
      *                  *---------------------------------------------*
           move      w-cnt-sts-imp-ntw    to   w-cnt-sts-imp-npt      .
           perform   snp-tes-reg-000      thru snp-tes-reg-999        .
      *                  *---------------------------------------------*
      *                  * Test su flag di uscita                      *
      *                  *---------------------------------------------*
           if        w-cnt-sts-imp-snp    not  = spaces
                     go to dmp-tes-reg-200.
      *                  *---------------------------------------------*
      *                  * Incremento numero pagine relative           *
      *                  *---------------------------------------------*
           add       1                    to   w-cnt-sts-imp-ntt      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     dmp-tes-reg-200.
       dmp-tes-reg-900.
      *              *-------------------------------------------------*
      *              * Ripristino numero pagina in trattamento         *
      *              *-------------------------------------------------*
           move      w-cnt-sts-imp-nts    to   w-cnt-sts-imp-npt      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     dmp-tes-reg-999.
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
                     snp-tes-reg-400
                     snp-tes-reg-500
                     snp-tes-reg-600
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
       snp-tes-reg-400.
      *              *-------------------------------------------------*
      *              * Pagina numero 4 - Dati generali di acquisto     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se personalizzazione lo prevede        *
      *                  *---------------------------------------------*
           if        w-prs-sna-igs-aaq    not  = "S"
                     move  "#"            to   w-cnt-sts-imp-snp
                     go to snp-tes-reg-999.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     snp-tes-reg-999.
       snp-tes-reg-500.
      *              *-------------------------------------------------*
      *              * Pagina numero 5 - Dati specifici di acquisto    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se personalizzazione lo prevede        *
      *                  *---------------------------------------------*
           if        w-prs-sna-igs-aaq    not  = "S"
                     move  "#"            to   w-cnt-sts-imp-snp
                     go to snp-tes-reg-999.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     snp-tes-reg-999.
       snp-tes-reg-600.
      *              *-------------------------------------------------*
      *              * Pagina numero 6 - Dati scorta minima            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se personalizzazione lo prevede        *
      *                  *---------------------------------------------*
           if        w-prs-sna-igs-fbs    not  = "S"
                     move  "#"            to   w-cnt-sts-imp-snp
                     go to snp-tes-reg-999.
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
                     acc-tes-reg-200
                     acc-tes-reg-300
                     acc-tes-reg-400
                     acc-tes-reg-500
                     acc-tes-reg-600
                     depending            on   w-cnt-sts-imp-npt      .
           go to     acc-tes-reg-999.
       acc-tes-reg-100.
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Codice materia prima                        *
      *                  *---------------------------------------------*
           perform   acc-cod-map-000      thru acc-cod-map-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-105.
      *                  *---------------------------------------------*
      *                  * Data inizio impiego materia prima           *
      *                  *---------------------------------------------*
           perform   acc-dat-iim-000      thru acc-dat-iim-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-100.
       acc-tes-reg-110.
      *                  *---------------------------------------------*
      *                  * Descrizione interna                         *
      *                  *---------------------------------------------*
           perform   acc-des-int-000      thru acc-des-int-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-105.
       acc-tes-reg-120.
      *                  *---------------------------------------------*
      *                  * Classe merceologica                         *
      *                  *---------------------------------------------*
           perform   acc-cla-map-000      thru acc-cla-map-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-110.
       acc-tes-reg-130.
      *                  *---------------------------------------------*
      *                  * Gruppo merceologico                         *
      *                  *---------------------------------------------*
           perform   acc-gru-map-000      thru acc-gru-map-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-120.
       acc-tes-reg-140.
      *                  *---------------------------------------------*
      *                  * Sottogruppo merceologico                    *
      *                  *---------------------------------------------*
           perform   acc-sgr-map-000      thru acc-sgr-map-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-130.
       acc-tes-reg-150.
      *                  *---------------------------------------------*
      *                  * Unita' di misura                            *
      *                  *---------------------------------------------*
           perform   acc-umi-prd-000      thru acc-umi-prd-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-140.
       acc-tes-reg-160.
      *                  *---------------------------------------------*
      *                  * Sinonimo                                    *
      *                  *---------------------------------------------*
           perform   acc-syn-map-000      thru acc-syn-map-999        .
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
      *                  * Si/no in confezione                         *
      *                  *---------------------------------------------*
           perform   acc-tip-cfz-000      thru acc-tip-cfz-999        .
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
      *                  * Quantita' per confezione                    *
      *                  *---------------------------------------------*
           perform   acc-qta-cfz-000      thru acc-qta-cfz-999        .
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
      *                  * Peso unitario lordo                         *
      *                  *---------------------------------------------*
           perform   acc-pes-uni-000      thru acc-pes-uni-999        .
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
      *                  * Peso tara                                   *
      *                  *---------------------------------------------*
           perform   acc-pes-tar-000      thru acc-pes-tar-999        .
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
      *                  * Volume unitario                             *
      *                  *---------------------------------------------*
           perform   acc-vol-uni-000      thru acc-vol-uni-999        .
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
      *                  * Larghezza                                   *
      *                  *---------------------------------------------*
           perform   acc-dim-lar-000      thru acc-dim-lar-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-240.
       acc-tes-reg-260.
      *                  *---------------------------------------------*
      *                  * Altezza                                     *
      *                  *---------------------------------------------*
           perform   acc-dim-alt-000      thru acc-dim-alt-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-250.
       acc-tes-reg-270.
      *                  *---------------------------------------------*
      *                  * Profondita'                                 *
      *                  *---------------------------------------------*
           perform   acc-dim-prf-000      thru acc-dim-prf-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-260.
       acc-tes-reg-280.
      *                  *---------------------------------------------*
      *                  * Peculiarita' fisiche                        *
      *                  *---------------------------------------------*
           perform   acc-pcl-fis-000      thru acc-pcl-fis-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-270.
       acc-tes-reg-290.
      *                  *---------------------------------------------*
      *                  * Coefficiente moltiplicatore                 *
      *                  *---------------------------------------------*
           perform   acc-coe-mol-000      thru acc-coe-mol-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-280.
       acc-tes-reg-295.
      *                  *---------------------------------------------*
      *                  * Coefficiente divisore                       *
      *                  *---------------------------------------------*
           perform   acc-coe-div-000      thru acc-coe-div-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-290.
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
                     go to acc-tes-reg-295.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
           go to     acc-tes-reg-999.
       acc-tes-reg-300.
      *                  *---------------------------------------------*
      *                  * Decimali quantita'                          *
      *                  *---------------------------------------------*
           perform   acc-dec-qta-000      thru acc-dec-qta-999        .
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
      *                  * Tipo variante                               *
      *                  *---------------------------------------------*
           perform   acc-tip-vpr-000      thru acc-tip-vpr-999        .
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
      *                  * Codice statistico 1                         *
      *                  *---------------------------------------------*
           perform   acc-cod-s01-000      thru acc-cod-s01-999        .
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
      *                  * Codice statistico 2                         *
      *                  *---------------------------------------------*
           perform   acc-cod-s02-000      thru acc-cod-s02-999        .
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
      *                  * Codice statistico 3                         *
      *                  *---------------------------------------------*
           perform   acc-cod-s03-000      thru acc-cod-s03-999        .
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
      *                  * Status commerciale                          *
      *                  *---------------------------------------------*
           perform   acc-sta-tus-000      thru acc-sta-tus-999        .
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
      *                  * Data determinazione status                  *
      *                  *---------------------------------------------*
           perform   acc-sta-tud-000      thru acc-sta-tud-999        .
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
      *                  * Codice di riferimento                       *
      *                  *---------------------------------------------*
           perform   acc-sta-tuc-000      thru acc-sta-tuc-999        .
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
      *                  * Presa visione per pagina 3                  *
      *                  *---------------------------------------------*
           perform   acc-pre-vpg-000      thru acc-pre-vpg-999        .
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
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-tes-reg-999.
       acc-tes-reg-400.
      *              *-------------------------------------------------*
      *              * Pagina 4                                        *
      *              *-------------------------------------------------*
       acc-tes-reg-404.
      *                  *---------------------------------------------*
      *                  * Codice iva per l'acquisto                   *
      *                  *---------------------------------------------*
           perform   acc-iva-acq-000      thru acc-iva-acq-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     move  "-"            to   w-cnt-tus-acc-tes
                     go to acc-tes-reg-999.
       acc-tes-reg-408.
      *                  *---------------------------------------------*
      *                  * Codice contropartita                        *
      *                  *---------------------------------------------*
           perform   acc-ctp-acq-000      thru acc-ctp-acq-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-404.
       acc-tes-reg-410.
      *                  *---------------------------------------------*
      *                  * Codice casa produttrice                     *
      *                  *---------------------------------------------*
           perform   acc-cod-pdt-000      thru acc-cod-pdt-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-408.
       acc-tes-reg-416.
      *                  *---------------------------------------------*
      *                  * Codice originale produttore                 *
      *                  *---------------------------------------------*
           perform   acc-cdp-pdt-000      thru acc-cdp-pdt-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-410.
       acc-tes-reg-424.
      *                  *---------------------------------------------*
      *                  * Codice fornitore preferenziale              *
      *                  *---------------------------------------------*
           perform   acc-dcf-pfz-000      thru acc-dcf-pfz-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-416.
       acc-tes-reg-428.
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
                     go to acc-tes-reg-424.
       acc-tes-reg-432.
      *                  *---------------------------------------------*
      *                  * Tempo di consegna medio                     *
      *                  *---------------------------------------------*
           perform   acc-tmp-cnm-000      thru acc-tmp-cnm-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-428.
       acc-tes-reg-436.
      *                  *---------------------------------------------*
      *                  * Lotto di acquisto                           *
      *                  *---------------------------------------------*
           perform   acc-lot-acq-000      thru acc-lot-acq-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-432.
       acc-tes-reg-490.
      *                  *---------------------------------------------*
      *                  * Presa visione per pagina 9                  *
      *                  *---------------------------------------------*
           perform   acc-pre-vpg-000      thru acc-pre-vpg-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-436.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-tes-reg-999.
       acc-tes-reg-500.
      *              *-------------------------------------------------*
      *              * Pagina 5                                        *
      *              *-------------------------------------------------*
       acc-tes-reg-504.
      *                  *---------------------------------------------*
      *                  * Codice prodotto per fornitore               *
      *                  *---------------------------------------------*
           perform   acc-cop-sfn-000      thru acc-cop-sfn-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     move  "-"            to   w-cnt-tus-acc-tes
                     go to acc-tes-reg-999.
       acc-tes-reg-512.
      *                  *---------------------------------------------*
      *                  * Opzione per la descrizione                  *
      *                  *---------------------------------------------*
           perform   acc-opz-des-000      thru acc-opz-des-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-504.
       acc-tes-reg-516.
      *                  *---------------------------------------------*
      *                  * Descrizione prodotto per fornitore          *
      *                  *---------------------------------------------*
           perform   acc-dep-sfn-000      thru acc-dep-sfn-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-512.
       acc-tes-reg-520.
      *                  *---------------------------------------------*
      *                  * Numero decimali prezzo di acquisto          *
      *                  *---------------------------------------------*
           perform   acc-dpr-acq-000      thru acc-dpr-acq-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-516.
       acc-tes-reg-524.
      *                  *---------------------------------------------*
      *                  * Sigla valuta                                *
      *                  *---------------------------------------------*
           perform   acc-sgl-vlt-000      thru acc-sgl-vlt-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-520.
       acc-tes-reg-528.
      *                  *---------------------------------------------*
      *                  * Tipo prezzo d'acquisto                      *
      *                  *---------------------------------------------*
           perform   acc-tip-pza-000      thru acc-tip-pza-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-524.
       acc-tes-reg-532.
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
                     go to acc-tes-reg-528.
       acc-tes-reg-536.
      *                  *---------------------------------------------*
      *                  * Percentuali di sconto in acquisto           *
      *                  *---------------------------------------------*
           perform   acc-psr-pes-000      thru acc-psr-pes-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-532.
       acc-tes-reg-540.
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
                     go to acc-tes-reg-536.
       acc-tes-reg-544.
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
                     go to acc-tes-reg-540.
       acc-tes-reg-550.
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
                     go to acc-tes-reg-544.
       acc-tes-reg-554.
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
                     go to acc-tes-reg-550.
       acc-tes-reg-560.
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
                     go to acc-tes-reg-554.
       acc-tes-reg-564.
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
                     go to acc-tes-reg-560.
       acc-tes-reg-590.
      *                  *---------------------------------------------*
      *                  * Presa visione per pagina 5                  *
      *                  *---------------------------------------------*
           perform   acc-pre-vpg-000      thru acc-pre-vpg-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-564.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-tes-reg-999.
       acc-tes-reg-600.
      *              *-------------------------------------------------*
      *              * Pagina 6                                        *
      *              *-------------------------------------------------*
       acc-tes-reg-604.
      *                  *---------------------------------------------*
      *                  * Scorta minima                               *
      *                  *---------------------------------------------*
           perform   acc-sco-min-000      thru acc-sco-min-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     move  "-"            to   w-cnt-tus-acc-tes
                     go to acc-tes-reg-999.
           if        v-key                =    "DOWN"
                     go to acc-tes-reg-612.
       acc-tes-reg-608.
      *                  *---------------------------------------------*
      *                  * Data ultimo aggiornamento scorta minima     *
      *                  *---------------------------------------------*
           perform   acc-dua-min-000      thru acc-dua-min-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-604.
       acc-tes-reg-612.
      *                  *---------------------------------------------*
      *                  * Scorta di sicurezza                         *
      *                  *---------------------------------------------*
           perform   acc-sco-sic-000      thru acc-sco-sic-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-604.
           if        v-key                =    "DOWN"
                     go to acc-tes-reg-620.
       acc-tes-reg-616.
      *                  *---------------------------------------------*
      *                  * Data ultimo aggiornamento scorta sicurezza  *
      *                  *---------------------------------------------*
           perform   acc-dua-sic-000      thru acc-dua-sic-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-612.
       acc-tes-reg-620.
      *                  *---------------------------------------------*
      *                  * Scorta massima                              *
      *                  *---------------------------------------------*
           perform   acc-sco-max-000      thru acc-sco-max-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-612.
           if        v-key                =    "DOWN"
                     go to acc-tes-reg-690.
       acc-tes-reg-624.
      *                  *---------------------------------------------*
      *                  * Data ultimo aggiornamento scorta massima    *
      *                  *---------------------------------------------*
           perform   acc-dua-max-000      thru acc-dua-max-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-620.
       acc-tes-reg-690.
      *                  *---------------------------------------------*
      *                  * Presa visione per pagina 6                  *
      *                  *---------------------------------------------*
           perform   acc-pre-vpg-000      thru acc-pre-vpg-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-620.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
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
                     vis-tes-reg-200
                     vis-tes-reg-300
                     vis-tes-reg-400
                     vis-tes-reg-500
                     vis-tes-reg-600
                     depending            on   w-cnt-sts-imp-npt      .
           go to     vis-tes-reg-999.
       vis-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Codice materia prima                            *
      *              *-------------------------------------------------*
           perform   vis-cod-map-000      thru vis-cod-map-999        .
      *              *-------------------------------------------------*
      *              * Data inizio impiego materia prima               *
      *              *-------------------------------------------------*
           perform   vis-dat-iim-000      thru vis-dat-iim-999        .
      *              *-------------------------------------------------*
      *              * Descrizione interna                             *
      *              *-------------------------------------------------*
           perform   vis-des-int-000      thru vis-des-int-999        .
      *              *-------------------------------------------------*
      *              * Classe merceologica                             *
      *              *-------------------------------------------------*
           perform   vis-cla-map-000      thru vis-cla-map-999        .
           perform   vis-des-cla-000      thru vis-des-cla-999        .
      *              *-------------------------------------------------*
      *              * Gruppo merceologico                             *
      *              *-------------------------------------------------*
           perform   vis-gru-map-000      thru vis-gru-map-999        .
           perform   vis-des-gru-000      thru vis-des-gru-999        .
      *              *-------------------------------------------------*
      *              * Sottogruppo merceologico                        *
      *              *-------------------------------------------------*
           perform   vis-sgr-map-000      thru vis-sgr-map-999        .
           perform   vis-des-sgr-000      thru vis-des-sgr-999        .
      *              *-------------------------------------------------*
      *              * Unita' di misura                                *
      *              *-------------------------------------------------*
           perform   vis-umi-prd-000      thru vis-umi-prd-999        .
           perform   vis-des-umi-000      thru vis-des-umi-999        .
      *              *-------------------------------------------------*
      *              * Sinonimo                                        *
      *              *-------------------------------------------------*
           perform   vis-syn-map-000      thru vis-syn-map-999        .
           go to     vis-tes-reg-999.
       vis-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Si/no in confezione                             *
      *              *-------------------------------------------------*
           perform   vis-tip-cfz-000      thru vis-tip-cfz-999        .
      *              *-------------------------------------------------*
      *              * Quantita' per confezione                        *
      *              *-------------------------------------------------*
           perform   vis-qta-cfz-000      thru vis-qta-cfz-999        .
      *              *-------------------------------------------------*
      *              * Peso unitario lordo                             *
      *              *-------------------------------------------------*
           perform   vis-pes-uni-000      thru vis-pes-uni-999        .
      *              *-------------------------------------------------*
      *              * Peso tara                                       *
      *              *-------------------------------------------------*
           perform   vis-pes-tar-000      thru vis-pes-tar-999        .
      *              *-------------------------------------------------*
      *              * Volume unitario                                 *
      *              *-------------------------------------------------*
           perform   vis-vol-uni-000      thru vis-vol-uni-999        .
      *              *-------------------------------------------------*
      *              * Larghezza                                       *
      *              *-------------------------------------------------*
           perform   vis-dim-lar-000      thru vis-dim-lar-999        .
      *              *-------------------------------------------------*
      *              * Altezza                                         *
      *              *-------------------------------------------------*
           perform   vis-dim-alt-000      thru vis-dim-alt-999        .
      *              *-------------------------------------------------*
      *              * Profondita'                                     *
      *              *-------------------------------------------------*
           perform   vis-dim-prf-000      thru vis-dim-prf-999        .
      *              *-------------------------------------------------*
      *              * Peculiarita' fisiche                            *
      *              *-------------------------------------------------*
           perform   vis-pcl-fis-000      thru vis-pcl-fis-999        .
      *              *-------------------------------------------------*
      *              * Coefficiente moltiplicatore                     *
      *              *-------------------------------------------------*
           perform   vis-coe-mol-000      thru vis-coe-mol-999        .
      *              *-------------------------------------------------*
      *              * Coefficiente divisore                           *
      *              *-------------------------------------------------*
           perform   vis-coe-div-000      thru vis-coe-div-999        .
           go to     vis-tes-reg-999.
       vis-tes-reg-300.
      *              *-------------------------------------------------*
      *              * Pagina 3                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Decimali quantita'                          *
      *                  *---------------------------------------------*
           perform   vis-dec-qta-000      thru vis-dec-qta-999        .
      *                  *---------------------------------------------*
      *                  * Tipo variante                               *
      *                  *---------------------------------------------*
           perform   vis-tip-vpr-000      thru vis-tip-vpr-999        .
           perform   vis-des-tvv-000      thru vis-des-tvv-999        .
      *                  *---------------------------------------------*
      *                  * Codice statistico 1                         *
      *                  *---------------------------------------------*
           perform   vis-cod-s01-000      thru vis-cod-s01-999        .
           perform   vis-des-cs1-000      thru vis-des-cs1-999        .
      *                  *---------------------------------------------*
      *                  * Codice statistico 2                         *
      *                  *---------------------------------------------*
           perform   vis-cod-s02-000      thru vis-cod-s02-999        .
           perform   vis-des-cs2-000      thru vis-des-cs2-999        .
      *                  *---------------------------------------------*
      *                  * Codice statistico 3                         *
      *                  *---------------------------------------------*
           perform   vis-cod-s03-000      thru vis-cod-s03-999        .
           perform   vis-des-cs3-000      thru vis-des-cs3-999        .
      *                  *---------------------------------------------*
      *                  * Status                                      *
      *                  *---------------------------------------------*
           perform   vis-sta-tus-000      thru vis-sta-tus-999        .
      *                  *---------------------------------------------*
      *                  * Data determinazione status                  *
      *                  *---------------------------------------------*
           perform   vis-sta-tud-000      thru vis-sta-tud-999        .
      *                  *---------------------------------------------*
      *                  * Codice di riferimento                       *
      *                  *---------------------------------------------*
           perform   vis-sta-tuc-000      thru vis-sta-tuc-999        .
           perform   vis-sta-tuc-des-000  thru vis-sta-tuc-des-999    .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-tes-reg-999.
       vis-tes-reg-400.
      *              *-------------------------------------------------*
      *              * Pagina 4                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice iva per l'acquisto                   *
      *                  *---------------------------------------------*
           perform   vis-iva-acq-000      thru vis-iva-acq-999        .
           perform   vis-iva-acq-des-000  thru vis-iva-acq-des-999    .
      *                  *---------------------------------------------*
      *                  * Codice contropartita                        *
      *                  *---------------------------------------------*
           perform   vis-ctp-acq-000      thru vis-ctp-acq-999        .
           perform   vis-ctp-acq-des-000  thru vis-ctp-acq-des-999    .
      *                  *---------------------------------------------*
      *                  * Codice casa produttrice                     *
      *                  *---------------------------------------------*
           perform   vis-cod-pdt-000      thru vis-cod-pdt-999        .
           perform   vis-cod-pdt-rag-000  thru vis-cod-pdt-rag-999    .
      *                  *---------------------------------------------*
      *                  * Codice originale produttore                 *
      *                  *---------------------------------------------*
           perform   vis-cdp-pdt-000      thru vis-cdp-pdt-999        .
      *                  *---------------------------------------------*
      *                  * Codice fornitore preferenziale              *
      *                  *---------------------------------------------*
           perform   vis-dcf-pfz-000      thru vis-dcf-pfz-999        .
           perform   vis-dcf-pfz-rag-000  thru vis-dcf-pfz-rag-999    .
      *                  *---------------------------------------------*
      *                  * Tempo di consegna                           *
      *                  *---------------------------------------------*
           perform   vis-tmp-cns-000      thru vis-tmp-cns-999        .
      *                  *---------------------------------------------*
      *                  * Tempo di consegna medio                     *
      *                  *---------------------------------------------*
           perform   vis-tmp-cnm-000      thru vis-tmp-cnm-999        .
      *                  *---------------------------------------------*
      *                  * Lotto di acquisto                           *
      *                  *---------------------------------------------*
           perform   vis-lot-acq-000      thru vis-lot-acq-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-tes-reg-999.
       vis-tes-reg-500.
      *              *-------------------------------------------------*
      *              * Pagina 5                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice prodotto per fornitore               *
      *                  *---------------------------------------------*
           perform   vis-cop-sfn-000      thru vis-cop-sfn-999        .
      *                  *---------------------------------------------*
      *                  * Opzione per la descrizione                  *
      *                  *---------------------------------------------*
           perform   vis-opz-des-000      thru vis-opz-des-999        .
      *                  *---------------------------------------------*
      *                  * Descrizione prodotto per fornitore          *
      *                  *---------------------------------------------*
           perform   vis-dep-sfn-000      thru vis-dep-sfn-999        .
      *                  *---------------------------------------------*
      *                  * Numero decimali prezzo di acquisto          *
      *                  *---------------------------------------------*
           perform   vis-dpr-acq-000      thru vis-dpr-acq-999        .
      *                  *---------------------------------------------*
      *                  * Sigla valuta                                *
      *                  *---------------------------------------------*
           perform   vis-sgl-vlt-000      thru vis-sgl-vlt-999        .
           perform   vis-sgl-vlt-des-000  thru vis-sgl-vlt-des-999    .
      *                  *---------------------------------------------*
      *                  * Tipo prezzo d'acquisto                      *
      *                  *---------------------------------------------*
           perform   vis-tip-pza-000      thru vis-tip-pza-999        .
      *                  *---------------------------------------------*
      *                  * Prezzo d'acquisto                           *
      *                  *---------------------------------------------*
           perform   vis-prz-pes-000      thru vis-prz-pes-999        .
      *                  *---------------------------------------------*
      *                  * Percentuali di sconto                       *
      *                  *---------------------------------------------*
           perform   vis-psr-pes-000      thru vis-psr-pes-999        .
      *                  *---------------------------------------------*
      *                  * Si/no trasformazione u.d.m.                 *
      *                  *---------------------------------------------*
           perform   vis-snx-tum-000      thru vis-snx-tum-999        .
      *                  *---------------------------------------------*
      *                  * Unita' di misura fornitore                  *
      *                  *---------------------------------------------*
           perform   vis-umf-tum-000      thru vis-umf-tum-999        .
      *                  *---------------------------------------------*
      *                  * Numero decimali quantita'                   *
      *                  *---------------------------------------------*
           perform   vis-nde-tum-000      thru vis-nde-tum-999        .
      *                  *---------------------------------------------*
      *                  * Coefficiente moltiplicatore                 *
      *                  *---------------------------------------------*
           perform   vis-cmo-tum-000      thru vis-cmo-tum-999        .
      *                  *---------------------------------------------*
      *                  * Coefficiente divisore                       *
      *                  *---------------------------------------------*
           perform   vis-cdi-tum-000      thru vis-cdi-tum-999        .
      *                  *---------------------------------------------*
      *                  * Data aggiornamento prezzo                   *
      *                  *---------------------------------------------*
           perform   vis-uda-pes-000      thru vis-uda-pes-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-tes-reg-999.
       vis-tes-reg-600.
      *              *-------------------------------------------------*
      *              * Pagina 6                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scorta minima                               *
      *                  *---------------------------------------------*
           perform   vis-sco-min-000      thru vis-sco-min-999        .
      *                  *---------------------------------------------*
      *                  * Data ultimo aggiornamento scorta minima     *
      *                  *---------------------------------------------*
           perform   vis-dua-min-000      thru vis-dua-min-999        .
      *                  *---------------------------------------------*
      *                  * Scorta di sicurezza                         *
      *                  *---------------------------------------------*
           perform   vis-sco-sic-000      thru vis-sco-sic-999        .
      *                  *---------------------------------------------*
      *                  * Data ultimo aggiornamento scorta sicurezza  *
      *                  *---------------------------------------------*
           perform   vis-dua-sic-000      thru vis-dua-sic-999        .
      *                  *---------------------------------------------*
      *                  * Scorta massima                              *
      *                  *---------------------------------------------*
           perform   vis-sco-max-000      thru vis-sco-max-999        .
      *                  *---------------------------------------------*
      *                  * Data ultimo aggiornamento scorta massima    *
      *                  *---------------------------------------------*
           perform   vis-dua-max-000      thru vis-dua-max-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
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
                     pmt-tes-reg-400
                     pmt-tes-reg-500
                     pmt-tes-reg-600
                     depending            on   w-cnt-sts-imp-npt      .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Codice materia prima                            *
      *              *-------------------------------------------------*
           perform   pmt-cod-map-000      thru pmt-cod-map-999        .
      *              *-------------------------------------------------*
      *              * Data inizio impiego materia prima               *
      *              *-------------------------------------------------*
           perform   pmt-dat-iim-000      thru pmt-dat-iim-999        .
      *              *-------------------------------------------------*
      *              * Descrizione interna                             *
      *              *-------------------------------------------------*
           perform   pmt-des-int-000      thru pmt-des-int-999        .
      *              *-------------------------------------------------*
      *              * Classe merceologica                             *
      *              *-------------------------------------------------*
           perform   pmt-cla-map-000      thru pmt-cla-map-999        .
      *              *-------------------------------------------------*
      *              * Gruppo merceologico                             *
      *              *-------------------------------------------------*
           perform   pmt-gru-map-000      thru pmt-gru-map-999        .
      *              *-------------------------------------------------*
      *              * Sottogruppo merceologico                        *
      *              *-------------------------------------------------*
           perform   pmt-sgr-map-000      thru pmt-sgr-map-999        .
      *              *-------------------------------------------------*
      *              * Unita' di misura                                *
      *              *-------------------------------------------------*
           perform   pmt-umi-prd-000      thru pmt-umi-prd-999        .
      *              *-------------------------------------------------*
      *              * Sinonimo                                        *
      *              *-------------------------------------------------*
           perform   pmt-syn-map-000      thru pmt-syn-map-999        .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Si/no in confezione                             *
      *              *-------------------------------------------------*
           perform   pmt-tip-cfz-000      thru pmt-tip-cfz-999        .
      *              *-------------------------------------------------*
      *              * Quantita' per confezione                        *
      *              *-------------------------------------------------*
           perform   pmt-qta-cfz-000      thru pmt-qta-cfz-999        .
      *              *-------------------------------------------------*
      *              * Peso unitario lordo                             *
      *              *-------------------------------------------------*
           perform   pmt-pes-uni-000      thru pmt-pes-uni-999        .
      *              *-------------------------------------------------*
      *              * Peso tara                                       *
      *              *-------------------------------------------------*
           perform   pmt-pes-tar-000      thru pmt-pes-tar-999        .
      *              *-------------------------------------------------*
      *              * Volume unitario                                 *
      *              *-------------------------------------------------*
           perform   pmt-vol-uni-000      thru pmt-vol-uni-999        .
      *              *-------------------------------------------------*
      *              * Larghezza                                       *
      *              *-------------------------------------------------*
           perform   pmt-dim-lar-000      thru pmt-dim-lar-999        .
      *              *-------------------------------------------------*
      *              * Altezza                                         *
      *              *-------------------------------------------------*
           perform   pmt-dim-alt-000      thru pmt-dim-alt-999        .
      *              *-------------------------------------------------*
      *              * Profondita'                                     *
      *              *-------------------------------------------------*
           perform   pmt-dim-prf-000      thru pmt-dim-prf-999        .
      *              *-------------------------------------------------*
      *              * Peculiarita' fisiche                            *
      *              *-------------------------------------------------*
           perform   pmt-pcl-fis-000      thru pmt-pcl-fis-999        .
      *              *-------------------------------------------------*
      *              * Coefficiente moltiplicatore                     *
      *              *-------------------------------------------------*
           perform   pmt-coe-mol-000      thru pmt-coe-mol-999        .
      *              *-------------------------------------------------*
      *              * Coefficiente divisore                           *
      *              *-------------------------------------------------*
           perform   pmt-coe-div-000      thru pmt-coe-div-999        .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-300.
      *              *-------------------------------------------------*
      *              * Pagina 3                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Decimali quantita'                          *
      *                  *---------------------------------------------*
           perform   pmt-dec-qta-000      thru pmt-dec-qta-999        .
      *                  *---------------------------------------------*
      *                  * Tipo variante                               *
      *                  *---------------------------------------------*
           perform   pmt-tip-vpr-000      thru pmt-tip-vpr-999        .
      *                  *---------------------------------------------*
      *                  * Codice statistico 1                         *
      *                  *---------------------------------------------*
           perform   pmt-cod-s01-000      thru pmt-cod-s01-999        .
      *                  *---------------------------------------------*
      *                  * Codice statistico 2                         *
      *                  *---------------------------------------------*
           perform   pmt-cod-s02-000      thru pmt-cod-s02-999        .
      *                  *---------------------------------------------*
      *                  * Codice statistico 3                         *
      *                  *---------------------------------------------*
           perform   pmt-cod-s03-000      thru pmt-cod-s03-999        .
      *                  *---------------------------------------------*
      *                  * Status                                      *
      *                  *---------------------------------------------*
           perform   pmt-sta-tus-000      thru pmt-sta-tus-999        .
      *                  *---------------------------------------------*
      *                  * Data determinazione status                  *
      *                  *---------------------------------------------*
           perform   pmt-sta-tud-000      thru pmt-sta-tud-999        .
      *                  *---------------------------------------------*
      *                  * Codice di riferimento                       *
      *                  *---------------------------------------------*
           perform   pmt-sta-tuc-000      thru pmt-sta-tuc-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-tes-reg-999.
       pmt-tes-reg-400.
      *              *-------------------------------------------------*
      *              * Pagina 4                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice iva per l'acquisto                   *
      *                  *---------------------------------------------*
           perform   pmt-iva-acq-000      thru pmt-iva-acq-999        .
      *                  *---------------------------------------------*
      *                  * Codice contropartita                        *
      *                  *---------------------------------------------*
           perform   pmt-ctp-acq-000      thru pmt-ctp-acq-999        .
      *                  *---------------------------------------------*
      *                  * Codice casa produttrice                     *
      *                  *---------------------------------------------*
           perform   pmt-cod-pdt-000      thru pmt-cod-pdt-999        .
      *                  *---------------------------------------------*
      *                  * Codice originale produttore                 *
      *                  *---------------------------------------------*
           perform   pmt-cdp-pdt-000      thru pmt-cdp-pdt-999        .
      *                  *---------------------------------------------*
      *                  * Codice fornitore preferenziale              *
      *                  *---------------------------------------------*
           perform   pmt-dcf-pfz-000      thru pmt-dcf-pfz-999        .
      *                  *---------------------------------------------*
      *                  * Tempo di consegna                           *
      *                  *---------------------------------------------*
           perform   pmt-tmp-cns-000      thru pmt-tmp-cns-999        .
      *                  *---------------------------------------------*
      *                  * Tempo di consegna medio                     *
      *                  *---------------------------------------------*
           perform   pmt-tmp-cnm-000      thru pmt-tmp-cnm-999        .
      *                  *---------------------------------------------*
      *                  * Lotto di acquisto                           *
      *                  *---------------------------------------------*
           perform   pmt-lot-acq-000      thru pmt-lot-acq-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-tes-reg-999.
       pmt-tes-reg-500.
      *              *-------------------------------------------------*
      *              * Pagina 5                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice prodotto per fornitore               *
      *                  *---------------------------------------------*
           perform   pmt-cop-sfn-000      thru pmt-cop-sfn-999        .
      *                  *---------------------------------------------*
      *                  * Opzione per la descrizione                  *
      *                  *---------------------------------------------*
           perform   pmt-opz-des-000      thru pmt-opz-des-999        .
      *                  *---------------------------------------------*
      *                  * Descrizione prodotto per fornitore          *
      *                  *---------------------------------------------*
           perform   pmt-dep-sfn-000      thru pmt-dep-sfn-999        .
      *                  *---------------------------------------------*
      *                  * Numero decimali prezzo di acquisto          *
      *                  *---------------------------------------------*
           perform   pmt-dpr-acq-000      thru pmt-dpr-acq-999        .
      *                  *---------------------------------------------*
      *                  * Sigla valuta                                *
      *                  *---------------------------------------------*
           perform   pmt-sgl-vlt-000      thru pmt-sgl-vlt-999        .
      *                  *---------------------------------------------*
      *                  * Tipo prezzo d'acquisto                      *
      *                  *---------------------------------------------*
           perform   pmt-tip-pza-000      thru pmt-tip-pza-999        .
      *                  *---------------------------------------------*
      *                  * Prezzo d'acquisto                           *
      *                  *---------------------------------------------*
           perform   pmt-prz-pes-000      thru pmt-prz-pes-999        .
      *                  *---------------------------------------------*
      *                  * Percentuali di sconto                       *
      *                  *---------------------------------------------*
           perform   pmt-psr-pes-000      thru pmt-psr-pes-999        .
      *                  *---------------------------------------------*
      *                  * Si/no trasformazione u.d.m.                 *
      *                  *---------------------------------------------*
           perform   pmt-snx-tum-000      thru pmt-snx-tum-999        .
      *                  *---------------------------------------------*
      *                  * Unita' di misura fornitore                  *
      *                  *---------------------------------------------*
           perform   pmt-umf-tum-000      thru pmt-umf-tum-999        .
      *                  *---------------------------------------------*
      *                  * Numero decimali quantita'                   *
      *                  *---------------------------------------------*
           perform   pmt-nde-tum-000      thru pmt-nde-tum-999        .
      *                  *---------------------------------------------*
      *                  * Coefficiente moltiplicatore                 *
      *                  *---------------------------------------------*
           perform   pmt-cmo-tum-000      thru pmt-cmo-tum-999        .
      *                  *---------------------------------------------*
      *                  * Coefficiente divisore                       *
      *                  *---------------------------------------------*
           perform   pmt-cdi-tum-000      thru pmt-cdi-tum-999        .
      *                  *---------------------------------------------*
      *                  * Data aggiornamento prezzo                   *
      *                  *---------------------------------------------*
           perform   pmt-uda-pes-000      thru pmt-uda-pes-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-tes-reg-999.
       pmt-tes-reg-600.
      *              *-------------------------------------------------*
      *              * Pagina 6                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scorta minima                               *
      *                  *---------------------------------------------*
           perform   pmt-sco-min-000      thru pmt-sco-min-999        .
      *                  *---------------------------------------------*
      *                  * Data ultimo aggiornamento scorta minima     *
      *                  *---------------------------------------------*
           perform   pmt-dua-min-000      thru pmt-dua-min-999        .
      *                  *---------------------------------------------*
      *                  * Scorta di sicurezza                         *
      *                  *---------------------------------------------*
           perform   pmt-sco-sic-000      thru pmt-sco-sic-999        .
      *                  *---------------------------------------------*
      *                  * Data ultimo aggiornamento scorta sicurezza  *
      *                  *---------------------------------------------*
           perform   pmt-dua-sic-000      thru pmt-dua-sic-999        .
      *                  *---------------------------------------------*
      *                  * Scorta massima                              *
      *                  *---------------------------------------------*
           perform   pmt-sco-max-000      thru pmt-sco-max-999        .
      *                  *---------------------------------------------*
      *                  * Data ultimo aggiornamento scorta massima    *
      *                  *---------------------------------------------*
           perform   pmt-dua-max-000      thru pmt-dua-max-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-tes-reg-999.
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice materia prima             *
      *    *-----------------------------------------------------------*
       pmt-cod-map-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice materia prima       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-map-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Data inizio impiego              *
      *    *-----------------------------------------------------------*
       pmt-dat-iim-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      52                   to   v-pos                  .
           move      "Data codifica :"    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dat-iim-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Descrizione interna              *
      *    *-----------------------------------------------------------*
       pmt-des-int-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Descrizione ad uso interno :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-des-int-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Classe merceologica              *
      *    *-----------------------------------------------------------*
       pmt-cla-map-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Classe merceologica        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cla-map-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Gruppo merceologico              *
      *    *-----------------------------------------------------------*
       pmt-gru-map-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Gruppo merceologico        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-gru-map-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Sottogruppo merceologico         *
      *    *-----------------------------------------------------------*
       pmt-sgr-map-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Sottogruppo merceologico   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-sgr-map-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Unita' di misura                 *
      *    *-----------------------------------------------------------*
       pmt-umi-prd-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Unita' di misura           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-umi-prd-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Sinonimo per la materia prima    *
      *    *-----------------------------------------------------------*
       pmt-syn-map-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Sinonimo per la materia    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                prima       "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-syn-map-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no in confezione              *
      *    *-----------------------------------------------------------*
       pmt-tip-cfz-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Materia prima in confezione:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-cfz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Quantita' per confezione         *
      *    *-----------------------------------------------------------*
       pmt-qta-cfz-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Q.ta' per   confezione     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-qta-cfz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Peso unitario                    *
      *    *-----------------------------------------------------------*
       pmt-pes-uni-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Peso unitario lordo        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-pes-uni-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Peso tara                        *
      *    *-----------------------------------------------------------*
       pmt-pes-tar-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Peso tara                  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-pes-tar-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Volume unitario                  *
      *    *-----------------------------------------------------------*
       pmt-vol-uni-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Volume unitario            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-vol-uni-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Larghezza                        *
      *    *-----------------------------------------------------------*
       pmt-dim-lar-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Larghezza                  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dim-lar-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Altezza                          *
      *    *-----------------------------------------------------------*
       pmt-dim-alt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Altezza                    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dim-alt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Profondita'                      *
      *    *-----------------------------------------------------------*
       pmt-dim-prf-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Profondita' / Spessore     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dim-prf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Peculiarita' fisiche             *
      *    *-----------------------------------------------------------*
       pmt-pcl-fis-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Peculiarita' fisiche       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-pcl-fis-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Coefficiente moltiplicatore      *
      *    *-----------------------------------------------------------*
       pmt-coe-mol-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Coefficiente moltiplicatore:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-coe-mol-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Coefficiente divisore            *
      *    *-----------------------------------------------------------*
       pmt-coe-div-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Coefficiente divisore      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-coe-div-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Numero decimali quantita'        *
      *    *-----------------------------------------------------------*
       pmt-dec-qta-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero decimali quantita'  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dec-qta-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tipo variante di produzione      *
      *    *-----------------------------------------------------------*
       pmt-tip-vpr-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo variante di produzione:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-vpr-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice statistico 1              *
      *    *-----------------------------------------------------------*
       pmt-cod-s01-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice statistico 1        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-s01-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice statistico 2              *
      *    *-----------------------------------------------------------*
       pmt-cod-s02-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice statistico 2        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-s02-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice statistico 3              *
      *    *-----------------------------------------------------------*
       pmt-cod-s03-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice statistico 3        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-s03-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Status                           *
      *    *-----------------------------------------------------------*
       pmt-sta-tus-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Status materia prima       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-sta-tus-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Data determinazione status       *
      *    *-----------------------------------------------------------*
       pmt-sta-tud-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Rilevazione status al      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-sta-tud-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice di riferimento            *
      *    *-----------------------------------------------------------*
       pmt-sta-tuc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice di riferimento   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-sta-tuc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice iva in acquisto           *
      *    *-----------------------------------------------------------*
       pmt-iva-acq-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Acquisto   - Codice Iva    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-iva-acq-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice contropartita             *
      *    *-----------------------------------------------------------*
       pmt-ctp-acq-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "           - Contropartita :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ctp-acq-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice casa produttrice          *
      *    *-----------------------------------------------------------*
       pmt-cod-pdt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Casa produttrice           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-pdt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice assegnato dal produttore  *
      *    *-----------------------------------------------------------*
       pmt-cdp-pdt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Cod. assegnato dalla casa  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "              produttrice   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cdp-pdt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice fornitore preferenziale   *
      *    *-----------------------------------------------------------*
       pmt-dcf-pfz-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Fornitore preferenziale    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dcf-pfz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tempo di consegna                *
      *    *-----------------------------------------------------------*
       pmt-tmp-cns-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
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
      *    * Visualizzazione prompt : Tempo di consegna medio          *
      *    *-----------------------------------------------------------*
       pmt-tmp-cnm-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      66                   to   v-pos                  .
           move      "medio :"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tmp-cnm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Lotto di acquisto                *
      *    *-----------------------------------------------------------*
       pmt-lot-acq-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Lotto di acquisto          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-lot-acq-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice prodotto per fornitore    *
      *    *-----------------------------------------------------------*
       pmt-cop-sfn-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice prodotto fornitore  :"
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
           move      08                   to   v-lin                  .
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
           move      09                   to   v-lin                  .
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
      *    * Visualizzazione prompt : Numero decimali prezzo           *
      *    *-----------------------------------------------------------*
       pmt-dpr-acq-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Decimali prezzo acquisto   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dpr-acq-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Sigla valuta                     *
      *    *-----------------------------------------------------------*
       pmt-sgl-vlt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Sigla valuta per il prezzo :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                d'acquisto  "
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
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo prezzo di acquisto    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-pza-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Prezzo d'acquisto                *
      *    *-----------------------------------------------------------*
       pmt-prz-pes-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Prezzo di acquisto         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-prz-pes-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Percentuali di sconto            *
      *    *-----------------------------------------------------------*
       pmt-psr-pes-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Percentuali di sconto      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-psr-pes-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no trasformazione u.d.m.      *
      *    *-----------------------------------------------------------*
       pmt-snx-tum-000.
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
      *    * Visualizzazione prompt : Data aggiornamento prezzo        *
      *    *-----------------------------------------------------------*
       pmt-uda-pes-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data aggiornamento prezzo  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-uda-pes-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Scorta minima                    *
      *    *-----------------------------------------------------------*
       pmt-sco-min-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Scorta minima              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-sco-min-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Data ultimo aggiornamento        *
      *    *                          scorta minima                    *
      *    *-----------------------------------------------------------*
       pmt-dua-min-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      22                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      50                   to   v-pos                  .
           move      "Ultimo aggiornamento :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dua-min-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Scorta di sicurezza              *
      *    *-----------------------------------------------------------*
       pmt-sco-sic-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Scorta di sicurezza        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-sco-sic-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Data ultimo aggiornamento        *
      *    *                          scorta di sicurezza              *
      *    *-----------------------------------------------------------*
       pmt-dua-sic-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      22                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      50                   to   v-pos                  .
           move      "Ultimo aggiornamento :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dua-sic-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Scorta massima                   *
      *    *-----------------------------------------------------------*
       pmt-sco-max-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Scorta massima             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-sco-max-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Data ultimo aggiornamento        *
      *    *                          scorta massima                   *
      *    *-----------------------------------------------------------*
       pmt-dua-max-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      22                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      50                   to   v-pos                  .
           move      "Ultimo aggiornamento :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dua-max-999.
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
      *    * Accettazione campo testata : Codice materia prima alfa-   *
      *    *                              numerico                     *
      *    *-----------------------------------------------------------*
       acc-cod-map-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-map-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-dpm-ope      .
           move      "L"                  to   w-cod-cod-dpm-tac      .
           move      w-tes-num-map        to   w-cod-cod-dpm-num      .
           move      w-tes-alf-map (1)    to   w-cod-cod-dpm-alf      .
           move      06                   to   w-cod-cod-dpm-lin      .
           move      30                   to   w-cod-cod-dpm-pos      .
           move      zero                 to   w-cod-cod-dpm-dln      .
           move      zero                 to   w-cod-cod-dpm-dps      .
           move      spaces               to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "NXSC"               to   v-pfk (08)             .
           perform   cod-cod-dpm-cll-000  thru cod-cod-dpm-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-dpm-foi-000  thru cod-cod-dpm-foi-999    .
       acc-cod-map-110.
           perform   cod-cod-dpm-cll-000  thru cod-cod-dpm-cll-999    .
           if        w-cod-cod-dpm-ope    =    "F+"
                     go to acc-cod-map-115.
           if        w-cod-cod-dpm-ope    =    "AC"
                     go to acc-cod-map-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-map-115.
           perform   cod-cod-dpm-foi-000  thru cod-cod-dpm-foi-999    .
           go to     acc-cod-map-110.
       acc-cod-map-120.
           move      w-cod-cod-dpm-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-map-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-map-999.
       acc-cod-map-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-alf-map (1)      .
       acc-cod-map-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione         *
      *                  *---------------------------------------------*
           if        w-tes-alf-map (1)    =    spaces
                     go to acc-cod-map-100.
       acc-cod-map-450.
      *                  *---------------------------------------------*
      *                  * Controllo che il valore impostato sia uni-  *
      *                  * co in archivio                              *
      *                  *---------------------------------------------*
           move      w-tes-num-map        to   w-ctl-uni-alf-num      .
           move      w-tes-alf-map (1)    to   w-ctl-uni-alf-alf      .
           perform   ctl-uni-alf-000      thru ctl-uni-alf-999        .
           if        w-ctl-uni-alf-flg    =    spaces
                     go to acc-cod-map-600.
      *                  *---------------------------------------------*
      *                  * Se controllo non superato                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Codice materia prima gia' utilizzato !"
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-cod-map-100.
       acc-cod-map-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-map-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-map-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-map-100.
       acc-cod-map-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice materia prima alfa *
      *    *-----------------------------------------------------------*
       vis-cod-map-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-alf-map (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-map-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Data inizio impiego                  *
      *    *-----------------------------------------------------------*
       acc-dat-iim-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dat-iim-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      06                   to   v-lin                  .
           move      68                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-dat-iim (1)    to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dat-iim-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dat-iim-999.
       acc-dat-iim-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   w-tes-dat-iim (1)      .
       acc-dat-iim-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dat-iim-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-iim-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dat-iim-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dat-iim-100.
       acc-dat-iim-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Data inizio impiego               *
      *    *-----------------------------------------------------------*
       vis-dat-iim-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      06                   to   v-lin                  .
           move      68                   to   v-pos                  .
           move      w-tes-dat-iim (1)    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-iim-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Descrizione interna          *
      *    *-----------------------------------------------------------*
       acc-des-int-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-des-int (1)    to   w-sav-des-int          .
       acc-des-int-100.
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
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-des-int (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-des-int-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-des-int-999.
       acc-des-int-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-des-int (1)      .
       acc-des-int-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione, a meno *
      *                  * che non sia su tasto Up                     *
      *                  *---------------------------------------------*
           if        w-tes-des-int (1)    not  = spaces
                     go to acc-des-int-450.
           if        v-key                =    "UP  "
                     go to acc-des-int-600
           else      go to acc-des-int-100.
       acc-des-int-450.
      *                  *---------------------------------------------*
      *                  * Se valore a non spaces il primo carattere   *
      *                  * della prima riga non deve essere a spaces   *
      *                  *---------------------------------------------*
           if        w-tes-des-int (1)
                    (01 : 01)             =    spaces
                     go to acc-des-int-100.
       acc-des-int-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione riga descrizione in uppercase  *
      *                  *---------------------------------------------*
           move      w-tes-des-int (1)    to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-tes-des-key (1)      .
       acc-des-int-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-des-int-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-des-int-100.
       acc-des-int-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione interna       *
      *    *-----------------------------------------------------------*
       vis-des-int-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-des-int (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-int-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Classe merceologica          *
      *    *-----------------------------------------------------------*
       acc-cla-map-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-cla-map (1)    to   w-sav-cla-map          .
       acc-cla-map-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zm1-ope      .
           move      w-tes-cla-map (1)    to   w-cod-mne-zm1-cla      .
           move      zero                 to   w-cod-mne-zm1-gru      .
           move      zero                 to   w-cod-mne-zm1-sgr      .
           move      11                   to   w-cod-mne-zm1-lin      .
           move      30                   to   w-cod-mne-zm1-pos      .
           move      11                   to   w-cod-mne-zm1-dln      .
           move      36                   to   w-cod-mne-zm1-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-zm1-cll-000  thru cod-mne-zm1-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zm1-foi-000  thru cod-mne-zm1-foi-999    .
       acc-cla-map-110.
           perform   cod-mne-zm1-cll-000  thru cod-mne-zm1-cll-999    .
           if        w-cod-mne-zm1-ope    =    "F+"
                     go to acc-cla-map-115.
           if        w-cod-mne-zm1-ope    =    "AC"
                     go to acc-cla-map-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cla-map-115.
           perform   cod-mne-zm1-foi-000  thru cod-mne-zm1-foi-999    .
           go to     acc-cla-map-110.
       acc-cla-map-120.
           move      w-cod-mne-zm1-cla    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cla-map-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cla-map-999.
       acc-cla-map-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cla-map (1)      .
       acc-cla-map-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zm1]                      *
      *                  *---------------------------------------------*
           move      w-tes-cla-map (1)    to   w-let-arc-zm1-cla      .
           perform   let-arc-zm1-000      thru let-arc-zm1-999        .
      *                   *--------------------------------------------*
      *                   * Bufferizzazione di :                       *
      *                   * - Descrizione                              *
      *                   * - Ulteriore suddivisione                   *
      *                   * - Unita' di misura da proporre             *
      *                   *--------------------------------------------*
           move      w-let-arc-zm1-des    to   w-tes-cla-map-des (1)  .
           move      w-let-arc-zm1-sud    to   w-tes-cla-map-sud (1)  .
           move      w-let-arc-zm1-umi    to   w-tes-cla-map-umi (1)  .
      *                   *--------------------------------------------*
      *                   * Visualizzazione descrizione                *
      *                   *--------------------------------------------*
           perform   vis-des-cla-000      thru vis-des-cla-999        .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : reimpostazione          *
      *                  *---------------------------------------------*
           if        w-let-arc-zm1-flg    not  = spaces
                     go to acc-cla-map-100.
       acc-cla-map-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore impostato uguale a precedente :   *
      *                  * oltre                                       *
      *                  *---------------------------------------------*
           if        w-tes-cla-map (1)    =    w-sav-cla-map
                     go to acc-cla-map-800.
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione dati  *
      *                      * gruppo merceologico                     *
      *                      *-----------------------------------------*
           if        w-tes-gru-map (1)    =    zero
                     go to acc-cla-map-620.
           move      zero                 to   w-tes-gru-map (1)      .
           move      spaces               to   w-tes-gru-map-des (1)  .
           move      zero                 to   w-tes-gru-map-sud (1)  .
           move      spaces               to   w-tes-gru-map-umi (1)  .
           perform   vis-gru-map-000      thru vis-gru-map-999        .
           perform   vis-des-gru-000      thru vis-des-gru-999        .
       acc-cla-map-620.
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione dati  *
      *                      * sottogruppo merceologico                *
      *                      *-----------------------------------------*
           if        w-tes-sgr-map (1)    =    zero
                     go to acc-cla-map-640.
           move      zero                 to   w-tes-sgr-map (1)      .
           move      spaces               to   w-tes-sgr-map-des (1)  .
           move      zero                 to   w-tes-sgr-map-sud (1)  .
           move      spaces               to   w-tes-sgr-map-umi (1)  .
           perform   vis-sgr-map-000      thru vis-sgr-map-999        .
           perform   vis-des-sgr-000      thru vis-des-sgr-999        .
       acc-cla-map-640.
       acc-cla-map-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cla-map-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cla-map-100.
       acc-cla-map-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Classe merceologica       *
      *    *-----------------------------------------------------------*
       vis-cla-map-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cla-map (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cla-map-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione classe        *
      *    *-----------------------------------------------------------*
       vis-des-cla-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-tes-cla-map-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-cla-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Gruppo merceologico          *
      *    *-----------------------------------------------------------*
       acc-gru-map-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore da accettare                 *
      *                  *---------------------------------------------*
           if        w-tes-cla-map (1)    =    zero
                     go to acc-gru-map-999.
           if        w-tes-cla-map-sud (1)
                                          =    01
                     go to acc-gru-map-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-gru-map (1)    to   w-sav-gru-map          .
       acc-gru-map-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zm2-ope      .
           move      w-tes-cla-map (1)    to   w-cod-mne-zm2-cla      .
           move      w-tes-cla-map-des (1)
                                          to   w-cod-mne-zm2-dcl      .
           move      w-tes-gru-map (1)    to   w-cod-mne-zm2-gru      .
           move      zero                 to   w-cod-mne-zm2-sgr      .
           move      12                   to   w-cod-mne-zm2-lin      .
           move      30                   to   w-cod-mne-zm2-pos      .
           move      12                   to   w-cod-mne-zm2-dln      .
           move      36                   to   w-cod-mne-zm2-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-zm2-cll-000  thru cod-mne-zm2-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zm2-foi-000  thru cod-mne-zm2-foi-999    .
       acc-gru-map-110.
           perform   cod-mne-zm2-cll-000  thru cod-mne-zm2-cll-999    .
           if        w-cod-mne-zm2-ope    =    "F+"
                     go to acc-gru-map-115.
           if        w-cod-mne-zm2-ope    =    "AC"
                     go to acc-gru-map-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-gru-map-115.
           perform   cod-mne-zm2-foi-000  thru cod-mne-zm2-foi-999    .
           go to     acc-gru-map-110.
       acc-gru-map-120.
           move      w-cod-mne-zm2-gru    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-gru-map-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-gru-map-999.
       acc-gru-map-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-gru-map (1)      .
       acc-gru-map-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zm2]                      *
      *                  *---------------------------------------------*
           move      w-tes-cla-map (1)    to   w-let-arc-zm2-cla      .
           move      w-tes-gru-map (1)    to   w-let-arc-zm2-gru      .
           perform   let-arc-zm2-000      thru let-arc-zm2-999        .
      *                   *--------------------------------------------*
      *                   * Bufferizzazione di :                       *
      *                   * - Descrizione                              *
      *                   * - Ulteriore suddivisione                   *
      *                   * - Unita' di misura da proporre             *
      *                   *--------------------------------------------*
           move      w-let-arc-zm2-des    to   w-tes-gru-map-des (1)  .
           move      w-let-arc-zm2-sud    to   w-tes-gru-map-sud (1)  .
           move      w-let-arc-zm2-umi    to   w-tes-gru-map-umi (1)  .
      *                   *--------------------------------------------*
      *                   * Visualizzazione descrizione                *
      *                   *--------------------------------------------*
           perform   vis-des-gru-000      thru vis-des-gru-999        .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : reimpostazione          *
      *                  *---------------------------------------------*
           if        w-let-arc-zm2-flg    not  = spaces
                     go to acc-gru-map-100.
      *                  *---------------------------------------------*
      *                  * Se valore a zero : reimpostazione, a meno   *
      *                  * che non sia su tasto Up                     *
      *                  *---------------------------------------------*
           if        w-tes-gru-map (1)    not  = zero
                     go to acc-gru-map-450.
           if        v-key                =    "UP  "
                     go to acc-gru-map-600
           else      go to acc-gru-map-100.
       acc-gru-map-450.
       acc-gru-map-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore impostato uguale a precedente :   *
      *                  * oltre                                       *
      *                  *---------------------------------------------*
           if        w-tes-gru-map (1)    =    w-sav-gru-map
                     go to acc-gru-map-800.
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione dati  *
      *                      * sottogruppo merceologico                *
      *                      *-----------------------------------------*
           if        w-tes-sgr-map (1)    =    zero
                     go to acc-gru-map-640.
           move      zero                 to   w-tes-sgr-map (1)      .
           move      spaces               to   w-tes-sgr-map-des (1)  .
           move      zero                 to   w-tes-sgr-map-sud (1)  .
           move      spaces               to   w-tes-sgr-map-umi (1)  .
           perform   vis-sgr-map-000      thru vis-sgr-map-999        .
           perform   vis-des-sgr-000      thru vis-des-sgr-999        .
       acc-gru-map-640.
       acc-gru-map-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-gru-map-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-gru-map-100.
       acc-gru-map-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Gruppo merceologico       *
      *    *-----------------------------------------------------------*
       vis-gru-map-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-gru-map (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-gru-map-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione gruppo        *
      *    *-----------------------------------------------------------*
       vis-des-gru-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-tes-gru-map-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-gru-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Sottogruppo merceologico     *
      *    *-----------------------------------------------------------*
       acc-sgr-map-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore da accettare                 *
      *                  *---------------------------------------------*
           if        w-tes-gru-map (1)    =    zero
                     go to acc-sgr-map-999.
           if        w-tes-gru-map-sud (1)
                                          =    01
                     go to acc-sgr-map-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-sgr-map (1)    to   w-sav-sgr-map          .
       acc-sgr-map-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zm3-ope      .
           move      w-tes-cla-map (1)    to   w-cod-mne-zm3-cla      .
           move      w-tes-cla-map-des (1)
                                          to   w-cod-mne-zm3-dcl      .
           move      w-tes-gru-map (1)    to   w-cod-mne-zm3-gru      .
           move      w-tes-gru-map-des (1)
                                          to   w-cod-mne-zm3-dgr      .
           move      w-tes-sgr-map (1)    to   w-cod-mne-zm3-sgr      .
           move      13                   to   w-cod-mne-zm3-lin      .
           move      30                   to   w-cod-mne-zm3-pos      .
           move      13                   to   w-cod-mne-zm3-dln      .
           move      36                   to   w-cod-mne-zm3-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-zm3-cll-000  thru cod-mne-zm3-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zm3-foi-000  thru cod-mne-zm3-foi-999    .
       acc-sgr-map-110.
           perform   cod-mne-zm3-cll-000  thru cod-mne-zm3-cll-999    .
           if        w-cod-mne-zm3-ope    =    "F+"
                     go to acc-sgr-map-115.
           if        w-cod-mne-zm3-ope    =    "AC"
                     go to acc-sgr-map-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-sgr-map-115.
           perform   cod-mne-zm3-foi-000  thru cod-mne-zm3-foi-999    .
           go to     acc-sgr-map-110.
       acc-sgr-map-120.
           move      w-cod-mne-zm3-sgr    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-sgr-map-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-sgr-map-999.
       acc-sgr-map-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-sgr-map (1)      .
       acc-sgr-map-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zm3]                      *
      *                  *---------------------------------------------*
           move      w-tes-cla-map (1)    to   w-let-arc-zm3-cla      .
           move      w-tes-gru-map (1)    to   w-let-arc-zm3-gru      .
           move      w-tes-sgr-map (1)    to   w-let-arc-zm3-sgr      .
           perform   let-arc-zm3-000      thru let-arc-zm3-999        .
      *                   *--------------------------------------------*
      *                   * Bufferizzazione di :                       *
      *                   * - Descrizione                              *
      *                   * - Ulteriore suddivisione                   *
      *                   * - Unita' di misura da proporre             *
      *                   *--------------------------------------------*
           move      w-let-arc-zm3-des    to   w-tes-sgr-map-des (1)  .
           move      w-let-arc-zm3-sud    to   w-tes-sgr-map-sud (1)  .
           move      w-let-arc-zm3-umi    to   w-tes-sgr-map-umi (1)  .
      *                   *--------------------------------------------*
      *                   * Visualizzazione descrizione                *
      *                   *--------------------------------------------*
           perform   vis-des-sgr-000      thru vis-des-sgr-999        .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : reimpostazione          *
      *                  *---------------------------------------------*
           if        w-let-arc-zm3-flg    not  = spaces
                     go to acc-sgr-map-100.
      *                  *---------------------------------------------*
      *                  * Se valore a zero : reimpostazione, a meno   *
      *                  * che non sia su tasto Up                     *
      *                  *---------------------------------------------*
           if        w-tes-sgr-map (1)    not  = zero
                     go to acc-sgr-map-450.
           if        v-key                =    "UP  "
                     go to acc-sgr-map-600
           else      go to acc-sgr-map-100.
       acc-sgr-map-450.
       acc-sgr-map-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sgr-map-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-sgr-map-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-sgr-map-100.
       acc-sgr-map-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Sottogruppo merceologico  *
      *    *-----------------------------------------------------------*
       vis-sgr-map-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-sgr-map (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sgr-map-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione sottogruppo   *
      *    *-----------------------------------------------------------*
       vis-des-sgr-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-tes-sgr-map-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-sgr-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Unita' di misura             *
      *    *-----------------------------------------------------------*
       acc-umi-prd-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione valore di default              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Solo in inserimento e con valori a spazi*
      *                      *-----------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to acc-umi-prd-100.
           if        w-tes-umi-prd (1)    not  = spaces
                     go to acc-umi-prd-100.
      *                      *-----------------------------------------*
      *                      * Se contenuta nel sottogruppo            *
      *                      *-----------------------------------------*
           if        w-tes-sgr-map-umi (1)
                                          not  = spaces
                     move  w-tes-sgr-map-umi (1)
                                          to   w-tes-umi-prd (1)
                     go to acc-umi-prd-100.
      *                      *-----------------------------------------*
      *                      * Se contenuta nel gruppo                 *
      *                      *-----------------------------------------*
           if        w-tes-gru-map-sud (1)
                                          =    01 and
                     w-tes-gru-map-umi (1)
                                          not  = spaces
                     move  w-tes-gru-map-umi (1)
                                          to   w-tes-umi-prd (1)
                     go to acc-umi-prd-100.
      *                      *-----------------------------------------*
      *                      * Se contenuta nella classe               *
      *                      *-----------------------------------------*
           if        w-tes-cla-map-sud (1)
                                          =    01
                     move  w-tes-cla-map-umi (1)
                                          to   w-tes-umi-prd (1)
                     go to acc-umi-prd-100.
       acc-umi-prd-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-umi-ope      .
           move      w-tes-umi-prd (1)    to   w-cod-cod-umi-cod      .
           move      15                   to   w-cod-cod-umi-lin      .
           move      30                   to   w-cod-cod-umi-pos      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "NXSC"               to   v-pfk (08)             .
           perform   cod-cod-zum-cll-000  thru cod-cod-zum-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-zum-foi-000  thru cod-cod-zum-foi-999    .
       acc-umi-prd-110.
           perform   cod-cod-zum-cll-000  thru cod-cod-zum-cll-999    .
           if        w-cod-cod-umi-ope    =    "F+"
                     go to acc-umi-prd-115.
           if        w-cod-cod-umi-ope    =    "AC"
                     go to acc-umi-prd-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-umi-prd-115.
           perform   cod-cod-zum-foi-000  thru cod-cod-zum-foi-999    .
           go to     acc-umi-prd-110.
       acc-umi-prd-120.
           move      w-cod-cod-umi-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-umi-prd-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-umi-prd-999.
       acc-umi-prd-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-umi-prd (1)      .
       acc-umi-prd-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella unita' di misura            *
      *                  *---------------------------------------------*
           move      w-tes-umi-prd (1)    to   w-let-arc-zum-cod      .
           perform   let-arc-zum-000      thru let-arc-zum-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione dati associati al codice     *
      *                  * unita' di misura                            *
      *                  *---------------------------------------------*
           move      w-let-arc-zum-des    to   w-tes-umi-prd-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione unita' di mi-   *
      *                  * sura                                        *
      *                  *---------------------------------------------*
           perform   vis-des-umi-000      thru vis-des-umi-999        .
      *                  *---------------------------------------------*
      *                  * Se unita' di misura non esistente : reimpo- *
      *                  * stazione                                    *
      *                  *---------------------------------------------*
           if        w-let-arc-zum-flg    not  = spaces
                     go to acc-umi-prd-100.
       acc-umi-prd-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-umi-prd-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-umi-prd-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-umi-prd-100.
       acc-umi-prd-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Unita' di misura                  *
      *    *-----------------------------------------------------------*
       vis-umi-prd-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-umi-prd (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-umi-prd-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Descrizione unita' di misura      *
      *    *-----------------------------------------------------------*
       vis-des-umi-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-tes-umi-prd-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-umi-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Sinonimo materia prima       *
      *    *-----------------------------------------------------------*
       acc-syn-map-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-syn-map-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-dpm-ope      .
           move      "S"                  to   w-cod-cod-dpm-tac      .
           move      w-tes-num-map        to   w-cod-cod-dpm-num      .
           move      w-tes-syn-map (1)    to   w-cod-cod-dpm-alf      .
           move      19                   to   w-cod-cod-dpm-lin      .
           move      30                   to   w-cod-cod-dpm-pos      .
           move      zero                 to   w-cod-cod-dpm-dln      .
           move      zero                 to   w-cod-cod-dpm-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "NXSC"               to   v-pfk (08)             .
           perform   cod-cod-dpm-cll-000  thru cod-cod-dpm-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-dpm-foi-000  thru cod-cod-dpm-foi-999    .
       acc-syn-map-110.
           perform   cod-cod-dpm-cll-000  thru cod-cod-dpm-cll-999    .
           if        w-cod-cod-dpm-ope    =    "F+"
                     go to acc-syn-map-115.
           if        w-cod-cod-dpm-ope    =    "AC"
                     go to acc-syn-map-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-syn-map-115.
           perform   cod-cod-dpm-foi-000  thru cod-cod-dpm-foi-999    .
           go to     acc-syn-map-110.
       acc-syn-map-120.
           move      w-cod-cod-dpm-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-syn-map-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-syn-map-999.
       acc-syn-map-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-syn-map (1)      .
       acc-syn-map-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-syn-map-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-syn-map-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-syn-map-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-syn-map-100.
       acc-syn-map-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Sinonimo materia prima    *
      *    *-----------------------------------------------------------*
       vis-syn-map-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-syn-map (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-syn-map-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Si/no in confezione          *
      *    *-----------------------------------------------------------*
       acc-tip-cfz-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-cfz-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-cfz-lun    to   v-car                  .
           move      w-exp-tip-cfz-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-cfz-tbl    to   v-txt                  .
           if        w-tes-tip-cfz (1)    =    zero
                     move  01             to   v-num
           else if   w-tes-tip-cfz (1)    =    01
                     move  02             to   v-num
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
                     go to acc-tip-cfz-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-tip-cfz-999.
       acc-tip-cfz-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  zero           to   w-tes-tip-cfz (1)
           else if   v-num                =    02
                     move  01             to   w-tes-tip-cfz (1)
           else      move  zero           to   w-tes-tip-cfz (1)      .
       acc-tip-cfz-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tip-cfz-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se 'no' in confezione                       *
      *                  *---------------------------------------------*
           if        w-tes-tip-cfz (1)    not  = zero
                     go to acc-tip-cfz-800.
      *                  *---------------------------------------------*
      *                  * Normalizzazione e visualizzazione quantita' *
      *                  *---------------------------------------------*
           move      zero                 to   w-tes-qta-cfz (1)      .
           perform   vis-qta-cfz-000      thru vis-qta-cfz-999        .
       acc-tip-cfz-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-tip-cfz-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-tip-cfz-100.
       acc-tip-cfz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Si/no in confezione       *
      *    *-----------------------------------------------------------*
       vis-tip-cfz-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-cfz-lun    to   v-car                  .
           move      w-exp-tip-cfz-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-cfz-tbl    to   v-txt                  .
           if        w-tes-tip-cfz (1)    =    zero
                     move  01             to   v-num
           else if   w-tes-tip-cfz (1)    =    01
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-cfz-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Quantita' per confezione     *
      *    *-----------------------------------------------------------*
       acc-qta-cfz-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-tip-cfz (1)    =    zero
                     go to acc-qta-cfz-999.
       acc-qta-cfz-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-qta-cfz (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-qta-cfz-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-qta-cfz-999.
       acc-qta-cfz-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-qta-cfz (1)      .
       acc-qta-cfz-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-qta-cfz-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-qta-cfz-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-qta-cfz-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-qta-cfz-100.
       acc-qta-cfz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Quantita' per confezione  *
      *    *-----------------------------------------------------------*
       vis-qta-cfz-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-qta-cfz (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-qta-cfz-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Peso unitario lordo          *
      *    *-----------------------------------------------------------*
       acc-pes-uni-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-pes-uni-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-pes-uni (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-pes-uni-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-pes-uni-999.
       acc-pes-uni-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-pes-uni (1)      .
       acc-pes-uni-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-pes-uni-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-pes-uni-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-pes-uni-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-pes-uni-100.
       acc-pes-uni-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Peso unitario lordo       *
      *    *-----------------------------------------------------------*
       vis-pes-uni-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-pes-uni (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-pes-uni-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Peso tara                    *
      *    *-----------------------------------------------------------*
       acc-pes-tar-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-pes-tar-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-pes-tar (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-pes-tar-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-pes-tar-999.
       acc-pes-tar-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-pes-tar (1)      .
       acc-pes-tar-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-pes-tar-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-pes-tar-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-pes-tar-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-pes-tar-100.
       acc-pes-tar-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Peso tara                 *
      *    *-----------------------------------------------------------*
       vis-pes-tar-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-pes-tar (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-pes-tar-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Volume unitario              *
      *    *-----------------------------------------------------------*
       acc-vol-uni-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-vol-uni-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-vol-uni (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-vol-uni-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-vol-uni-999.
       acc-vol-uni-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-vol-uni (1)      .
       acc-vol-uni-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-vol-uni-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-vol-uni-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-vol-uni-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-vol-uni-100.
       acc-vol-uni-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Volume unitario           *
      *    *-----------------------------------------------------------*
       vis-vol-uni-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-vol-uni (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-vol-uni-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Larghezza                    *
      *    *-----------------------------------------------------------*
       acc-dim-lar-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dim-lar-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-dim-lar (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dim-lar-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dim-lar-999.
       acc-dim-lar-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-dim-lar (1)      .
       acc-dim-lar-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dim-lar-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dim-lar-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dim-lar-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dim-lar-100.
       acc-dim-lar-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Larghezza                 *
      *    *-----------------------------------------------------------*
       vis-dim-lar-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-dim-lar (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dim-lar-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Altezza                      *
      *    *-----------------------------------------------------------*
       acc-dim-alt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dim-alt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-dim-alt (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dim-alt-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dim-alt-999.
       acc-dim-alt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-dim-alt (1)      .
       acc-dim-alt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dim-alt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dim-alt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dim-alt-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dim-alt-100.
       acc-dim-alt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Altezza                   *
      *    *-----------------------------------------------------------*
       vis-dim-alt-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-dim-alt (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dim-alt-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Profondita'                  *
      *    *-----------------------------------------------------------*
       acc-dim-prf-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dim-prf-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-dim-prf (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dim-prf-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dim-prf-999.
       acc-dim-prf-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-dim-prf (1)      .
       acc-dim-prf-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dim-prf-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dim-prf-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dim-prf-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dim-prf-100.
       acc-dim-prf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Profondita'               *
      *    *-----------------------------------------------------------*
       vis-dim-prf-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-dim-prf (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dim-prf-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Peculiarita' fisiche         *
      *    *-----------------------------------------------------------*
       acc-pcl-fis-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-pcl-fis-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-pcl-fis (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-pcl-fis-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-pcl-fis-999.
       acc-pcl-fis-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-pcl-fis (1)      .
       acc-pcl-fis-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-pcl-fis-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-pcl-fis-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-pcl-fis-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-pcl-fis-100.
       acc-pcl-fis-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Peculiarita' fisiche      *
      *    *-----------------------------------------------------------*
       vis-pcl-fis-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-pcl-fis (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-pcl-fis-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Coefficiente moltiplicatore  *
      *    *-----------------------------------------------------------*
       acc-coe-mol-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-coe-mol-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-coe-mol (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-coe-mol-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-coe-mol-999.
       acc-coe-mol-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-coe-mol (1)      .
       acc-coe-mol-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-coe-mol-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-coe-mol-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-coe-mol-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-coe-mol-100.
       acc-coe-mol-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Coefficiente moltiplic.   *
      *    *-----------------------------------------------------------*
       vis-coe-mol-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-coe-mol (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-coe-mol-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Coefficiente divisore        *
      *    *-----------------------------------------------------------*
       acc-coe-div-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-coe-div-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-coe-div (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-coe-div-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-coe-div-999.
       acc-coe-div-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-coe-div (1)      .
       acc-coe-div-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-coe-div-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-coe-div-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-coe-div-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-coe-div-100.
       acc-coe-div-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Coefficiente divisore     *
      *    *-----------------------------------------------------------*
       vis-coe-div-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-coe-div (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-coe-div-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Decimali quantita'           *
      *    *-----------------------------------------------------------*
       acc-dec-qta-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dec-qta-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-dec-qta (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dec-qta-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dec-qta-999.
       acc-dec-qta-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-dec-qta (1)      .
       acc-dec-qta-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore non ammesso : reimpostazione      *
      *                  *---------------------------------------------*
           if        w-tes-dec-qta (1)    >    3
                     go to acc-dec-qta-100.
       acc-dec-qta-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dec-qta-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dec-qta-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dec-qta-100.
       acc-dec-qta-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Decimali quantita'        *
      *    *-----------------------------------------------------------*
       vis-dec-qta-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-dec-qta (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dec-qta-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Tipo variante                *
      *    *-----------------------------------------------------------*
       acc-tip-vpr-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-vpr-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-tip-var-ope      .
           move      w-tes-tip-vpr (1)    to   w-cod-tip-var-cod      .
           move      09                   to   w-cod-tip-var-lin      .
           move      30                   to   w-cod-tip-var-pos      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           perform   cod-cod-ztv-cll-000  thru cod-cod-ztv-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-ztv-foi-000  thru cod-cod-ztv-foi-999    .
       acc-tip-vpr-110.
           perform   cod-cod-ztv-cll-000  thru cod-cod-ztv-cll-999    .
           if        w-cod-tip-var-ope    =    "F+"
                     go to acc-tip-vpr-115.
           if        w-cod-tip-var-ope    =    "AC"
                     go to acc-tip-vpr-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-tip-vpr-115.
           perform   cod-cod-ztv-foi-000  thru cod-cod-ztv-foi-999    .
           go to     acc-tip-vpr-110.
       acc-tip-vpr-120.
           move      w-cod-tip-var-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-tip-vpr-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-tip-vpr-999.
       acc-tip-vpr-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-tip-vpr (1)      .
       acc-tip-vpr-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella                             *
      *                  *---------------------------------------------*
           move      w-tes-tip-vpr (1)    to   w-let-arc-ztv-cod      .
           perform   let-arc-ztv-000      thru let-arc-ztv-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-ztv-des    to   w-tes-tip-vpr-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-tvv-000      thru vis-des-tvv-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-ztv-flg    not  = spaces
                     go to acc-tip-vpr-100.
       acc-tip-vpr-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-vpr-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-tip-vpr-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-tip-vpr-100.
       acc-tip-vpr-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Tipo variante             *
      *    *-----------------------------------------------------------*
       vis-tip-vpr-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-tip-vpr (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-vpr-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione variante      *
      *    *-----------------------------------------------------------*
       vis-des-tvv-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-tes-tip-vpr-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-tvv-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice statistico 1          *
      *    *-----------------------------------------------------------*
       acc-cod-s01-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-s01-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cmn-zms-001-ope      .
           move      w-tes-cod-s01 (1)    to   w-cmn-zms-001-cod      .
           move      12                   to   w-cmn-zms-001-lin      .
           move      30                   to   w-cmn-zms-001-pos      .
           move      12                   to   w-cmn-zms-001-dln      .
           move      37                   to   w-cmn-zms-001-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           perform   cmn-zms-001-cll-000  thru cmn-zms-001-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cmn-zms-001-foi-000  thru cmn-zms-001-foi-999    .
       acc-cod-s01-110.
           perform   cmn-zms-001-cll-000  thru cmn-zms-001-cll-999    .
           if        w-cmn-zms-001-ope    =    "F+"
                     go to acc-cod-s01-115.
           if        w-cmn-zms-001-ope    =    "AC"
                     go to acc-cod-s01-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-s01-115.
           perform   cmn-zms-001-foi-000  thru cmn-zms-001-foi-999    .
           go to     acc-cod-s01-110.
       acc-cod-s01-120.
           move      w-cmn-zms-001-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-s01-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-s01-999.
       acc-cod-s01-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-s01 (1)      .
       acc-cod-s01-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella                             *
      *                  *---------------------------------------------*
           move      01                   to   w-let-arc-zms-tip      .
           move      w-tes-cod-s01 (1)    to   w-let-arc-zms-cod      .
           perform   let-arc-zms-000      thru let-arc-zms-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zms-des    to   w-tes-cod-s01-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-cs1-000      thru vis-des-cs1-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zms-flg    not  = spaces
                     go to acc-cod-s01-100.
       acc-cod-s01-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-s01-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-s01-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-s01-100.
       acc-cod-s01-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice statistico 1       *
      *    *-----------------------------------------------------------*
       vis-cod-s01-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-s01 (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-s01-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione codice sta-   *
      *    * tistico 1                                                 *
      *    *-----------------------------------------------------------*
       vis-des-cs1-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-tes-cod-s01-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-cs1-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice statistico 2          *
      *    *-----------------------------------------------------------*
       acc-cod-s02-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-s02-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cmn-zms-002-ope      .
           move      w-tes-cod-s02 (1)    to   w-cmn-zms-002-cod      .
           move      13                   to   w-cmn-zms-002-lin      .
           move      30                   to   w-cmn-zms-002-pos      .
           move      13                   to   w-cmn-zms-002-dln      .
           move      37                   to   w-cmn-zms-002-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           perform   cmn-zms-002-cll-000  thru cmn-zms-002-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cmn-zms-002-foi-000  thru cmn-zms-002-foi-999    .
       acc-cod-s02-110.
           perform   cmn-zms-002-cll-000  thru cmn-zms-002-cll-999    .
           if        w-cmn-zms-002-ope    =    "F+"
                     go to acc-cod-s02-115.
           if        w-cmn-zms-002-ope    =    "AC"
                     go to acc-cod-s02-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-s02-115.
           perform   cmn-zms-002-foi-000  thru cmn-zms-002-foi-999    .
           go to     acc-cod-s02-110.
       acc-cod-s02-120.
           move      w-cmn-zms-002-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-s02-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-s02-999.
       acc-cod-s02-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-s02 (1)      .
       acc-cod-s02-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella                             *
      *                  *---------------------------------------------*
           move      02                   to   w-let-arc-zms-tip      .
           move      w-tes-cod-s02 (1)    to   w-let-arc-zms-cod      .
           perform   let-arc-zms-000      thru let-arc-zms-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zms-des    to   w-tes-cod-s02-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-cs2-000      thru vis-des-cs2-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zms-flg    not  = spaces
                     go to acc-cod-s02-100.
       acc-cod-s02-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-s02-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-s02-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-s02-100.
       acc-cod-s02-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice statistico 2       *
      *    *-----------------------------------------------------------*
       vis-cod-s02-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-s02 (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-s02-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione codice sta-   *
      *    * tistico 2                                                 *
      *    *-----------------------------------------------------------*
       vis-des-cs2-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-tes-cod-s02-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-cs2-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice statistico 3          *
      *    *-----------------------------------------------------------*
       acc-cod-s03-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-s03-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cmn-zms-003-ope      .
           move      w-tes-cod-s03 (1)    to   w-cmn-zms-003-cod      .
           move      14                   to   w-cmn-zms-003-lin      .
           move      30                   to   w-cmn-zms-003-pos      .
           move      14                   to   w-cmn-zms-003-dln      .
           move      37                   to   w-cmn-zms-003-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           perform   cmn-zms-003-cll-000  thru cmn-zms-003-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cmn-zms-003-foi-000  thru cmn-zms-003-foi-999    .
       acc-cod-s03-110.
           perform   cmn-zms-003-cll-000  thru cmn-zms-003-cll-999    .
           if        w-cmn-zms-003-ope    =    "F+"
                     go to acc-cod-s03-115.
           if        w-cmn-zms-003-ope    =    "AC"
                     go to acc-cod-s03-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-s03-115.
           perform   cmn-zms-003-foi-000  thru cmn-zms-003-foi-999    .
           go to     acc-cod-s03-110.
       acc-cod-s03-120.
           move      w-cmn-zms-003-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-s03-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-s03-999.
       acc-cod-s03-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-s03 (1)      .
       acc-cod-s03-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella                             *
      *                  *---------------------------------------------*
           move      03                   to   w-let-arc-zms-tip      .
           move      w-tes-cod-s03 (1)    to   w-let-arc-zms-cod      .
           perform   let-arc-zms-000      thru let-arc-zms-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zms-des    to   w-tes-cod-s03-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-cs3-000      thru vis-des-cs3-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zms-flg    not  = spaces
                     go to acc-cod-s03-100.
       acc-cod-s03-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-s03-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-s03-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-s03-100.
       acc-cod-s03-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice statistico 3       *
      *    *-----------------------------------------------------------*
       vis-cod-s03-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-s03 (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-s03-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione codice sta-   *
      *    * tistico 3                                                 *
      *    *-----------------------------------------------------------*
       vis-des-cs3-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-tes-cod-s03-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-cs3-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Status materia prima         *
      *    *-----------------------------------------------------------*
       acc-sta-tus-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-sta-tus (1)    to   w-sav-sta-tus          .
       acc-sta-tus-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sta-tus-lun    to   v-car                  .
           move      w-exp-sta-tus-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-sta-tus-tbl    to   v-txt                  .
      *
           if        w-tes-sta-tus (1)    =    01
                     move  01             to   v-num
           else if   w-tes-sta-tus (1)    =    11
                     move  02             to   v-num
           else if   w-tes-sta-tus (1)    =    21
                     move  03             to   v-num
           else if   w-tes-sta-tus (1)    =    51
                     move  04             to   v-num
           else if   w-tes-sta-tus (1)    =    52
                     move  05             to   v-num
           else if   w-tes-sta-tus (1)    =    71
                     move  06             to   v-num
           else if   w-tes-sta-tus (1)    =    72
                     move  07             to   v-num
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
                     go to acc-sta-tus-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-sta-tus-999.
       acc-sta-tus-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  01             to   w-tes-sta-tus (1)
           else if   v-num                =    02
                     move  11             to   w-tes-sta-tus (1)
           else if   v-num                =    03
                     move  21             to   w-tes-sta-tus (1)
           else if   v-num                =    04
                     move  51             to   w-tes-sta-tus (1)
           else if   v-num                =    05
                     move  52             to   w-tes-sta-tus (1)
           else if   v-num                =    06
                     move  71             to   w-tes-sta-tus (1)
           else if   v-num                =    07
                     move  72             to   w-tes-sta-tus (1)
           else      move zero            to   w-tes-sta-tus (1)      .
       acc-sta-tus-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : reimpostazione, a meno che non si *
      *                  * sia in Up, e che il valore precedente non   *
      *                  * fosse gia' a zero                           *
      *                  *---------------------------------------------*
           if        w-tes-sta-tus (1)    not  = zero
                     go to acc-sta-tus-600.
           if        v-key                not  = "UP  "
                     go to acc-sta-tus-100.
           if        w-sav-sta-tus        =    zero
                     go to acc-sta-tus-600
           else      go to acc-sta-tus-100.
       acc-sta-tus-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuale normalizzazione prodotto di rife- *
      *                  * rimento e modalita' di trattamento per le   *
      *                  * statistiche                                 *
      *                  *---------------------------------------------*
           if        w-tes-sta-tuc (1)    =    zero
                     go to acc-sta-tus-650.
           if        w-tes-sta-tus (1)    =    w-sav-sta-tus
                     go to acc-sta-tus-650.
           if        w-tes-sta-tus (1)    =    21  or
                     w-tes-sta-tus (1)    =    52  or
                     w-tes-sta-tus (1)    =    72
                     go to acc-sta-tus-650.
      *                  *---------------------------------------------*
      *                  * Normalizzazione prodotto di riferimento     *
      *                  *---------------------------------------------*
           move      zero                 to   w-tes-sta-tuc (1)      .
           move      spaces               to   w-tes-sta-tuc-alf (1)  .
           move      spaces               to   w-tes-sta-tuc-des (1)  .
      *                  *---------------------------------------------*
      *                  * Normalizzazione modalita' di trattamento    *
      *                  * per le statistiche                          *
      *                  *---------------------------------------------*
           move      zero                 to   w-tes-sta-tux (1)      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione prodotto di riferimento     *
      *                  *---------------------------------------------*
           perform   vis-sta-tuc-000      thru vis-sta-tuc-999        .
           perform   vis-sta-tuc-des-000  thru vis-sta-tuc-des-999    .
       acc-sta-tus-650.
      *                  *---------------------------------------------*
      *                  * Eventuale normalizzazione data di rileva-   *
      *                  * mento status                                *
      *                  *---------------------------------------------*
           if        w-tes-sta-tud (1)    =    zero
                     go to acc-sta-tus-800.
           if        w-tes-sta-tus (1)    =    w-sav-sta-tus
                     go to acc-sta-tus-800.
           if        w-tes-sta-tus (1)    not  = 01
                     go to acc-sta-tus-800.
      *                  *---------------------------------------------*
      *                  * Normalizzazione data di rilevamento status  *
      *                  *---------------------------------------------*
           move      zero                 to   w-tes-sta-tud (1)      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione data di rilevamento status  *
      *                  *---------------------------------------------*
           perform   vis-sta-tud-000      thru vis-sta-tud-999        .
       acc-sta-tus-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-sta-tus-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-sta-tus-100.
       acc-sta-tus-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Status materia prima      *
      *    *-----------------------------------------------------------*
       vis-sta-tus-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sta-tus-lun    to   v-car                  .
           move      w-exp-sta-tus-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-sta-tus-tbl    to   v-txt                  .
           if        w-tes-sta-tus (1)    =    01
                     move  01             to   v-num
           else if   w-tes-sta-tus (1)    =    11
                     move  02             to   v-num
           else if   w-tes-sta-tus (1)    =    21
                     move  03             to   v-num
           else if   w-tes-sta-tus (1)    =    51
                     move  04             to   v-num
           else if   w-tes-sta-tus (1)    =    52
                     move  05             to   v-num
           else if   w-tes-sta-tus (1)    =    71
                     move  06             to   v-num
           else if   w-tes-sta-tus (1)    =    72
                     move  07             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sta-tus-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Data determinazione status   *
      *    *-----------------------------------------------------------*
       acc-sta-tud-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *                                             *
      *                  * Solo se status diverso da 01                *
      *                  *---------------------------------------------*
           if        w-tes-sta-tus (1)    =    01
                     go to acc-sta-tud-999.
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        w-tes-sta-tud (1)    not  = zero
                     go to acc-sta-tud-100.
      *                  *---------------------------------------------*
      *                  * System date da segreteria                   *
      *                  *---------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-tes-sta-tud (1)      .
       acc-sta-tud-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-sta-tud (1)    to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-sta-tud-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-sta-tud-999.
       acc-sta-tud-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   w-tes-sta-tud (1)      .
       acc-sta-tud-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-sta-tud-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sta-tud-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-sta-tud-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-sta-tud-100.
       acc-sta-tud-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Data determinazione sta-  *
      *    *                                 tus                       *
      *    *-----------------------------------------------------------*
       vis-sta-tud-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-sta-tud (1)    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sta-tud-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice di riferimento        *
      *    *-----------------------------------------------------------*
       acc-sta-tuc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *                                             *
      *                  * Solo se status a 21,52,72                   *
      *                  *---------------------------------------------*
           if        w-tes-sta-tus (1)    not  = 21  and
                     w-tes-sta-tus (1)    not  = 52  and
                     w-tes-sta-tus (1)    not  = 72
                     go to acc-sta-tuc-999.
       acc-sta-tuc-100.
      *              *-------------------------------------------------*
      *              * Salvataggio valore precedente                   *
      *              *-------------------------------------------------*
           move      w-tes-sta-tuc (1)    to   w-sav-sta-tuc          .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-dpm-ope      .
           move      "A"                  to   w-cod-cod-dpm-tac      .
           move      w-tes-sta-tuc (1)    to   w-cod-cod-dpm-num      .
           move      w-tes-sta-tuc-alf (1)
                                          to   w-cod-cod-dpm-alf      .
           move      20                   to   w-cod-cod-dpm-lin      .
           move      27                   to   w-cod-cod-dpm-pos      .
           move      20                   to   w-cod-cod-dpm-dln      .
           move      41                   to   w-cod-cod-dpm-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           perform   cod-cod-dpm-cll-000  thru cod-cod-dpm-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-dpm-foi-000  thru cod-cod-dpm-foi-999    .
       acc-sta-tuc-110.
           perform   cod-cod-dpm-cll-000  thru cod-cod-dpm-cll-999    .
           if        w-cod-cod-dpm-ope    =    "F+"
                     go to acc-sta-tuc-115.
           if        w-cod-cod-dpm-ope    =    "AC"
                     go to acc-sta-tuc-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-sta-tuc-115.
           perform   cod-cod-dpm-foi-000  thru cod-cod-dpm-foi-999    .
           go to     acc-sta-tuc-110.
       acc-sta-tuc-120.
           move      w-cod-cod-dpm-num    to   v-num                  .
           move      w-cod-cod-dpm-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-sta-tuc-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-sta-tuc-999.
       acc-sta-tuc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-sta-tuc (1)      .
           move      v-alf                to   w-tes-sta-tuc-alf (1)  .
       acc-sta-tuc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura file [dcp]                          *
      *                  *---------------------------------------------*
           move      w-tes-sta-tuc (1)    to   w-let-arc-dpm-num      .
           perform   let-arc-dpm-000      thru let-arc-dpm-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-dpm-des    to   w-tes-sta-tuc-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-sta-tuc-des-000  thru vis-sta-tuc-des-999    .
      *                  *---------------------------------------------*
      *                  * Se prodotto non esistente : reimpostazione  *
      *                  *---------------------------------------------*
           if        w-let-arc-dpm-flg    not  = spaces
                     go to acc-sta-tuc-100.
      *                  *---------------------------------------------*
      *                  * Se zero : reimpostazione, a meno che non si *
      *                  * sia in Up, e che il valore precedente non   *
      *                  * fosse gia' a zero                           *
      *                  *---------------------------------------------*
           if        w-tes-sta-tuc (1)    not  = zero
                     go to acc-sta-tuc-500.
           if        v-key                not  = "UP  "
                     go to acc-sta-tuc-100.
           if        w-sav-sta-tuc        =    zero
                     go to acc-sta-tuc-600
           else      go to acc-sta-tuc-100.
       acc-sta-tuc-500.
      *                  *---------------------------------------------*
      *                  * Controllo che il codice prodotto impostato  *
      *                  * non sia pari a quello di testata            *
      *                  *---------------------------------------------*
           if        w-tes-sta-tuc (1)    =    w-tes-num-map
                     go to acc-sta-tuc-100.
       acc-sta-tuc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuale normalizzazione modalita' di      *
      *                  * trattamento per le statistiche              *
      *                  *---------------------------------------------*
           if        w-tes-sta-tuc (1)    not  = zero
                     go to acc-sta-tuc-800.
           if        w-tes-sta-tuc (1)    =    w-sav-sta-tuc
                     go to acc-sta-tuc-800.
           if        w-tes-sta-tux (1)    =    zero
                     go to acc-sta-tuc-800.
      *                  *---------------------------------------------*
      *                  * Normalizzazione modalita' di trattamento    *
      *                  * per le statistiche                          *
      *                  *---------------------------------------------*
           move      zero                 to   w-tes-sta-tux (1)      .
       acc-sta-tuc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-sta-tuc-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-sta-tuc-100.
       acc-sta-tuc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice di riferimento     *
      *    *-----------------------------------------------------------*
       vis-sta-tuc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      27                   to   v-pos                  .
           move      w-tes-sta-tuc-alf (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sta-tuc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice prodotto di rife-  *
      *    *                                 rimento, descrizione      *
      *    *-----------------------------------------------------------*
       vis-sta-tuc-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-sta-tuc-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sta-tuc-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice iva di acquisto       *
      *    *-----------------------------------------------------------*
       acc-iva-acq-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione valore default                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Solo se valore a zero                   *
      *                      *-----------------------------------------*
           if        w-tes-iva-acq (1)    not  = zero
                     go to acc-iva-acq-100.
           move      w-tes-cod-iva (1)    to   w-tes-iva-acq (1)      .
       acc-iva-acq-100.
      *              *-------------------------------------------------*
      *              * Editing preliminare per l'accettazione          *
      *              *-------------------------------------------------*
           move      w-tes-iva-acq (1)    to   w-edt-iva-cod          .
           perform   edt-cod-iva-000      thru edt-cod-iva-999        .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zci-ope      .
           move      w-edt-iva-cie        to   w-cod-mne-zci-cod      .
           move      07                   to   w-cod-mne-zci-lin      .
           move      30                   to   w-cod-mne-zci-pos      .
           move      07                   to   w-cod-mne-zci-dln      .
           move      41                   to   w-cod-mne-zci-dps      .
           move      "<BD"                to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "NXSC"               to   v-pfk (08)             .
           perform   cod-mne-zci-cll-000  thru cod-mne-zci-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zci-foi-000  thru cod-mne-zci-foi-999    .
       acc-iva-acq-110.
           perform   cod-mne-zci-cll-000  thru cod-mne-zci-cll-999    .
           if        w-cod-mne-zci-ope    =    "F+"
                     go to acc-iva-acq-115.
           if        w-cod-mne-zci-ope    =    "AC"
                     go to acc-iva-acq-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-iva-acq-115.
           perform   cod-mne-zci-foi-000  thru cod-mne-zci-foi-999    .
           go to     acc-iva-acq-110.
       acc-iva-acq-120.
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
                     go to acc-iva-acq-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-iva-acq-999.
       acc-iva-acq-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-iva-acq (1)      .
       acc-iva-acq-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine di controllo codice iva    *
      *                  *---------------------------------------------*
           move      w-tes-iva-acq (1)    to   d-imp-iva-cod-iva      .
           move      spaces               to   d-imp-iva-tip-iva      .
           move      spaces               to   d-imp-iva-tip-ctl      .
           perform   ctl-cod-iva-000      thru ctl-cod-iva-999        .
      *                      *-----------------------------------------*
      *                      * Se errato : messaggio di errore         *
      *                      *-----------------------------------------*
           if        d-imp-iva-exi-sts    =    spaces
                     move  d-imp-iva-msg-exi
                                          to   w-tes-iva-acq-des (1)
                     go to acc-iva-acq-500.
           move      "ME"                 to   v-ope                  .
           move      d-imp-iva-msg-exi    to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-iva-acq-100.
       acc-iva-acq-500.
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-iva-acq-des-000  thru vis-iva-acq-des-999    .
       acc-iva-acq-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-iva-acq-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-iva-acq-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-iva-acq-100.
       acc-iva-acq-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice iva                *
      *    *-----------------------------------------------------------*
       vis-iva-acq-000.
      *              *-------------------------------------------------*
      *              * Editing preliminare                             *
      *              *-------------------------------------------------*
           move      w-tes-iva-acq (1)    to   w-edt-iva-cod          .
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
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-edt-iva-cie        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-iva-acq-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione iva           *
      *    *-----------------------------------------------------------*
       vis-iva-acq-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-iva-acq-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-iva-acq-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice contropartita per     *
      *    *                              l'acquisto                   *
      *    *-----------------------------------------------------------*
       acc-ctp-acq-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-ctp-acq-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-pdc-ope      .
           move      w-prs-liv-pdc        to   w-cod-mne-pdc-liv      .
           move      w-tes-ctp-acq (1)    to   w-cod-mne-pdc-cod      .
           move      08                   to   w-cod-mne-pdc-lin      .
           move      30                   to   w-cod-mne-pdc-pos      .
           move      08                   to   w-cod-mne-pdc-dln      .
           move      41                   to   w-cod-mne-pdc-dps      .
           move      "B"                  to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "NXSC"               to   v-pfk (08)             .
           perform   cod-mne-pdc-cll-000  thru cod-mne-pdc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-pdc-foi-000  thru cod-mne-pdc-foi-999    .
       acc-ctp-acq-110.
           perform   cod-mne-pdc-cll-000  thru cod-mne-pdc-cll-999    .
           if        w-cod-mne-pdc-ope    =    "F+"
                     go to acc-ctp-acq-115.
           if        w-cod-mne-pdc-ope    =    "AC"
                     go to acc-ctp-acq-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-ctp-acq-115.
           perform   cod-mne-pdc-foi-000  thru cod-mne-pdc-foi-999    .
           go to     acc-ctp-acq-110.
       acc-ctp-acq-120.
           move      w-cod-mne-pdc-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-ctp-acq-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-ctp-acq-999.
       acc-ctp-acq-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-ctp-acq (1)      .
       acc-ctp-acq-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio piano dei conti            *
      *                  *---------------------------------------------*
           move      w-tes-ctp-acq (1)    to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-tes-ctp-acq-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione sottoconto      *
      *                  *---------------------------------------------*
           perform   vis-ctp-acq-des-000  thru vis-ctp-acq-des-999    .
      *                  *---------------------------------------------*
      *                  * Se codice sottoconto non esistente : reim-  *
      *                  * postazione                                  *
      *                  *---------------------------------------------*
           if        w-let-arc-pdc-flg    not  = spaces
                     go to acc-ctp-acq-100.
       acc-ctp-acq-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-ctp-acq-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-ctp-acq-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-ctp-acq-100.
       acc-ctp-acq-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice contropartita      *
      *    *-----------------------------------------------------------*
       vis-ctp-acq-000.
      *              *-------------------------------------------------*
      *              * Editing con appoggio a sinistra                 *
      *              *-------------------------------------------------*
           move      w-prs-liv-pdc        to   w-edt-cod-pdc-liv      .
           move      w-tes-ctp-acq (1)    to   w-edt-cod-pdc-cod      .
           move      "B"                  to   w-edt-cod-pdc-edm      .
           perform   edt-pdc-asx-000      thru edt-pdc-asx-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-edt-cod-pdc-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ctp-acq-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione contropartita *
      *    *-----------------------------------------------------------*
       vis-ctp-acq-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-ctp-acq-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ctp-acq-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice casa produttrice      *
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
           move      w-tes-cod-pdt (1)    to   w-cod-mne-pdt-cod      .
           move      12                   to   w-cod-mne-pdt-lin      .
           move      30                   to   w-cod-mne-pdt-pos      .
           move      12                   to   w-cod-mne-pdt-rln      .
           move      41                   to   w-cod-mne-pdt-rps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
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
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-pdt-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-pdt-999.
       acc-cod-pdt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-pdt (1)      .
       acc-cod-pdt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [pdt]                      *
      *                  *---------------------------------------------*
           move      w-tes-cod-pdt (1)    to   w-let-arc-pdt-cod      .
           perform   let-arc-pdt-000      thru let-arc-pdt-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione ragione sociale              *
      *                  *---------------------------------------------*
           move      w-let-arc-pdt-rag    to   w-tes-cod-pdt-rag (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione ragione sociale             *
      *                  *---------------------------------------------*
           perform   vis-cod-pdt-rag-000  thru vis-cod-pdt-rag-999    .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-pdt-flg    not  = spaces
                     go to acc-cod-pdt-100.
       acc-cod-pdt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-pdt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-pdt-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-pdt-100.
       acc-cod-pdt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice casa produttrice           *
      *    *-----------------------------------------------------------*
       vis-cod-pdt-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-pdt (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-pdt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Ragione sociale casa produttrice  *
      *    *-----------------------------------------------------------*
       vis-cod-pdt-rag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-pdt-rag (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-pdt-rag-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice del produttore        *
      *    *-----------------------------------------------------------*
       acc-cdp-pdt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cdp-pdt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-cdp-pdt (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cdp-pdt-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cdp-pdt-999.
       acc-cdp-pdt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-cdp-pdt (1)      .
       acc-cdp-pdt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cdp-pdt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cdp-pdt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cdp-pdt-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cdp-pdt-100.
       acc-cdp-pdt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice del produttore             *
      *    *-----------------------------------------------------------*
       vis-cdp-pdt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cdp-pdt (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cdp-pdt-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice fornitore prefe-      *
      *    *                              renziale                     *
      *    *-----------------------------------------------------------*
       acc-dcf-pfz-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-dcf-pfz (1)    to   w-sav-dcf-pfz          .
       acc-dcf-pfz-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-dcf-ope      .
           move      w-tes-dcf-pfz (1)    to   w-cod-mne-dcf-cod      .
           move      18                   to   w-cod-mne-dcf-lin      .
           move      30                   to   w-cod-mne-dcf-pos      .
           move      18                   to   w-cod-mne-dcf-rln      .
           move      41                   to   w-cod-mne-dcf-rps      .
           move      zero                 to   w-cod-mne-dcf-vln      .
           move      zero                 to   w-cod-mne-dcf-vps      .
           move      zero                 to   w-cod-mne-dcf-lln      .
           move      zero                 to   w-cod-mne-dcf-lps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "NXSC"               to   v-pfk (08)             .
           perform   cod-mne-dcf-cll-000  thru cod-mne-dcf-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-dcf-foi-000  thru cod-mne-dcf-foi-999    .
       acc-dcf-pfz-110.
           perform   cod-mne-dcf-cll-000  thru cod-mne-dcf-cll-999    .
           if        w-cod-mne-dcf-ope    =    "F+"
                     go to acc-dcf-pfz-115.
           if        w-cod-mne-dcf-ope    =    "AC"
                     go to acc-dcf-pfz-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-dcf-pfz-115.
           perform   cod-mne-dcf-foi-000  thru cod-mne-dcf-foi-999    .
           go to     acc-dcf-pfz-110.
       acc-dcf-pfz-120.
           move      w-cod-mne-dcf-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dcf-pfz-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dcf-pfz-999.
       acc-dcf-pfz-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-dcf-pfz (1)      .
       acc-dcf-pfz-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dcf-pfz-410.
      *                  *---------------------------------------------*
      *                  * Lettura record anagrafica contabile del     *
      *                  * fornitore preferenziale                     *
      *                  *---------------------------------------------*
           move      w-tes-dcf-pfz (1)    to   w-let-arc-fnt-cod      .
           perform   let-arc-fnt-000      thru let-arc-fnt-999        .
      *                  *---------------------------------------------*
      *                  * Lettura record anagrafica commerciale del   *
      *                  * fornitore preferenziale                     *
      *                  *---------------------------------------------*
           move      w-tes-dcf-pfz (1)    to   w-let-arc-dcf-fnt      .
           move      spaces               to   w-let-arc-dcf-dpz      .
           perform   let-arc-dcf-000      thru let-arc-dcf-999        .
       acc-dcf-pfz-420.
      *                  *---------------------------------------------*
      *                  * Memorizzazione ragione sociale del forni-   *
      *                  * tore preferenziale                          *
      *                  *---------------------------------------------*
           if        w-let-arc-dcf-flg    =    spaces
                     move  w-let-arc-dcf-rag
                                          to   w-tes-dcf-pfz-rag (1)
           else      move  w-let-arc-fnt-rag
                                          to   w-tes-dcf-pfz-rag (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione ragione sociale del forni-  *
      *                  * tore preferenziale                          *
      *                  *---------------------------------------------*
           perform   vis-dcf-pfz-rag-000  thru vis-dcf-pfz-rag-999    .
       acc-dcf-pfz-430.
      *                  *---------------------------------------------*
      *                  * Se mancano sia l'anagrafica contabile che   *
      *                  * quella commerciale : reimpostazione         *
      *                  *---------------------------------------------*
           if        w-let-arc-fnt-flg    not  = spaces and
                     w-let-arc-dcf-flg    not  = spaces
                     go to acc-dcf-pfz-100.
      *                  *---------------------------------------------*
      *                  * Se manca l'anagrafica commerciale : messag- *
      *                  * gio e reimpostazione                        *
      *                  *---------------------------------------------*
           if        w-let-arc-dcf-flg    =    spaces
                     go to acc-dcf-pfz-440.
           move      "Mancano i dati commerciali del fornitore !"
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     acc-dcf-pfz-100.
       acc-dcf-pfz-440.
      *                  *---------------------------------------------*
      *                  * A dipendenze da impostazione                *
      *                  *---------------------------------------------*
           go to     acc-dcf-pfz-600.
       acc-dcf-pfz-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuale default descrizione prodotto per  *
      *                  * il fornitore                                *
      *                  *---------------------------------------------*
           if        w-sav-dcf-pfz        not  = zero
                     go to acc-dcf-pfz-700.
      *                      *-----------------------------------------*
      *                      * Test sul valore della personalizzazione *
      *                      *-----------------------------------------*
           if        w-prs-sna-igs-aaf    not  = "S"
                     go to acc-dcf-pfz-700.
           if        w-tes-dcf-pfz (1)    =    zero
                     go to acc-dcf-pfz-700.
      *                      *-----------------------------------------*
      *                      * Normalizzazione record [aaf]            *
      *                      *-----------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *                      *-----------------------------------------*
      *                      * Lettura descrizione prodotto per il     *
      *                      * fornitore                               *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "PRFNFM    "         to   f-key                  .
           move      w-tes-tip-mag        to   rf-aaf-tip-mag         .
           move      w-tes-num-map        to   rf-aaf-num-pro         .
           move      w-tes-dcf-pfz (1)    to   rf-aaf-cod-dcf         .
           move      spaces               to   rf-aaf-fda-pif         .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *                          *-------------------------------------*
      *                          * Test su esito della lettura         *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to acc-dcf-pfz-640.
       acc-dcf-pfz-620.
      *                          *-------------------------------------*
      *                          * Se record trovato                   *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Descrizione letta               *
      *                              *---------------------------------*
           move      rf-aaf-dep-sfn       to   w-tes-dep-sfn (1)      .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     acc-dcf-pfz-700.
       acc-dcf-pfz-640.
      *                          *-------------------------------------*
      *                          * Se record non trovato               *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Descrizione del prodotto per il *
      *                              * fornitore a spaces come default *
      *                              *---------------------------------*
           move      spaces               to   w-tes-dep-sfn (1)      .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     acc-dcf-pfz-700.
       acc-dcf-pfz-700.
      *                  *---------------------------------------------*
      *                  * Eventuale normalizzazione campo di comodo   *
      *                  * per la descrizione per il fornitore         *
      *                  *---------------------------------------------*
           if        w-tes-dep-sfn (1)    =    spaces
                     move  01             to   w-acc-opz-des-acc      .
      *                  *---------------------------------------------*
      *                  * Eventuale normalizzazione di codice, des-   *
      *                  * crizione ed altri valori specifici per il   *
      *                  * fornitore                                   *
      *                  *---------------------------------------------*
           if        w-tes-dcf-pfz (1)    not  = zero
                     go to acc-dcf-pfz-800.
      *                  *---------------------------------------------*
      *                  * Normalizzazione di codice e descrizione per *
      *                  * il fornitore, % di sconto e lotto acquisto  *
      *                  *---------------------------------------------*
           move      spaces               to   w-tes-cop-sfn (1)      .
           move      spaces               to   w-tes-dep-sfn (1)      .
           move      zero                 to   w-tes-xdp-sfn (1)      .
           move      zero                 to   w-tes-psr-pes (1, 1)   .
           move      zero                 to   w-tes-psr-pes (1, 2)   .
           move      zero                 to   w-tes-psr-pes (1, 3)   .
           move      zero                 to   w-tes-psr-pes (1, 4)   .
           move      zero                 to   w-tes-psr-pes (1, 5)   .
           move      zero                 to   w-tes-lot-acq (1)      .
       acc-dcf-pfz-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dcf-pfz-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dcf-pfz-100.
       acc-dcf-pfz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice fornitore preferenziale    *
      *    *-----------------------------------------------------------*
       vis-dcf-pfz-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-dcf-pfz (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dcf-pfz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Ragione sociale fornitore pre-    *
      *    *                         ferenziale                        *
      *    *-----------------------------------------------------------*
       vis-dcf-pfz-rag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-dcf-pfz-rag (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dcf-pfz-rag-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Tempo di consegna            *
      *    *-----------------------------------------------------------*
       acc-tmp-cns-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione di codice e descrizione per *
      *                  * il fornitore e % di sconto e lotto acquisto *
      *                  *---------------------------------------------*
           perform   vis-cop-sfn-000      thru vis-cop-sfn-999        .
           perform   vis-dep-sfn-000      thru vis-dep-sfn-999        .
           perform   vis-psr-pes-000      thru vis-psr-pes-999        .
           perform   vis-lot-acq-000      thru vis-lot-acq-999        .
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
           move      19                   to   v-lin                  .
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
           move      19                   to   v-lin                  .
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
      *    * Accettazione campo testata : Tempo di consegna medio      *
      *    *-----------------------------------------------------------*
       acc-tmp-cnm-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tmp-cnm-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      19                   to   v-lin                  .
           move      74                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-tmp-cnm (1)    to   v-num                  .
           if        w-prs-mde-tdc        =    1
                     divide   7           into v-num
                                               rounded                .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-tmp-cnm-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-tmp-cnm-999.
       acc-tmp-cnm-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-tmp-cnm (1)      .
           if        w-prs-mde-tdc        =    1
                     multiply  7          by   w-tes-tmp-cnm (1)      .
       acc-tmp-cnm-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tmp-cnm-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tmp-cnm-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-tmp-cnm-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-tmp-cnm-100.
       acc-tmp-cnm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tempo di consegna medio           *
      *    *-----------------------------------------------------------*
       vis-tmp-cnm-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      19                   to   v-lin                  .
           move      74                   to   v-pos                  .
           move      w-tes-tmp-cnm (1)    to   v-num                  .
           if        w-prs-mde-tdc        =    1
                     divide   7           into v-num
                                               rounded                .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tmp-cnm-999.
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
           move      07                   to   v-car                  .
           move      w-tes-dec-qta (1)    to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      20                   to   v-lin                  .
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
           move      07                   to   v-car                  .
           move      w-tes-dec-qta (1)    to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-lot-acq (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-lot-acq-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice prodotto fornitore    *
      *    *-----------------------------------------------------------*
       acc-cop-sfn-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-dcf-pfz (1)    =    zero
                     go to acc-cop-sfn-999.
       acc-cop-sfn-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
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
           move      07                   to   v-lin                  .
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
           move      "QS#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-opz-des-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
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
           move      08                   to   v-lin                  .
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
           if        w-tes-dcf-pfz (1)    =    zero
                     go to acc-dep-sfn-999.
           if        w-acc-opz-des-acc    =    01
                     go to acc-dep-sfn-999.
      *                  *---------------------------------------------*
      *                  * Preparazione default                        *
      *                  *---------------------------------------------*
           if        w-tes-dep-sfn (1)    =    spaces
                     move  w-tes-des-doc (1)
                                        to   w-tes-dep-sfn (1)      .
       acc-dep-sfn-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "T"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-ldt                  .
           move      09                   to   v-lin                  .
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
      *                  * Compattamento verso l'alto delle righe di   *
      *                  * descrizione                                 *
      *                  *---------------------------------------------*
           if        w-tes-dep-sfn (1)    =    spaces
                     go to acc-dep-sfn-600.
           move      w-tes-dep-sfn (1)    to   w-cmp-rig-des-des      .
           perform   cmp-rig-des-000      thru cmp-rig-des-999        .
           move      w-cmp-rig-des-des    to   w-tes-dep-sfn (1)      .
      *                  *---------------------------------------------*
      *                  * Se compattamento avvenuto : reimpostazione  *
      *                  *---------------------------------------------*
           if        w-cmp-rig-des-flg    not  = spaces
                     go to acc-dep-sfn-100.
       acc-dep-sfn-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di estensione alla descrizione         *
      *                  *---------------------------------------------*
           if        w-cmp-rig-des-nrd    not  > 1
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
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-dep-sfn (1)    to   v-txt                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dep-sfn-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Numero decimali prezzo di    *
      *    *                              acquisto                     *
      *    *-----------------------------------------------------------*
       acc-dpr-acq-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-dpr-acq (1)    to   w-sav-dpr-acq          .
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-tip-pza (1)    =    02
                     go to acc-dpr-acq-999.
       acc-dpr-acq-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-dpr-acq (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dpr-acq-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dpr-acq-999.
       acc-dpr-acq-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-dpr-acq (1)      .
       acc-dpr-acq-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Il numero decimali prezzo non puo' essere   *
      *                  * maggiore di 2                               *
      *                  *---------------------------------------------*
           if        w-tes-dpr-acq (1)    not  > 2
                     go to acc-dpr-acq-600.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Il numero decimali non puo' essere maggiore di 2 !
      -              "      "             to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * Reimpostazione                          *
      *                      *-----------------------------------------*
           go to     acc-dpr-acq-100.
       acc-dpr-acq-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore attuale come precedente : oltre   *
      *                  *---------------------------------------------*
           if        w-tes-dpr-acq (1)    =    w-sav-dpr-acq
                     go to acc-dpr-acq-800.
      *                  *---------------------------------------------*
      *                  * Trattamento prezzo di acquisto              *
      *                  *---------------------------------------------*
           if        w-tes-prz-pes (1)    =    zero
                     go to acc-dpr-acq-800.
      *                      *-----------------------------------------*
      *                      * Aggiornamento prezzo di acquisto        *
      *                      *-----------------------------------------*
           subtract  w-sav-dpr-acq        from w-tes-dpr-acq (1)
                                        giving w-acc-dpr-acq-wdd      .
           move      w-tes-prz-pes (1)    to   w-acc-dpr-acq-wpz      .
           if        w-acc-dpr-acq-wdd    <    zero
                     go to acc-dpr-acq-620.
           if        w-acc-dpr-acq-wdd    =    1
                     multiply 10          by   w-acc-dpr-acq-wpz
           else if   w-acc-dpr-acq-wdd    =    2
                     multiply 100         by   w-acc-dpr-acq-wpz      .
           go to     acc-dpr-acq-640.
       acc-dpr-acq-620.
           if        w-acc-dpr-acq-wdd    =    -1
                     divide   10          into w-acc-dpr-acq-wpz
                                               rounded
           else if   w-acc-dpr-acq-wdd    =    -2
                     divide   100         into w-acc-dpr-acq-wpz
                                               rounded                .
           go to     acc-dpr-acq-640.
       acc-dpr-acq-640.
           move      w-acc-dpr-acq-wpz    to   w-tes-prz-pes (1)      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione prezzo di acquisto      *
      *                      *-----------------------------------------*
           perform   vis-prz-pes-000      thru vis-prz-pes-999        .
       acc-dpr-acq-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dpr-acq-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dpr-acq-100.
       acc-dpr-acq-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Numero decimali prezzo            *
      *    *-----------------------------------------------------------*
       vis-dpr-acq-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-dpr-acq (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dpr-acq-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Sigla valuta di acquisto     *
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
      *                  *---------------------------------------------*
      *                  * Sigla valuta base                           *
      *                  *---------------------------------------------*
           move      c-sgl                to   w-tes-sgl-vlt (1)      .
       acc-sgl-vlt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-zvl-ope      .
           move      w-tes-sgl-vlt (1)    to   w-cod-cod-zvl-cod      .
           move      11                   to   w-cod-cod-zvl-lin      .
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
           move      w-let-arc-zvl-dec    to   w-tes-sgl-vlt-dec (1)  .
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
           move      11                   to   v-lin                  .
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
           move      11                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-sgl-vlt-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sgl-vlt-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Tipo prezzo d'acquisto       *
      *    *-----------------------------------------------------------*
       acc-tip-pza-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-dcf-pfz (1)    =    zero
                     go to acc-tip-pza-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-tip-pza (1)    to   w-sav-tip-pza          .
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
           move      13                   to   v-lin                  .
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
           move      w-tes-dpr-acq (1)    to   w-tes-dpr-acq (1)      .
           perform   vis-dpr-acq-000      thru vis-dpr-acq-999        .
       acc-tip-pza-620.
      *                  *---------------------------------------------*
      *                  * Trattamento prezzo e sconti                 *
      *                  *---------------------------------------------*
           if        w-tes-tip-pza (1)    =    w-sav-tip-pza
                     go to acc-tip-pza-800.
           if        w-tes-tip-pza (1)    not  = 02
                     go to acc-tip-pza-800.
           move      zero                 to   w-tes-prz-pes (1)      .
           move      zero                 to   w-tes-psr-pes (1, 1)   .
           move      zero                 to   w-tes-psr-pes (1, 2)   .
           move      zero                 to   w-tes-psr-pes (1, 3)   .
           move      zero                 to   w-tes-psr-pes (1, 4)   .
           move      zero                 to   w-tes-psr-pes (1, 5)   .
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
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-pza-lun    to   v-car                  .
           move      w-exp-tip-pza-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-pza-tbl    to   v-txt                  .
           move      w-tes-tip-pza (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-pza-999.
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
      *                      * Se tipo prezzo d'acquisto : stesso      *
      *                      * prezzo del listino base                 *
      *                      *-----------------------------------------*
           if        w-tes-tip-pza (1)    not  = 02
                     go to acc-prz-pes-100.
      *                          *-------------------------------------*
      *                          * Normalizzazione prezzo d'acquisto   *
      *                          *-------------------------------------*
           move      zero                 to   w-tes-prz-pes (1)      .
      *                          *-------------------------------------*
      *                          * Visualizzazione  prezzo d'acquisto  *
      *                          *-------------------------------------*
           perform   vis-prz-pes-000      thru vis-prz-pes-999        .
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
           move      w-tes-sgl-vlt-dec (1)
                                          to   v-dec                  .
           add       w-tes-dpr-acq (1)    to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-prz-pes (1)    to   v-num                  .
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
           move      v-num                to   w-tes-prz-pes (1)      .
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
      *    * Visualizzazione campo : Prezzo di acquisto                *
      *    *-----------------------------------------------------------*
       vis-prz-pes-000.
      *              *-------------------------------------------------*
      *              * Editing                                         *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      w-tes-sgl-vlt-dec (1)
                                          to   v-dec                  .
           add       w-tes-dpr-acq (1)    to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      w-tes-prz-pes (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      v-edt                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-prz-pes-999.
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
           if        w-tes-dcf-pfz (1)    =    zero
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
           move      15                   to   v-lin                  .
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
                    (1, w-edt-psr-pes-c01)
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
                                              (1, w-edt-psr-pes-c01)  .
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
                    (1, w-edt-psr-pes-c01)
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
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-edt-psr-pes-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-psr-pes-999.
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
           move      21                   to   v-lin                  .
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
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-uda-pes (1)    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-uda-pes-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Scorta minima                *
      *    *-----------------------------------------------------------*
       acc-sco-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore attuale                  *
      *                  *---------------------------------------------*
           move      w-tes-sco-min (1, 1) to   w-sav-sco-min          .
       acc-sco-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      w-tes-dec-qta (1)    to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-sco-min (1, 1) to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-sco-min-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-sco-min-999.
       acc-sco-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-sco-min (1, 1)   .
       acc-sco-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-sco-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore variato                           *
      *                  *---------------------------------------------*
           if        w-tes-sco-min (1, 1) =    w-sav-sco-min
                     go to acc-sco-min-800.
      *                  *---------------------------------------------*
      *                  * Data attuale in data ultimo aggiornamento   *
      *                  * scorta minima                               *
      *                  *---------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-tes-dua-min (1, 1)   .
      *                  *---------------------------------------------*
      *                  * Visualizzazione data ultimo aggiornamento   *
      *                  * scorta minima                               *
      *                  *---------------------------------------------*
           perform   vis-dua-min-000      thru vis-dua-min-999        .
       acc-sco-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-sco-min-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-sco-min-100.
       acc-sco-min-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Scorta minima             *
      *    *-----------------------------------------------------------*
       vis-sco-min-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      w-tes-dec-qta (1)    to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-sco-min (1, 1) to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sco-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Data ultimo aggiornamento    *
      *    *                              scorta minima                *
      *    *-----------------------------------------------------------*
       acc-dua-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dua-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      07                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-dua-min (1, 1) to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dua-min-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dua-min-999.
       acc-dua-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   w-tes-dua-min (1, 1)   .
       acc-dua-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dua-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dua-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dua-min-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dua-min-100.
       acc-dua-min-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Data ultimo aggiornamento *
      *    *                                 scorta minima             *
      *    *-----------------------------------------------------------*
       vis-dua-min-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      07                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      w-tes-dua-min (1, 1) to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dua-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Scorta di sicurezza          *
      *    *-----------------------------------------------------------*
       acc-sco-sic-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore attuale                  *
      *                  *---------------------------------------------*
           move      w-tes-sco-sic (1, 1) to   w-sav-sco-sic          .
       acc-sco-sic-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      w-tes-dec-qta (1)    to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-sco-sic (1, 1) to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-sco-sic-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-sco-sic-999.
       acc-sco-sic-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-sco-sic (1, 1)   .
       acc-sco-sic-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore variato                           *
      *                  *---------------------------------------------*
           if        w-tes-sco-sic (1, 1) =    w-sav-sco-sic
                     go to acc-sco-sic-800.
      *                  *---------------------------------------------*
      *                  * Data attuale in data ultimo aggiornamento   *
      *                  * scorta di sicurezza                         *
      *                  *---------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-tes-dua-sic (1, 1)   .
      *                  *---------------------------------------------*
      *                  * Visualizzazione data ultimo aggiornamento   *
      *                  * scorta di sicurezza                         *
      *                  *---------------------------------------------*
           perform   vis-dua-sic-000      thru vis-dua-sic-999        .
       acc-sco-sic-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sco-sic-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-sco-sic-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-sco-sic-100.
       acc-sco-sic-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Scorta di sicurezza       *
      *    *-----------------------------------------------------------*
       vis-sco-sic-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      w-tes-dec-qta (1)    to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-sco-sic (1, 1) to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sco-sic-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Data ultimo aggiornamento    *
      *    *                              scorta di sicurezza          *
      *    *-----------------------------------------------------------*
       acc-dua-sic-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dua-sic-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      09                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-dua-sic (1, 1) to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dua-sic-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dua-sic-999.
       acc-dua-sic-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   w-tes-dua-sic (1, 1)   .
       acc-dua-sic-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dua-sic-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dua-sic-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dua-sic-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dua-sic-100.
       acc-dua-sic-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Data ultimo aggiornamento *
      *    *                                 scorta di sicurezza       *
      *    *-----------------------------------------------------------*
       vis-dua-sic-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      09                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      w-tes-dua-sic (1, 1) to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dua-sic-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Scorta massima               *
      *    *-----------------------------------------------------------*
       acc-sco-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore attuale                  *
      *                  *---------------------------------------------*
           move      w-tes-sco-max (1, 1) to   w-sav-sco-max          .
       acc-sco-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      w-tes-dec-qta (1)    to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-sco-max (1, 1) to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-sco-max-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-sco-max-999.
       acc-sco-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-sco-max (1, 1)   .
       acc-sco-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-sco-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore variato                           *
      *                  *---------------------------------------------*
           if        w-tes-sco-max (1, 1) =    w-sav-sco-max
                     go to acc-sco-max-800.
      *                  *---------------------------------------------*
      *                  * Data attuale in data ultimo aggiornamento   *
      *                  * scorta massima                              *
      *                  *---------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-tes-dua-max (1, 1)   .
      *                  *---------------------------------------------*
      *                  * Visualizzazione data ultimo aggiornamento   *
      *                  * scorta massima                              *
      *                  *---------------------------------------------*
           perform   vis-dua-max-000      thru vis-dua-max-999        .
       acc-sco-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-sco-max-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-sco-max-100.
       acc-sco-max-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Scorta massima            *
      *    *-----------------------------------------------------------*
       vis-sco-max-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      w-tes-dec-qta (1)    to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-sco-max (1, 1) to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sco-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Data ultimo aggiornamento    *
      *    *                              scorta massima               *
      *    *-----------------------------------------------------------*
       acc-dua-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dua-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      11                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           move      w-tes-dua-max (1, 1) to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dua-max-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dua-max-999.
       acc-dua-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   w-tes-dua-max (1, 1)   .
       acc-dua-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dua-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dua-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dua-max-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dua-max-100.
       acc-dua-max-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Data ultimo aggiornamento *
      *    *                                 scorta massima            *
      *    *-----------------------------------------------------------*
       vis-dua-max-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      11                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      w-tes-dua-max (1, 1) to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dua-max-999.
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
           if        w-tes-num-map        =    zero
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
      *              *-------------------------------------------------*
      *              * Controllo che il valore impostato sia unico in  *
      *              * archivio                                        *
      *              *-------------------------------------------------*
           move      w-tes-num-map        to   w-ctl-uni-alf-num      .
           move      w-tes-alf-map (1)    to   w-ctl-uni-alf-alf      .
           perform   ctl-uni-alf-000      thru ctl-uni-alf-999        .
           if        w-ctl-uni-alf-flg    =    spaces
                     go to cnt-tdo-nok-002.
           move      "Codice materia prima gia' utilizzato !"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-002.
      *              *-------------------------------------------------*
      *              * Controllo su descrizione per uso interno        *
      *              *-------------------------------------------------*
           if        w-tes-des-int (1)    not  = spaces
                     go to cnt-tdo-nok-010.
           move      "Manca la descrizione per uso interno"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-010.
      *              *-------------------------------------------------*
      *              * Controllo su gruppo merceologico                *
      *              *-------------------------------------------------*
           if        w-tes-cla-map (1)    =    zero
                     go to cnt-tdo-nok-020.
           if        w-tes-cla-map-sud (1)
                                          =    01
                     go to cnt-tdo-nok-020.
           if        w-tes-gru-map (1)    not  = zero
                     go to cnt-tdo-nok-020.
           move      "Manca il codice gruppo merceologico"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-020.
      *              *-------------------------------------------------*
      *              * Controllo su sottogruppo merceologico           *
      *              *-------------------------------------------------*
           if        w-tes-cla-map (1)    =    zero
                     go to cnt-tdo-nok-100.
           if        w-tes-gru-map (1)    =    zero
                     go to cnt-tdo-nok-100.
           if        w-tes-cla-map-sud (1)
                                          =    01
                     go to cnt-tdo-nok-100.
           if        w-tes-gru-map-sud (1)
                                          =    01
                     go to cnt-tdo-nok-100.
           if        w-tes-sgr-map (1)    not  = zero
                     go to cnt-tdo-nok-100.
           move      "Manca il codice sottogruppo merceologico"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-100.
      *              *-------------------------------------------------*
      *              * Controlli per status                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data rilevamento status                     *
      *                  *---------------------------------------------*
           if        w-tes-sta-tus (1)    =    01 or
                     w-tes-sta-tus (1)    =    99
                     go to cnt-tdo-nok-120.
           if        w-tes-sta-tud (1)    not  = zero
                     go to cnt-tdo-nok-120.
           move      "Manca la data di rilevamento dello status !       
      -              "          "         to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-120.
      *                  *---------------------------------------------*
      *                  * Codice di riferimento                       *
      *                  *---------------------------------------------*
           if        w-tes-sta-tus (1)    not  = 21  and
                     w-tes-sta-tus (1)    not  = 52  and
                     w-tes-sta-tus (1)    not  = 72
                     go to cnt-tdo-nok-500.
           if        w-tes-sta-tuc (1)    =    zero
                     go to cnt-tdo-nok-125.
      *                  *---------------------------------------------*
      *                  * Lettura per controllo                       *
      *                  *---------------------------------------------*
           move      w-tes-sta-tuc (1)    to   w-let-arc-dpm-num      .
           perform   let-arc-dpm-000      thru let-arc-dpm-999        .
           if        w-let-arc-dpm-flg    =    spaces
                     go to cnt-tdo-nok-500.
           move      "Codice di riferimento non esistente !             
      -              "          "         to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-125.
           move      "Manca il codice di riferimento !                  
      -              "          "         to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-500.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
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
           move      zero                 to   w-tes-num-map          .
           move      spaces               to   w-tes-num-map-aut      .
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *-----------------------------------------------------------*
       nor-nok-tes-000.
           move      zero                 to   w-tes-ide-dat (1)      .
           move      spaces               to   w-tes-ide-ute (1)      .
           move      spaces               to   w-tes-ide-fas (1)      .
           move      spaces               to   w-tes-alf-map (1)      .
           move      spaces               to   w-tes-syn-map (1)      .
           move      spaces               to   w-tes-des-key (1)      .
           move      spaces               to   w-tes-des-int (1)      .
           move      zero                 to   w-tes-cla-map (1)      .
           move      spaces               to   w-tes-cla-map-des (1)  .
           move      zero                 to   w-tes-cla-map-sud (1)  .
           move      spaces               to   w-tes-cla-map-umi (1)  .
           move      zero                 to   w-tes-gru-map (1)      .
           move      spaces               to   w-tes-gru-map-des (1)  .
           move      zero                 to   w-tes-gru-map-sud (1)  .
           move      spaces               to   w-tes-gru-map-umi (1)  .
           move      zero                 to   w-tes-sgr-map (1)      .
           move      spaces               to   w-tes-sgr-map-des (1)  .
           move      zero                 to   w-tes-sgr-map-sud (1)  .
           move      spaces               to   w-tes-sgr-map-umi (1)  .
           move      zero                 to   w-tes-tip-map (1)      .
           move      zero                 to   w-tes-tip-cfz (1)      .
           move      zero                 to   w-tes-qta-cfz (1)      .
           move      zero                 to   w-tes-pes-uni (1)      .
           move      zero                 to   w-tes-pes-tar (1)      .
           move      zero                 to   w-tes-vol-uni (1)      .
           move      zero                 to   w-tes-dim-lar (1)      .
           move      zero                 to   w-tes-dim-alt (1)      .
           move      zero                 to   w-tes-dim-prf (1)      .
           move      spaces               to   w-tes-pcl-fis (1)      .
           move      zero                 to   w-tes-coe-mol (1)      .
           move      zero                 to   w-tes-coe-div (1)      .
      *
           move      zero                 to   w-tes-sco-min (1, 1)   .
           move      zero                 to   w-tes-sco-min (1, 2)   .
           move      zero                 to   w-tes-sco-min (1, 3)   .
           move      zero                 to   w-tes-sco-min (1, 4)   .
           move      zero                 to   w-tes-dua-min (1, 1)   .
           move      zero                 to   w-tes-dua-min (1, 2)   .
           move      zero                 to   w-tes-dua-min (1, 3)   .
           move      zero                 to   w-tes-dua-min (1, 4)   .
           move      zero                 to   w-tes-sco-sic (1, 1)   .
           move      zero                 to   w-tes-sco-sic (1, 2)   .
           move      zero                 to   w-tes-sco-sic (1, 3)   .
           move      zero                 to   w-tes-sco-sic (1, 4)   .
           move      zero                 to   w-tes-dua-sic (1, 1)   .
           move      zero                 to   w-tes-dua-sic (1, 2)   .
           move      zero                 to   w-tes-dua-sic (1, 3)   .
           move      zero                 to   w-tes-dua-sic (1, 4)   .
           move      zero                 to   w-tes-sco-max (1, 1)   .
           move      zero                 to   w-tes-sco-max (1, 2)   .
           move      zero                 to   w-tes-sco-max (1, 3)   .
           move      zero                 to   w-tes-sco-max (1, 4)   .
           move      zero                 to   w-tes-dua-max (1, 1)   .
           move      zero                 to   w-tes-dua-max (1, 2)   .
           move      zero                 to   w-tes-dua-max (1, 3)   .
           move      zero                 to   w-tes-dua-max (1, 4)   .
      *
           move      spaces               to   w-tes-umi-prd (1)      .
           move      spaces               to   w-tes-umi-prd-des (1)  .
           move      zero                 to   w-tes-dec-qta (1)      .
           move      zero                 to   w-tes-snx-2qt (1)      .
           move      zero                 to   w-tes-dec-2qt (1)      .
           move      zero                 to   w-tes-snx-3qt (1)      .
           move      zero                 to   w-tes-dec-3qt (1)      .
           move      spaces               to   w-tes-tip-vpr (1)      .
           move      spaces               to   w-tes-tip-vpr-des (1)  .
           move      zero                 to   w-tes-cod-s01 (1)      .
           move      spaces               to   w-tes-cod-s01-des (1)  .
           move      zero                 to   w-tes-cod-s02 (1)      .
           move      spaces               to   w-tes-cod-s02-des (1)  .
           move      zero                 to   w-tes-cod-s03 (1)      .
           move      spaces               to   w-tes-cod-s03-des (1)  .
           move      zero                 to   w-tes-cla-bdg (1)      .
           move      zero                 to   w-tes-dat-iim (1)      .
           move      zero                 to   w-tes-sta-tus (1)      .
           move      zero                 to   w-tes-sta-tud (1)      .
           move      zero                 to   w-tes-sta-tuc (1)      .
           move      spaces               to   w-tes-sta-tuc-alf (1)  .
           move      spaces               to   w-tes-sta-tuc-des (1)  .
           move      zero                 to   w-tes-sta-tux (1)      .
           move      spaces               to   w-tes-alx-exp (1)      .
      *              *-------------------------------------------------*
      *              * Normalizzazione dati per la gestione acquisti   *
      *              *-------------------------------------------------*
           perform   nor-nok-tes-acq-000  thru nor-nok-tes-acq-999    .
       nor-nok-tes-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *                                                           *
      *    * Normalizzazione dati per la gestione acquisti             *
      *    *-----------------------------------------------------------*
       nor-nok-tes-acq-000.
      *              *-------------------------------------------------*
      *              * Test su personalizzazione                       *
      *              *-------------------------------------------------*
           if        w-prs-sna-igs-aaq    not  = "S"
                     go to nor-nok-tes-acq-900.
       nor-nok-tes-acq-100.
      *              *-------------------------------------------------*
      *              * Normalizzazione                                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-tes-iva-acq (1)      .
           move      spaces               to   w-tes-iva-acq-des (1)  .
           move      zero                 to   w-tes-ctp-acq (1)      .
           move      spaces               to   w-tes-ctp-acq-des (1)  .
           move      zero                 to   w-tes-cod-pdt (1)      .
           move      spaces               to   w-tes-cod-pdt-rag (1)  .
           move      spaces               to   w-tes-cdp-pdt (1)      .
           move      zero                 to   w-tes-tmp-cns (1)      .
           move      zero                 to   w-tes-tmp-cnm (1)      .
           move      zero                 to   w-tes-lot-acq (1)      .
           move      zero                 to   w-tes-dcf-pfz (1)      .
           move      spaces               to   w-tes-dpz-pfz (1)      .
           move      spaces               to   w-tes-dcf-pfz-rag (1)  .
           move      spaces               to   w-tes-cop-sfn (1)      .
           move      spaces               to   w-tes-dep-sfn (1)      .
           move      zero                 to   w-tes-xdp-sfn (1)      .
           move      zero                 to   w-tes-dpr-acq (1)      .
           move      zero                 to   w-tes-tip-pza (1)      .
           move      spaces               to   w-tes-sgl-vlt (1)      .
           move      spaces               to   w-tes-sgl-vlt-des (1)  .
           move      zero                 to   w-tes-sgl-vlt-dec (1)  .
           move      zero                 to   w-tes-prz-pes (1)      .
           move      zero                 to   w-tes-psr-pes (1, 1)   .
           move      zero                 to   w-tes-psr-pes (1, 2)   .
           move      zero                 to   w-tes-psr-pes (1, 3)   .
           move      zero                 to   w-tes-psr-pes (1, 4)   .
           move      zero                 to   w-tes-psr-pes (1, 5)   .
           move      spaces               to   w-tes-snx-tum (1)      .
           move      spaces               to   w-tes-umf-tum (1)      .
           move      zero                 to   w-tes-nde-tum (1)      .
           move      zero                 to   w-tes-cmo-tum (1)      .
           move      zero                 to   w-tes-cdi-tum (1)      .
           move      zero                 to   w-tes-uda-pes (1)      .
           move      zero                 to   w-tes-per-mpa (1)      .
           move      zero                 to   w-tes-cod-iva (1)      .
           move      spaces               to   w-tes-cod-iva-des (1)  .
           move      spaces               to   w-tes-lgv-vlt (1)      .
           move      spaces               to   w-tes-lgv-vlt-des (1)  .
           move      zero                 to   w-tes-lgv-dcv (1)      .
           move      zero                 to   w-tes-lgv-cdc (1)      .
           move      spaces               to   w-tes-lgv-tdc (1)      .
           move      zero                 to   w-tes-lgv-pdt (1)      .
      *              *-------------------------------------------------*
      *              * Normalizzazione comodi di accettazione          *
      *              *-------------------------------------------------*
           move      zero                 to   w-acc-opz-des-acc      .
       nor-nok-tes-acq-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     nor-nok-tes-acq-999.
       nor-nok-tes-acq-999.
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
      *              * Forzatura del tipo magazzino                    *
      *              *-------------------------------------------------*
           move      03                   to   w-tes-tip-mag          .
      *              *-------------------------------------------------*
      *              * Lettura                                         *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMMAP    "         to   f-key                  .
           move      w-tes-num-map        to   rf-dpm-num-map         .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
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
      *                          *-------------------------------------*
      *                          * Valori contenuti direttamente in    *
      *                          * record [dpm]                        *
      *                          *-------------------------------------*
           move      rf-dpm-ide-dat       to   w-tes-ide-dat (1)      .
           move      rf-dpm-ide-ute       to   w-tes-ide-ute (1)      .
           move      rf-dpm-ide-fas       to   w-tes-ide-fas (1)      .
           move      rf-dpm-alf-map       to   w-tes-alf-map (1)      .
           move      rf-dpm-syn-map       to   w-tes-syn-map (1)      .
           move      rf-dpm-des-key       to   w-tes-des-key (1)      .
           move      rf-dpm-des-map       to   w-tes-des-int (1)      .
           move      rf-dpm-cla-map       to   w-tes-cla-map (1)      .
           move      rf-dpm-gru-map       to   w-tes-gru-map (1)      .
           move      rf-dpm-sgr-map       to   w-tes-sgr-map (1)      .
           move      rf-dpm-tip-map       to   w-tes-tip-map (1)      .
           move      rf-dpm-tip-cfz       to   w-tes-tip-cfz (1)      .
           move      rf-dpm-qta-cfz       to   w-tes-qta-cfz (1)      .
           move      rf-dpm-pes-uni       to   w-tes-pes-uni (1)      .
           move      rf-dpm-pes-tar       to   w-tes-pes-tar (1)      .
           move      rf-dpm-vol-uni       to   w-tes-vol-uni (1)      .
           move      rf-dpm-dim-lar       to   w-tes-dim-lar (1)      .
           move      rf-dpm-dim-alt       to   w-tes-dim-alt (1)      .
           move      rf-dpm-dim-prf       to   w-tes-dim-prf (1)      .
           move      rf-dpm-pcl-fis       to   w-tes-pcl-fis (1)      .
           move      rf-dpm-coe-mol       to   w-tes-coe-mol (1)      .
           move      rf-dpm-coe-div       to   w-tes-coe-div (1)      .
           move      rf-dpm-umi-prd       to   w-tes-umi-prd (1)      .
           move      rf-dpm-dec-qta       to   w-tes-dec-qta (1)      .
           move      rf-dpm-snx-2qt       to   w-tes-snx-2qt (1)      .
           move      rf-dpm-dec-2qt       to   w-tes-dec-2qt (1)      .
           move      rf-dpm-snx-3qt       to   w-tes-snx-3qt (1)      .
           move      rf-dpm-dec-3qt       to   w-tes-dec-3qt (1)      .
           move      rf-dpm-tip-vpr       to   w-tes-tip-vpr (1)      .
           move      rf-dpm-cod-s01       to   w-tes-cod-s01 (1)      .
           move      rf-dpm-cod-s02       to   w-tes-cod-s02 (1)      .
           move      rf-dpm-cod-s03       to   w-tes-cod-s03 (1)      .
           move      rf-dpm-cla-bdg       to   w-tes-cla-bdg (1)      .
           move      rf-dpm-dat-iim       to   w-tes-dat-iim (1)      .
           move      rf-dpm-sta-tus       to   w-tes-sta-tus (1)      .
           move      rf-dpm-sta-tud       to   w-tes-sta-tud (1)      .
           move      rf-dpm-sta-tuc       to   w-tes-sta-tuc (1)      .
           move      rf-dpm-sta-tux       to   w-tes-sta-tux (1)      .
           move      rf-dpm-alx-exp       to   w-tes-alx-exp (1)      .
      *                          *-------------------------------------*
      *                          * Normalizzazione valori inseriti in  *
      *                          * momenti successivi                  *
      *                          *-------------------------------------*
           if        rf-dpm-dat-iim       not  numeric
                     move  zero           to   w-tes-dat-iim (1)      .
      *
           if        rf-dpm-sta-tus       not  numeric
                     move  01             to   w-tes-sta-tus (1)      .
      *
           if        rf-dpm-sta-tud       not  numeric
                     move  zero           to   w-tes-sta-tud (1)      .
      *
           if        rf-dpm-sta-tuc       not  numeric
                     move  zero           to   w-tes-sta-tuc (1)      .
      *
           if        rf-dpm-sta-tux       not  numeric
                     move  zero           to   w-tes-sta-tux (1)      .
      *
       rou-let-reg-250.
      *                          *-------------------------------------*
      *                          * Valori contenuti indirettamente in  *
      *                          * record [dpm]                        *
      *                          *-------------------------------------*
       rou-let-reg-300.
      *                              *---------------------------------*
      *                              * Lettura archivio [zm1]          *
      *                              *---------------------------------*
           move      w-tes-cla-map (1)    to   w-let-arc-zm1-cla      .
           perform   let-arc-zm1-000      thru let-arc-zm1-999        .
           move      w-let-arc-zm1-des    to   w-tes-cla-map-des (1)  .
           move      w-let-arc-zm1-sud    to   w-tes-cla-map-sud (1)  .
           move      w-let-arc-zm1-umi    to   w-tes-cla-map-umi (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [zm2]          *
      *                              *---------------------------------*
           move      w-tes-cla-map (1)    to   w-let-arc-zm2-cla      .
           move      w-tes-gru-map (1)    to   w-let-arc-zm2-gru      .
           perform   let-arc-zm2-000      thru let-arc-zm2-999        .
           move      w-let-arc-zm2-des    to   w-tes-gru-map-des (1)  .
           move      w-let-arc-zm2-sud    to   w-tes-gru-map-sud (1)  .
           move      w-let-arc-zm2-umi    to   w-tes-gru-map-umi (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [zm3]          *
      *                              *---------------------------------*
           move      w-tes-cla-map (1)    to   w-let-arc-zm3-cla      .
           move      w-tes-gru-map (1)    to   w-let-arc-zm3-gru      .
           move      w-tes-sgr-map (1)    to   w-let-arc-zm3-sgr      .
           perform   let-arc-zm3-000      thru let-arc-zm3-999        .
           move      w-let-arc-zm3-des    to   w-tes-sgr-map-des (1)  .
           move      w-let-arc-zm3-sud    to   w-tes-sgr-map-sud (1)  .
           move      w-let-arc-zm3-umi    to   w-tes-sgr-map-umi (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [zum]          *
      *                              *---------------------------------*
           move      w-tes-umi-prd (1)    to   w-let-arc-zum-cod      .
           perform   let-arc-zum-000      thru let-arc-zum-999        .
           move      w-let-arc-zum-des    to   w-tes-umi-prd-des (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [ztv]          *
      *                              *---------------------------------*
           move      w-tes-tip-vpr (1)    to   w-let-arc-ztv-cod      .
           perform   let-arc-ztv-000      thru let-arc-ztv-999        .
           move      w-let-arc-ztv-des    to   w-tes-tip-vpr-des (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [zms]          *
      *                              *---------------------------------*
           move      01                   to   w-let-arc-zms-tip      .
           move      w-tes-cod-s01 (1)    to   w-let-arc-zms-cod      .
           perform   let-arc-zms-000      thru let-arc-zms-999        .
           move      w-let-arc-zms-des    to   w-tes-cod-s01-des (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [zms]          *
      *                              *---------------------------------*
           move      02                   to   w-let-arc-zms-tip      .
           move      w-tes-cod-s02 (1)    to   w-let-arc-zms-cod      .
           perform   let-arc-zms-000      thru let-arc-zms-999        .
           move      w-let-arc-zms-des    to   w-tes-cod-s02-des (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [zms]          *
      *                              *---------------------------------*
           move      03                   to   w-let-arc-zms-tip      .
           move      w-tes-cod-s03 (1)    to   w-let-arc-zms-cod      .
           perform   let-arc-zms-000      thru let-arc-zms-999        .
           move      w-let-arc-zms-des    to   w-tes-cod-s03-des (1)  .
      *                              *---------------------------------*
      *                              * Lettura file [dpm] per codice   *
      *                              * prodotto di riferimento         *
      *                              *---------------------------------*
           move      w-tes-sta-tuc (1)    to   w-let-arc-dpm-num      .
           perform   let-arc-dpm-000      thru let-arc-dpm-999        .
           move      w-let-arc-dpm-des    to   w-tes-sta-tuc-des (1)  .
           move      w-let-arc-dpm-alf    to   w-tes-sta-tuc-alf (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [aaq]          *
      *                              *---------------------------------*
           perform   rou-let-reg-aaq-000  thru rou-let-reg-aaq-999    .
      *                              *---------------------------------*
      *                              * Lettura archivio [aaf]          *
      *                              *---------------------------------*
           perform   rou-let-reg-aaf-000  thru rou-let-reg-aaf-999    .
      *                              *---------------------------------*
      *                              * Lettura archivio [fbs]          *
      *                              *---------------------------------*
           perform   rou-let-reg-fbs-000  thru rou-let-reg-fbs-999    .
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
      *    * Lettura registrazione pre-esistente                       *
      *    *                                                           *
      *    * Subroutine per il caricamento eventuale dei dati relativi *
      *    * al sottoscorta se gestibili dal programma                 *
      *    *-----------------------------------------------------------*
       rou-let-reg-aaq-000.
      *              *-------------------------------------------------*
      *              * Test se la personalizzazione lo consente        *
      *              *-------------------------------------------------*
           if        w-prs-sna-igs-aaq    not  = "S"
                     go to rou-let-reg-aaq-900.
       rou-let-reg-aaq-100.
      *              *-------------------------------------------------*
      *              * Lettura                                         *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO    "         to   f-key                  .
           move      w-tes-tip-mag        to   rf-aaq-tip-mag         .
           move      w-tes-num-map        to   rf-aaq-num-pro         .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *                  *---------------------------------------------*
      *                  * Test su esito della lettura                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rou-let-reg-aaq-900.
       rou-let-reg-aaq-200.
      *              *-------------------------------------------------*
      *              * Se record trovato                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valori contenuti direttamente in record     *
      *                  * [aaq]                                       *
      *                  *---------------------------------------------*
           move      rf-aaq-cod-iva       to   w-tes-iva-acq (1)      .
           move      rf-aaq-ctp-acq       to   w-tes-ctp-acq (1)      .
           move      rf-aaq-dcf-pfz       to   w-tes-dcf-pfz (1)      .
           move      rf-aaq-dpz-pfz       to   w-tes-dpz-pfz (1)      .
           move      rf-aaq-sgl-vlt       to   w-tes-sgl-vlt (1)      .
           move      rf-aaq-dec-vlt       to   w-tes-sgl-vlt-dec (1)  .
           move      rf-aaq-dec-prz       to   w-tes-dpr-acq (1)      .
           move      rf-aaq-cod-pdt       to   w-tes-cod-pdt (1)      .
           move      rf-aaq-cdp-pdt       to   w-tes-cdp-pdt (1)      .
           move      rf-aaq-prz-acr       to   w-tes-prz-pes (1)      .
           move      rf-aaq-uda-par       to   w-tes-uda-pes (1)      .
           move      rf-aaq-tmp-apv       to   w-tes-tmp-cnm (1)      .
           move      rf-aaq-lot-acq       to   w-tes-lot-acq (1)      .
      *                  *---------------------------------------------*
      *                  * Valori contenuti indirettamente in record   *
      *                  * [aaq]                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura archivio [zci]                  *
      *                      *-----------------------------------------*
           move      w-tes-iva-acq (1)    to   w-let-arc-zci-cod      .
           perform   let-arc-zci-000      thru let-arc-zci-999        .
           move      w-let-arc-zci-des    to   w-tes-iva-acq-des (1)  .
           if        w-tes-iva-acq (1)    <    00100
                     move  spaces         to   w-tes-iva-acq-des (1)  .
      *                      *-----------------------------------------*
      *                      * Lettura archivio [pdc]                  *
      *                      *-----------------------------------------*
           move      w-tes-ctp-acq (1)    to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-tes-ctp-acq-des (1)  .
      *                      *-----------------------------------------*
      *                      * Lettura archivio [dcf]                  *
      *                      *-----------------------------------------*
           move      w-tes-dcf-pfz (1)    to   w-let-arc-dcf-fnt      .
           move      spaces               to   w-let-arc-dcf-dpz      .
           perform   let-arc-dcf-000      thru let-arc-dcf-999        .
           move      w-let-arc-dcf-rag    to   w-tes-dcf-pfz-rag (1)  .
      *                      *-----------------------------------------*
      *                      * Lettura archivio [pdt]                  *
      *                      *-----------------------------------------*
           move      w-tes-cod-pdt (1)    to   w-let-arc-pdt-cod      .
           perform   let-arc-pdt-000      thru let-arc-pdt-999        .
           move      w-let-arc-pdt-rag    to   w-tes-cod-pdt-rag (1)  .
      *                      *-----------------------------------------*
      *                      * Descrizione per sigla valuta            *
      *                      *-----------------------------------------*
           move      w-tes-sgl-vlt (1)    to   w-let-arc-zvl-cod      .
           perform   let-arc-zvl-000      thru let-arc-zvl-999        .
           move      w-let-arc-zvl-des    to   w-tes-sgl-vlt-des (1)  .
       rou-let-reg-aaq-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-let-reg-aaq-999.
       rou-let-reg-aaq-999.
           exit.

      *    *===========================================================*
      *    * Lettura registrazione pre-esistente                       *
      *    *                                                           *
      *    * Subroutine per il caricamento eventuale dei dati relativi *
      *    * al sottoscorta se gestibili dal programma                 *
      *    *-----------------------------------------------------------*
       rou-let-reg-aaf-000.
      *              *-------------------------------------------------*
      *              * Test se la personalizzazione lo consente        *
      *              *-------------------------------------------------*
           if        w-prs-sna-igs-aaf    not  = "S"
                     go to rou-let-reg-aaf-900.
       rou-let-reg-aaf-100.
      *              *-------------------------------------------------*
      *              * Normalizzazione                                 *
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
           move      w-tes-num-map        to   rf-aaf-num-pro         .
           move      w-tes-dcf-pfz (1)    to   rf-aaf-cod-dcf         .
           move      spaces               to   rf-aaf-fda-pif         .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *                  *---------------------------------------------*
      *                  * Test su esito della lettura                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rou-let-reg-aaf-900.
       rou-let-reg-aaf-200.
      *              *-------------------------------------------------*
      *              * Se record trovato                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valori contenuti direttamente in record     *
      *                  * [aaf]                                       *
      *                  *---------------------------------------------*
           move      rf-aaf-cop-sfn       to   w-tes-cop-sfn (1)      .
           move      rf-aaf-dep-sfn       to   w-tes-dep-sfn (1)      .
           move      rf-aaf-xdp-sfn       to   w-tes-xdp-sfn (1)      .
           move      rf-aaf-snx-tum       to   w-tes-snx-tum (1)      .
           move      rf-aaf-umf-tum       to   w-tes-umf-tum (1)      .
           move      rf-aaf-nde-tum       to   w-tes-nde-tum (1)      .
           move      rf-aaf-cmo-tum       to   w-tes-cmo-tum (1)      .
           move      rf-aaf-cdi-tum       to   w-tes-cdi-tum (1)      .
           move      rf-aaf-tmp-cns       to   w-tes-tmp-cns (1)      .
           move      rf-aaf-tip-pza       to   w-tes-tip-pza (1)      .
           move      rf-aaf-lot-acq       to   w-tes-lot-acq (1)      .
           move      rf-aaf-uda-pes       to   w-tes-uda-pes (1)      .
           move      rf-aaf-per-mpa       to   w-tes-per-mpa (1)      .
           move      rf-aaf-lgv-vlt       to   w-tes-lgv-vlt (1)      .
           move      rf-aaf-lgv-dcv       to   w-tes-lgv-dcv (1)      .
           move      rf-aaf-lgv-tdc       to   w-tes-lgv-tdc (1)      .
           move      rf-aaf-lgv-cdc       to   w-tes-lgv-cdc (1)      .
           move      rf-aaf-lgv-pdt       to   w-tes-lgv-pdt (1)      .
           move      rf-aaf-prz-pes (1)   to   w-tes-prz-pes (1)      .
           move      rf-aaf-psr-pes (1, 1)
                                          to   w-tes-psr-pes (1, 1)   .
           move      rf-aaf-psr-pes (1, 2)
                                          to   w-tes-psr-pes (1, 2)   .
           move      rf-aaf-psr-pes (1, 3)
                                          to   w-tes-psr-pes (1, 3)   .
           move      rf-aaf-psr-pes (1, 4)
                                          to   w-tes-psr-pes (1, 4)   .
           move      rf-aaf-psr-pes (1, 5)
                                          to   w-tes-psr-pes (1, 5)   .
      *                  *---------------------------------------------*
      *                  * Valori contenuti indirettamente in record   *
      *                  * [aaf]                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Opzione per la descrizione              *
      *                      *-----------------------------------------*
           if        rf-aaf-dep-sfn       =    spaces
                     move  01             to   w-acc-opz-des-acc
           else      move  02             to   w-acc-opz-des-acc      .
       rou-let-reg-aaf-300.
      *                      *-----------------------------------------*
      *                      * Lettura archivio [pdx]                  *
      *                      *-----------------------------------------*
           if        w-tes-xdp-sfn (1)    =    0
                     go to rou-let-reg-aaf-900.
      *                          *-------------------------------------*
      *                          * Start su file [pdx]                 *
      *                          *-------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "TRCLNG    "         to   f-key                  .
           move      33                   to   rf-pdx-tip-rec         .
           move      w-tes-dcf-pfz (1)    to   rf-pdx-cod-arc         .
           move      spaces               to   rf-pdx-cod-lng         .
           move      w-tes-num-map        to   rf-pdx-cod-num         .
           move      spaces               to   rf-pdx-for-mat         .
           move      zero                 to   rf-pdx-num-prg         .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                              *---------------------------------*
      *                              * Test su esito operazione        *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     go to rou-let-reg-aaf-900.
       rou-let-reg-aaf-320.
      *                          *-------------------------------------*
      *                          * Lettura sequenziale file [pdx]      *
      *                          *-------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                              *---------------------------------*
      *                              * Test se 'at end'                *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     go to rou-let-reg-aaf-900.
      *                          *-------------------------------------*
      *                          * Test sul massimo                    *
      *                          *-------------------------------------*
           if        rf-pdx-tip-rec       not  = 33                or
                     rf-pdx-cod-arc       not  = w-tes-dcf-pfz (1) or
                     rf-pdx-cod-lng       not  = spaces            or
                     rf-pdx-cod-num       not  = w-tes-num-map     or
                     rf-pdx-for-mat       not  = spaces
                     go to rou-let-reg-aaf-900.
      *                          *-------------------------------------*
      *                          * Bufferizzazione descrizione         *
      *                          *-------------------------------------*
           move      rf-pdx-des-pro       to   w-tes-dep-rig
                                              (1, rf-pdx-num-prg)     .
      *                          *-------------------------------------*
      *                          * Riciclo su lettura                  *
      *                          *-------------------------------------*
           go to     rou-let-reg-aaf-320.
       rou-let-reg-aaf-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-let-reg-aaf-999.
       rou-let-reg-aaf-999.
           exit.

      *    *===========================================================*
      *    * Lettura registrazione pre-esistente                       *
      *    *                                                           *
      *    * Subroutine per il caricamento eventuale dei dati relativi *
      *    * al sottoscorta se gestibili dal programma                 *
      *    *-----------------------------------------------------------*
       rou-let-reg-fbs-000.
      *              *-------------------------------------------------*
      *              * Test se la personalizzazione lo consente        *
      *              *-------------------------------------------------*
           if        w-prs-sna-igs-fbs    not  = "S"
                     go to rou-let-reg-fbs-900.
       rou-let-reg-fbs-100.
      *              *-------------------------------------------------*
      *              * Lettura                                         *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "MAGDPZ    "         to   f-key                  .
           move      w-tes-tip-mag        to   rf-fbs-tip-mag         .
           move      w-tes-num-map        to   rf-fbs-num-mag         .
           move      spaces               to   rf-fbs-var-mag         .
           move      01                   to   rf-fbs-cod-dpz         .
           move      "pgm/fab/fls/ioc/obj/ioffbs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fbs                 .
      *                  *---------------------------------------------*
      *                  * Test su esito della lettura                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rou-let-reg-fbs-900.
       rou-let-reg-fbs-200.
      *              *-------------------------------------------------*
      *              * Se record trovato                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valori contenuti direttamente in record     *
      *                  * [fbs]                                       *
      *                  *---------------------------------------------*
           move      rf-fbs-sco-min (1)   to   w-tes-sco-min (1, 1)   .
           move      rf-fbs-dua-min (1)   to   w-tes-dua-min (1, 1)   .
           move      rf-fbs-sco-sic (1)   to   w-tes-sco-sic (1, 1)   .
           move      rf-fbs-dua-sic (1)   to   w-tes-dua-sic (1, 1)   .
           move      rf-fbs-sco-max (1)   to   w-tes-sco-max (1, 1)   .
           move      rf-fbs-dua-max (1)   to   w-tes-dua-max (1, 1)   .
      *                  *---------------------------------------------*
      *                  * Valori contenuti indirettamente in record   *
      *                  * [fbs]                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura archivio [zts]                  *
      *                      *-----------------------------------------*
       rou-let-reg-fbs-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-let-reg-fbs-999.
       rou-let-reg-fbs-999.
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
      *              * Duplicazione record precedente se richiesto     *
      *              *-------------------------------------------------*
           if        w-cnt-dup-rec-flg    =    spaces
                     go to pre-acc-ins-200.
      *                  *---------------------------------------------*
      *                  * Routine di duplicazione record              *
      *                  *---------------------------------------------*
           perform   rou-dup-rec-000      thru rou-dup-rec-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pre-acc-ins-999.
       pre-acc-ins-200.
      *              *-------------------------------------------------*
      *              * Preparazione eventuali defaults per l'inseri-   *
      *              * mento                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Status commerciale                          *
      *                  *---------------------------------------------*
           move      01                   to   w-tes-sta-tus (1)      .
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
           move      spaces               to   w-cnt-sts-vis-ptx (2)  .
           move      spaces               to   w-cnt-sts-vis-ptx (3)  .
           move      spaces               to   w-cnt-sts-vis-ptx (4)  .
           move      spaces               to   w-cnt-sts-vis-ptx (5)  .
           move      spaces               to   w-cnt-sts-vis-ptx (6)  .
           move      spaces               to   w-cnt-sts-vis-ptx (7)  .
           move      spaces               to   w-cnt-sts-vis-ptx (8)  .
           move      spaces               to   w-cnt-sts-vis-ptx (9)  .
      *              *-------------------------------------------------*
      *              * Normalizzazione segnale di duplicazione         *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-dup-rec-flg      .
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
      *              *-------------------------------------------------*
      *              * Normalizzazione segnale di duplicazione         *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-dup-rec-flg      .
      *              *-------------------------------------------------*
      *              * Eventuale messaggio di status non normale       *
      *              *-------------------------------------------------*
           perform   box-msg-sts-000      thru box-msg-sts-999        .
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
      *              * Eventuale messaggio di status non normale       *
      *              *-------------------------------------------------*
           perform   box-msg-sts-000      thru box-msg-sts-999        .
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
      *              *-------------------------------------------------*
      *              * Se e' stata eseguita l'attribuzione del codice  *
      *              * in automatico, si ripristina, se possibile, il  *
      *              * codice al valore precedente l'incremento        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non attribuzione automatica : uscita     *
      *                  *---------------------------------------------*
           if        w-tes-num-map-aut    =    spaces
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
      *              * Trattamento file [dpm]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-500.
      *                      *-----------------------------------------*
      *                      * Write record [dpm]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-dpm-000      thru wrt-rec-dpm-999        .
      *                      *-----------------------------------------*
      *                      * Write record [aaq]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-aaq-000      thru wrt-rec-aaq-999        .
      *                      *-----------------------------------------*
      *                      * Write record [aaf]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-aaf-000      thru wrt-rec-aaf-999        .
      *                      *-----------------------------------------*
      *                      * Write record [fbs]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-fbs-000      thru wrt-rec-fbs-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     scr-mov-fil-999.
       scr-mov-fil-500.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rewrite record [dpm]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-dpm-000      thru rew-rec-dpm-999        .
      *                      *-----------------------------------------*
      *                      * Rewrite record [aaq]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-aaq-000      thru rew-rec-aaq-999        .
      *                      *-----------------------------------------*
      *                      * Rewrite record [aaf]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-aaf-000      thru rew-rec-aaf-999        .
      *                      *-----------------------------------------*
      *                      * Rewrite record [fbs]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-fbs-000      thru rew-rec-fbs-999        .
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *-----------------------------------------------------------*
       del-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Delete record [dpm]                             *
      *              *-------------------------------------------------*
           perform   del-rec-dpm-000      thru del-rec-dpm-999        .
      *              *-------------------------------------------------*
      *              * Delete record [aaq]                             *
      *              *-------------------------------------------------*
           perform   del-rec-aaq-000      thru del-rec-aaq-999        .
      *              *-------------------------------------------------*
      *              * Delete record [aaf]                             *
      *              *-------------------------------------------------*
           perform   del-rec-aaf-000      thru del-rec-aaf-999        .
      *              *-------------------------------------------------*
      *              * Delete record [fbs]                             *
      *              *-------------------------------------------------*
           perform   del-rec-fbs-000      thru del-rec-fbs-999        .
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [dpm]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-dpm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-tes-num-map        to   rf-dpm-num-map         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-dpm-ide-dat         .
           move      s-ute                to   rf-dpm-ide-ute         .
           move      s-fas                to   rf-dpm-ide-fas         .
           move      w-tes-alf-map (1)    to   rf-dpm-alf-map         .
           move      w-tes-syn-map (1)    to   rf-dpm-syn-map         .
           move      w-tes-des-key (1)    to   rf-dpm-des-key         .
           move      w-tes-des-int (1)    to   rf-dpm-des-map         .
           move      w-tes-cla-map (1)    to   rf-dpm-cla-map         .
           move      w-tes-gru-map (1)    to   rf-dpm-gru-map         .
           move      w-tes-sgr-map (1)    to   rf-dpm-sgr-map         .
           move      w-tes-tip-map (1)    to   rf-dpm-tip-map         .
           move      w-tes-tip-cfz (1)    to   rf-dpm-tip-cfz         .
           move      w-tes-qta-cfz (1)    to   rf-dpm-qta-cfz         .
           move      w-tes-pes-uni (1)    to   rf-dpm-pes-uni         .
           move      w-tes-pes-tar (1)    to   rf-dpm-pes-tar         .
           move      w-tes-vol-uni (1)    to   rf-dpm-vol-uni         .
           move      w-tes-dim-lar (1)    to   rf-dpm-dim-lar         .
           move      w-tes-dim-alt (1)    to   rf-dpm-dim-alt         .
           move      w-tes-dim-prf (1)    to   rf-dpm-dim-prf         .
           move      w-tes-pcl-fis (1)    to   rf-dpm-pcl-fis         .
           move      w-tes-coe-mol (1)    to   rf-dpm-coe-mol         .
           move      w-tes-coe-div (1)    to   rf-dpm-coe-div         .
           move      w-tes-umi-prd (1)    to   rf-dpm-umi-prd         .
           move      w-tes-dec-qta (1)    to   rf-dpm-dec-qta         .
           move      w-tes-snx-2qt (1)    to   rf-dpm-snx-2qt         .
           move      w-tes-dec-2qt (1)    to   rf-dpm-dec-2qt         .
           move      w-tes-snx-3qt (1)    to   rf-dpm-snx-3qt         .
           move      w-tes-dec-3qt (1)    to   rf-dpm-dec-3qt         .
           move      w-tes-tip-vpr (1)    to   rf-dpm-tip-vpr         .
           move      w-tes-cod-s01 (1)    to   rf-dpm-cod-s01         .
           move      w-tes-cod-s02 (1)    to   rf-dpm-cod-s02         .
           move      w-tes-cod-s03 (1)    to   rf-dpm-cod-s03         .
           move      w-tes-cla-bdg (1)    to   rf-dpm-cla-bdg         .
           move      w-tes-dat-iim (1)    to   rf-dpm-dat-iim         .
           move      w-tes-sta-tus (1)    to   rf-dpm-sta-tus         .
           move      w-tes-sta-tud (1)    to   rf-dpm-sta-tud         .
           move      w-tes-sta-tuc (1)    to   rf-dpm-sta-tuc         .
           move      w-tes-sta-tux (1)    to   rf-dpm-sta-tux         .
           move      w-tes-alx-exp (1)    to   rf-dpm-alx-exp         .
       cmp-rec-dpm-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [dpm]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-dpm-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-dpm-000      thru cmp-rec-dpm-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
       wrt-rec-dpm-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [dpm]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-dpm-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-dpm-000      thru cmp-rec-dpm-999        .
      *              *-------------------------------------------------*
      *              * Forced put record                               *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
       rew-rec-dpm-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [dpm]                                *
      *    *-----------------------------------------------------------*
       del-rec-dpm-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-dpm-000      thru cmp-rec-dpm-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
       del-rec-dpm-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [aaq]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-aaq-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-tes-tip-mag        to   rf-aaq-tip-mag         .
           move      w-tes-num-map        to   rf-aaq-num-pro         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-aaq-ide-dat         .
           move      s-ute                to   rf-aaq-ide-ute         .
           move      s-fas                to   rf-aaq-ide-fas         .
           move      w-tes-iva-acq (1)    to   rf-aaq-cod-iva         .
           move      w-tes-ctp-acq (1)    to   rf-aaq-ctp-acq         .
           move      w-tes-dcf-pfz (1)    to   rf-aaq-dcf-pfz         .
           move      w-tes-dpz-pfz (1)    to   rf-aaq-dpz-pfz         .
           move      w-tes-sgl-vlt (1)    to   rf-aaq-sgl-vlt         .
           move      w-tes-sgl-vlt-dec (1)
                                          to   rf-aaq-dec-vlt         .
           move      w-tes-dpr-acq (1)    to   rf-aaq-dec-prz         .
           move      w-tes-prz-pes (1)    to   rf-aaq-prz-acr         .
           move      w-tes-uda-pes (1)    to   rf-aaq-uda-par         .
           move      w-tes-cod-pdt (1)    to   rf-aaq-cod-pdt         .
           move      w-tes-cdp-pdt (1)    to   rf-aaq-cdp-pdt         .
           move      w-tes-tmp-cnm (1)    to   rf-aaq-tmp-apv         .
           move      w-tes-lot-acq (1)    to   rf-aaq-lot-acq         .
           move      01                   to   rf-aaq-epz-rgf         .
       cmp-rec-aaq-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [aaq]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-aaq-000.
      *              *-------------------------------------------------*
      *              * Test se record da trattare                      *
      *              *-------------------------------------------------*
           if        w-prs-sna-igs-aaq    not  = "S"
                     go to wrt-rec-aaq-999.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-aaq-000      thru cmp-rec-aaq-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
       wrt-rec-aaq-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [aaq]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-aaq-000.
      *              *-------------------------------------------------*
      *              * Test se record da trattare                      *
      *              *-------------------------------------------------*
           if        w-prs-sna-igs-aaq    not  = "S"
                     go to rew-rec-aaq-999.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-aaq-000      thru cmp-rec-aaq-999        .
      *              *-------------------------------------------------*
      *              * Forced put record                               *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
       rew-rec-aaq-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [aaq]                                *
      *    *-----------------------------------------------------------*
       del-rec-aaq-000.
      *              *-------------------------------------------------*
      *              * Test se record da trattare                      *
      *              *-------------------------------------------------*
           if        w-prs-sna-igs-aaq    not  = "S"
                     go to del-rec-aaq-999.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-aaq-000      thru cmp-rec-aaq-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
       del-rec-aaq-999.
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
           move      w-tes-num-map        to   rf-aaf-num-pro         .
           move      w-tes-dcf-pfz (1)    to   rf-aaf-cod-dcf         .
           move      spaces               to   rf-aaf-fda-pif         .
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
      *                      *-----------------------------------------*
      *                      * Trasformazione unita' di misura         *
      *                      *-----------------------------------------*
           move      w-tes-snx-tum (1)    to   rf-aaf-snx-tum         .
           move      w-tes-umf-tum (1)    to   rf-aaf-umf-tum         .
           move      w-tes-nde-tum (1)    to   rf-aaf-nde-tum         .
           move      w-tes-cmo-tum (1)    to   rf-aaf-cmo-tum         .
           move      w-tes-cdi-tum (1)    to   rf-aaf-cdi-tum         .
       cmp-rec-aaf-300.
           move      w-tes-tmp-cns (1)    to   rf-aaf-tmp-cns         .
           move      w-tes-sgl-vlt (1)    to   rf-aaf-sgl-vlt         .
           move      w-tes-sgl-vlt-dec (1)
                                          to   rf-aaf-dec-vlt         .
           move      w-tes-dpr-acq (1)    to   rf-aaf-dec-prz         .
           move      w-tes-tip-pza (1)    to   rf-aaf-tip-pza         .
           move      w-tes-lot-acq (1)    to   rf-aaf-lot-acq         .
           move      01                   to   rf-aaf-tap-pes         .
           move      w-tes-uda-pes (1)    to   rf-aaf-uda-pes         .
           move      w-tes-per-mpa (1)    to   rf-aaf-per-mpa         .
           move      spaces               to   rf-aaf-lgv-vlt         .
           move      zero                 to   rf-aaf-lgv-dcv         .
           move      spaces               to   rf-aaf-lgv-tdc         .
           move      zero                 to   rf-aaf-lgv-cdc         .
           move      zero                 to   rf-aaf-lgv-pdt         .
           move      w-tes-prz-pes (1)    to   rf-aaf-prz-pes (1)     .
           move      w-tes-psr-pes (1, 1) to   rf-aaf-psr-pes (1, 1)  .
           move      w-tes-psr-pes (1, 2) to   rf-aaf-psr-pes (1, 2)  .
           move      w-tes-psr-pes (1, 3) to   rf-aaf-psr-pes (1, 3)  .
           move      w-tes-psr-pes (1, 4) to   rf-aaf-psr-pes (1, 4)  .
           move      w-tes-psr-pes (1, 5) to   rf-aaf-psr-pes (1, 5)  .
       cmp-rec-aaf-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [aaf]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-aaf-000.
      *              *-------------------------------------------------*
      *              * Test se record da trattare                      *
      *              *-------------------------------------------------*
           if        w-prs-sna-igs-aaf    not  = "S"
                     go to wrt-rec-aaf-999.
      *              *-------------------------------------------------*
      *              * Test se codice fornitore preferenziale esiste   *
      *              *-------------------------------------------------*
           if        w-tes-dcf-pfz (1)    =    zero
                     go to wrt-rec-aaf-999.
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
      *              * Test se record da trattare                      *
      *              *-------------------------------------------------*
           if        w-prs-sna-igs-aaf    not  = "S"
                     go to rew-rec-aaf-999.
      *              *-------------------------------------------------*
      *              * Delete record preliminare                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Composizione chiave primaria                *
      *                  *---------------------------------------------*
           move      w-tes-tip-mag        to   rf-aaf-tip-mag         .
           move      w-tes-num-map        to   rf-aaf-num-pro         .
           move      w-tes-dcf-pfz (2)    to   rf-aaf-cod-dcf         .
           move      spaces               to   rf-aaf-fda-pif         .
      *                  *---------------------------------------------*
      *                  * Delete record                               *
      *                  *---------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
       rew-rec-aaf-200.
      *              *-------------------------------------------------*
      *              * Scrittura record [aaf]                          *
      *              *-------------------------------------------------*
           perform   wrt-rec-aaf-000      thru wrt-rec-aaf-999        .
       rew-rec-aaf-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [aaf]                                *
      *    *-----------------------------------------------------------*
       del-rec-aaf-000.
      *              *-------------------------------------------------*
      *              * Test se record da trattare                      *
      *              *-------------------------------------------------*
           if        w-prs-sna-igs-aaf    not  = "S"
                     go to del-rec-aaf-999.
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
      *    * Composizione record [fbs]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-fbs-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/fab/fls/ioc/obj/ioffbs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fbs                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-tes-tip-mag        to   rf-fbs-tip-mag         .
           move      w-tes-num-map        to   rf-fbs-num-mag         .
           move      spaces               to   rf-fbs-var-mag         .
           move      01                   to   rf-fbs-cod-dpz         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      spaces               to   rf-fbs-tip-stg         .
           move      w-tes-sco-min (1, 1) to   rf-fbs-sco-min (1)     .
           move      zero                 to   rf-fbs-sco-min (2)     .
           move      zero                 to   rf-fbs-sco-min (3)     .
           move      zero                 to   rf-fbs-sco-min (4)     .
           move      w-tes-dua-min (1, 1) to   rf-fbs-dua-min (1)     .
           move      zero                 to   rf-fbs-dua-min (2)     .
           move      zero                 to   rf-fbs-dua-min (3)     .
           move      zero                 to   rf-fbs-dua-min (4)     .
           move      w-tes-sco-sic (1, 1) to   rf-fbs-sco-sic (1)     .
           move      zero                 to   rf-fbs-sco-sic (2)     .
           move      zero                 to   rf-fbs-sco-sic (3)     .
           move      zero                 to   rf-fbs-sco-sic (4)     .
           move      w-tes-dua-sic (1, 1) to   rf-fbs-dua-sic (1)     .
           move      zero                 to   rf-fbs-dua-sic (2)     .
           move      zero                 to   rf-fbs-dua-sic (3)     .
           move      zero                 to   rf-fbs-dua-sic (4)     .
           move      w-tes-sco-max (1, 1) to   rf-fbs-sco-max (1)     .
           move      zero                 to   rf-fbs-sco-max (2)     .
           move      zero                 to   rf-fbs-sco-max (3)     .
           move      zero                 to   rf-fbs-sco-max (4)     .
           move      w-tes-dua-max (1, 1) to   rf-fbs-dua-max (1)     .
           move      zero                 to   rf-fbs-dua-max (2)     .
           move      zero                 to   rf-fbs-dua-max (3)     .
           move      zero                 to   rf-fbs-dua-max (4)     .
           move      spaces               to   rf-fbs-sco-not         .
           move      spaces               to   rf-fbs-exp-alf         .
           move      zero                 to   rf-fbs-exp-num         .
       cmp-rec-fbs-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [fbs]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-fbs-000.
      *              *-------------------------------------------------*
      *              * Test se record da trattare                      *
      *              *-------------------------------------------------*
           if        w-prs-sna-igs-fbs    not  = "S"
                     go to wrt-rec-fbs-999.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-fbs-000      thru cmp-rec-fbs-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/fab/fls/ioc/obj/ioffbs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fbs                 .
       wrt-rec-fbs-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [fbs]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-fbs-000.
      *              *-------------------------------------------------*
      *              * Test se record da trattare                      *
      *              *-------------------------------------------------*
           if        w-prs-sna-igs-fbs    not  = "S"
                     go to rew-rec-fbs-999.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-fbs-000      thru cmp-rec-fbs-999        .
      *              *-------------------------------------------------*
      *              * Forced put record                               *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/fab/fls/ioc/obj/ioffbs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fbs                 .
       rew-rec-fbs-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [fbs]                                *
      *    *-----------------------------------------------------------*
       del-rec-fbs-000.
      *              *-------------------------------------------------*
      *              * Test se record da trattare                      *
      *              *-------------------------------------------------*
           if        w-prs-sna-igs-fbs    not  = "S"
                     go to del-rec-fbs-999.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-fbs-000      thru cmp-rec-fbs-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/fab/fls/ioc/obj/ioffbs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fbs                 .
       del-rec-fbs-999.
           exit.

      *    *===========================================================*
      *    * Box per messaggio di status non 'normale'                 *
      *    *-----------------------------------------------------------*
       box-msg-sts-000.
      *              *-------------------------------------------------*
      *              * Test se messaggio da visualizzare               *
      *              *-------------------------------------------------*
           if        w-tes-sta-tus (1)    =    01
                     move  01             to   w-wrk-sta-tus-pnt
           else if   w-tes-sta-tus (1)    =    11
                     move  02             to   w-wrk-sta-tus-pnt
           else if   w-tes-sta-tus (1)    =    21
                     move  03             to   w-wrk-sta-tus-pnt
           else if   w-tes-sta-tus (1)    =    51
                     move  04             to   w-wrk-sta-tus-pnt
           else if   w-tes-sta-tus (1)    =    52
                     move  05             to   w-wrk-sta-tus-pnt
           else if   w-tes-sta-tus (1)    =    71
                     move  06             to   w-wrk-sta-tus-pnt
           else if   w-tes-sta-tus (1)    =    72
                     move  07             to   w-wrk-sta-tus-pnt
           else      move  zero           to   w-wrk-sta-tus-pnt      .
           if        w-wrk-sta-tus-pnt    <    02
                     go to box-msg-sts-900.
           if        w-wrk-sta-tus-pnt    >    07
                     go to box-msg-sts-900.
       box-msg-sts-100.
      *              *-------------------------------------------------*
      *              * Preparazione messaggio - riga 1                 *
      *              *-------------------------------------------------*
           move      w-exp-sta-tus-tbl    to   w-wrk-sta-tus-tbl      .
      *
           move      "Attenzione : lo status della materia prima richiam
      -              "ata e' :  "         to   w-err-box-err-msg      .
      *              *-------------------------------------------------*
      *              * Preparazione messaggio - riga 2                 *
      *              *-------------------------------------------------*
           move      65                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      w-wrk-sta-tus-ele
                    (w-wrk-sta-tus-pnt)   to   w-all-str-cat (1)      .
           if        w-tes-sta-tuc-alf (1)
                                          =    spaces
                     move  spaces         to   w-all-str-cat (2)
                     move  spaces         to   w-all-str-cat (3)
                     move  spaces         to   w-all-str-cat (4)
           else      move  "'"            to   w-all-str-cat (2)
                     move  w-tes-sta-tuc-alf (1)
                                          to   w-all-str-cat (3)
                     move  "'"            to   w-all-str-cat (4)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           move      w-all-str-alf        to   w-err-box-err-m02      .
           perform   box-msg-e02-000      thru box-msg-e02-999        .
       box-msg-sts-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     box-msg-sts-999.
       box-msg-sts-999.
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
      *    * Routine di compattamento righe di descrizione             *
      *    *-----------------------------------------------------------*
       cmp-rig-des-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di rivisualizzazione de-   *
      *              * scrizione                                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-cmp-rig-des-flg      .
      *              *-------------------------------------------------*
      *              * Determinazione numero righe attuale             *
      *              *-------------------------------------------------*
           move      11                   to   w-cmp-rig-des-nri      .
       cmp-rig-des-100.
           subtract  1                    from w-cmp-rig-des-nri      .
           if        w-cmp-rig-des-rig
                    (w-cmp-rig-des-nri)   =    spaces
                     go to cmp-rig-des-100.
      *              *-------------------------------------------------*
      *              * Inizializzazione numero righe descrizione       *
      *              *-------------------------------------------------*
           move      zero                 to   w-cmp-rig-des-nrd      .
      *              *-------------------------------------------------*
      *              * Ciclo per compattamento                         *
      *              *-------------------------------------------------*
           move      zero                 to   w-cmp-rig-des-ctr      .
       cmp-rig-des-200.
           add       1                    to   w-cmp-rig-des-ctr      .
           if        w-cmp-rig-des-ctr    >    w-cmp-rig-des-nri
                     go to cmp-rig-des-300.
      *                  *---------------------------------------------*
      *                  * Se riga di descrizione a spazi : riciclo    *
      *                  *---------------------------------------------*
           if        w-cmp-rig-des-rig
                    (w-cmp-rig-des-ctr)   =    spaces
                     go to cmp-rig-des-200.
      *                  *---------------------------------------------*
      *                  * Incremento contatore numero righe           *
      *                  *---------------------------------------------*
           add       1                    to   w-cmp-rig-des-nrd      .
      *                  *---------------------------------------------*
      *                  * Spostamento riga di descrizione             *
      *                  *---------------------------------------------*
           move      w-cmp-rig-des-rig
                    (w-cmp-rig-des-ctr)   to   w-cmp-rig-des-rig
                                              (w-cmp-rig-des-nrd)     .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     cmp-rig-des-200.
       cmp-rig-des-300.
      *              *-------------------------------------------------*
      *              * Abblencamento righe residue                     *
      *              *-------------------------------------------------*
           move      w-cmp-rig-des-nrd    to   w-cmp-rig-des-ctr      .
       cmp-rig-des-320.
           add       1                    to   w-cmp-rig-des-ctr      .
           if        w-cmp-rig-des-ctr    >    w-cmp-rig-des-nri
                     go to cmp-rig-des-900.
           move      spaces               to   w-cmp-rig-des-rig
                                              (w-cmp-rig-des-ctr)     .
           go to     cmp-rig-des-320.
       cmp-rig-des-900.
      *              *-------------------------------------------------*
      *              * Determinazione del flag di rivisualizzazione    *
      *              *-------------------------------------------------*
           if        w-cmp-rig-des-nri    not  = w-cmp-rig-des-nrd
                     move  "#"            to   w-cmp-rig-des-flg      .
       cmp-rig-des-999.
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
      *    * Routine lettura tabella [zm1]                             *
      *    *-----------------------------------------------------------*
       let-arc-zm1-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zm1-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice classe a zero                    *
      *              *-------------------------------------------------*
           if        w-let-arc-zm1-cla    =    zero
                     go to let-arc-zm1-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLA    "         to   f-key                  .
           move      w-let-arc-zm1-cla    to   rf-zm1-cod-cla         .
           move      "pgm/dpm/fls/ioc/obj/iofzm1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zm1                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zm1-400.
       let-arc-zm1-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zm1-des-cla       to   w-let-arc-zm1-des      .
           move      rf-zm1-ult-sud       to   w-let-arc-zm1-sud      .
           move      rf-zm1-umi-def       to   w-let-arc-zm1-umi      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zm1-999.
       let-arc-zm1-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zm1-flg      .
           move      all   "."            to   w-let-arc-zm1-des      .
           go to     let-arc-zm1-600.
       let-arc-zm1-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zm1-des      .
       let-arc-zm1-600.
           move      zero                 to   w-let-arc-zm1-sud      .
           move      spaces               to   w-let-arc-zm1-umi      .
       let-arc-zm1-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zm2]                             *
      *    *-----------------------------------------------------------*
       let-arc-zm2-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zm2-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice gruppo a zero                    *
      *              *-------------------------------------------------*
           if        w-let-arc-zm2-gru    =    zero
                     go to let-arc-zm2-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODGRU    "         to   f-key                  .
           move      w-let-arc-zm2-cla    to   rf-zm2-cod-cla         .
           move      w-let-arc-zm2-gru    to   rf-zm2-cod-gru         .
           move      "pgm/dpm/fls/ioc/obj/iofzm2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zm2                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zm2-400.
       let-arc-zm2-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zm2-des-gru       to   w-let-arc-zm2-des      .
           move      rf-zm2-ult-sud       to   w-let-arc-zm2-sud      .
           move      rf-zm2-umi-def       to   w-let-arc-zm2-umi      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zm2-999.
       let-arc-zm2-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zm2-flg      .
           move      all   "."            to   w-let-arc-zm2-des      .
           go to     let-arc-zm2-600.
       let-arc-zm2-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zm2-des      .
       let-arc-zm2-600.
           move      zero                 to   w-let-arc-zm2-sud      .
           move      spaces               to   w-let-arc-zm2-umi      .
       let-arc-zm2-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zm3]                             *
      *    *-----------------------------------------------------------*
       let-arc-zm3-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zm3-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice sottogruppo a zero               *
      *              *-------------------------------------------------*
           if        w-let-arc-zm3-sgr    =    zero
                     go to let-arc-zm3-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSGR    "         to   f-key                  .
           move      w-let-arc-zm3-cla    to   rf-zm3-cod-cla         .
           move      w-let-arc-zm3-gru    to   rf-zm3-cod-gru         .
           move      w-let-arc-zm3-sgr    to   rf-zm3-cod-sgr         .
           move      "pgm/dpm/fls/ioc/obj/iofzm3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zm3                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zm3-400.
       let-arc-zm3-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zm3-des-sgr       to   w-let-arc-zm3-des      .
           move      rf-zm3-ult-sud       to   w-let-arc-zm3-sud      .
           move      rf-zm3-umi-def       to   w-let-arc-zm3-umi      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zm3-999.
       let-arc-zm3-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zm3-flg      .
           move      all   "."            to   w-let-arc-zm3-des      .
           go to     let-arc-zm3-600.
       let-arc-zm3-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zm3-des      .
       let-arc-zm3-600.
           move      zero                 to   w-let-arc-zm3-sud      .
           move      spaces               to   w-let-arc-zm3-umi      .
       let-arc-zm3-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zum]                             *
      *    *-----------------------------------------------------------*
       let-arc-zum-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zum-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice unita' di misura a spaces        *
      *              *-------------------------------------------------*
           if        w-let-arc-zum-cod    =    spaces
                     go to let-arc-zum-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODUMI    "         to   f-key                  .
           move      w-let-arc-zum-cod    to   rf-zum-cod-umi         .
           move      "pgm/dcp/fls/ioc/obj/iofzum"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zum                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zum-400.
       let-arc-zum-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zum-des-umi       to   w-let-arc-zum-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zum-999.
       let-arc-zum-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zum-flg      .
           move      all   "."            to   w-let-arc-zum-des      .
           go to     let-arc-zum-999.
       let-arc-zum-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zum-des      .
       let-arc-zum-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [ztv]                         *
      *    *-----------------------------------------------------------*
       let-arc-ztv-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-ztv-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice variante a spaces                *
      *              *-------------------------------------------------*
           if        w-let-arc-ztv-cod    =    spaces
                     go to let-arc-ztv-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "TIPVAR    "         to   f-key                  .
           move      w-let-arc-ztv-cod    to   rf-ztv-tip-var         .
           move      "pgm/dcp/fls/ioc/obj/iofztv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ztv                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-ztv-400.
       let-arc-ztv-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-ztv-des-var       to   w-let-arc-ztv-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-ztv-999.
       let-arc-ztv-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-ztv-flg      .
           move      all   "."            to   w-let-arc-ztv-des      .
           go to     let-arc-ztv-999.
       let-arc-ztv-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-ztv-des      .
       let-arc-ztv-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zms]                         *
      *    *-----------------------------------------------------------*
       let-arc-zms-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zms-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice statistico a zero                *
      *              *-------------------------------------------------*
           if        w-let-arc-zms-cod    =    zero
                     go to let-arc-zms-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLS    "         to   f-key                  .
           move      w-let-arc-zms-tip    to   rf-zms-tip-cls         .
           move      w-let-arc-zms-cod    to   rf-zms-cod-cls         .
           move      "pgm/dpm/fls/ioc/obj/iofzms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zms                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zms-400.
       let-arc-zms-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zms-des-cls       to   w-let-arc-zms-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zms-999.
       let-arc-zms-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zms-flg      .
           move      all   "."            to   w-let-arc-zms-des      .
           go to     let-arc-zms-999.
       let-arc-zms-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zms-des      .
       let-arc-zms-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zci]                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/larczci0.lts"                   .

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
           go to     let-arc-dcf-600.
       let-arc-dcf-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcf-rag      .
       let-arc-dcf-600.
           move      spaces               to   w-let-arc-dcf-vlt      .
           move      spaces               to   w-let-arc-dcf-lng      .
       let-arc-dcf-999.
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
      *    * Routine di lettura archivio [pdt]                         *
      *    *-----------------------------------------------------------*
       let-arc-pdt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-pdt-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice casa produttrice a zero          *
      *              *-------------------------------------------------*
           if        w-let-arc-pdt-cod    =    zero
                     go to let-arc-pdt-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODPDT"             to   f-key                  .
           move      w-let-arc-pdt-cod    to   rf-pdt-cod-pdt         .
           move      "pgm/dcf/fls/ioc/obj/iofpdt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdt                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-pdt-400.
       let-arc-pdt-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-pdt-rag-soc       to   w-let-arc-pdt-rag      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-pdt-999.
       let-arc-pdt-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-pdt-flg      .
           move      all   "."            to   w-let-arc-pdt-rag      .
           go to     let-arc-pdt-999.
       let-arc-pdt-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-pdt-rag      .
       let-arc-pdt-999.
           exit.

      *    *===========================================================*
      *    * Controllo codice iva                                      *
      *    *-----------------------------------------------------------*
       ctl-cod-iva-000.
      *              *-------------------------------------------------*
      *              * Controllo                                       *
      *              *-------------------------------------------------*
           move      "CT"                 to   d-imp-iva-tip-ope      .
           move      "pgm/cge/prg/obj/dimpiva0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-imp-iva              .
       ctl-cod-iva-999.
           exit.

      *    *===========================================================*
      *    * Controllo unicita' codice materia prima alfanumerico      *
      *    *-----------------------------------------------------------*
       ctl-uni-alf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-ctl-uni-alf-flg      .
      *              *-------------------------------------------------*
      *              * Contatore records letti con chiave alfanumerica *
      *              * pari a quella passata, ma con chiave numerica   *
      *              * diversa da quella passata : a zero              *
      *              *-------------------------------------------------*
           move      zero                 to   w-ctl-uni-alf-ctr      .
      *              *-------------------------------------------------*
      *              * Start su file [dpm]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "ALFMAP    "         to   f-key                  .
           move      w-ctl-uni-alf-alf    to   rf-dpm-alf-map         .
           move      zero                 to   rf-dpm-num-map         .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
      *              *-------------------------------------------------*
      *              * Se errata : a trattamento finale                *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ctl-uni-alf-600.
       ctl-uni-alf-100.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [dpm]                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
      *              *-------------------------------------------------*
      *              * Se 'at end' : a trattamento finale              *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ctl-uni-alf-600.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
           if        rf-dpm-alf-map       not  = w-ctl-uni-alf-alf
                     go to ctl-uni-alf-600.
      *              *-------------------------------------------------*
      *              * Se valore numerico letto diverso da quello pas- *
      *              * sato incremento il contatore                    *
      *              *-------------------------------------------------*
           if        rf-dpm-num-map       not  = w-ctl-uni-alf-num
                     add   1              to   w-ctl-uni-alf-ctr      .
      *              *-------------------------------------------------*
      *              * Riciclo in lettura                              *
      *              *-------------------------------------------------*
           go to     ctl-uni-alf-100.
       ctl-uni-alf-600.
      *              *-------------------------------------------------*
      *              * Trattamento finale                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se il contatore e' a zero : uscita Ok       *
      *                  *---------------------------------------------*
           if        w-ctl-uni-alf-ctr    =    zero
                     go to ctl-uni-alf-999.
      *                  *---------------------------------------------*
      *                  * Altrimenti : uscita per errore              *
      *                  *---------------------------------------------*
           move      "#"                  to   w-ctl-uni-alf-flg      .
       ctl-uni-alf-999.
           exit.

      *    *===========================================================*
      *    * Routine di attribuzione codice automatico progressivo     *
      *    *-----------------------------------------------------------*
       att-cod-aut-000.
      *              *-------------------------------------------------*
      *              * Lettura codice automatico per [dpm]             *
      *              *-------------------------------------------------*
           move      "Eg"                 to   s-ope                  .
           move      "dpm "               to   s-nam                  .
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
           move      "dpm "               to   s-nam                  .
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
           move      s-num                to   w-enc-dpm-val-pre      .
      *                  *---------------------------------------------*
      *                  * Incremento del valore                       *
      *                  *---------------------------------------------*
           move      w-enc-dpm-val-pre    to   w-enc-dpm-val-pos      .
           add       1                    to   w-enc-dpm-val-pos      .
       att-cod-aut-500.
      *                  *---------------------------------------------*
      *                  * Se l'incremento porta a zero si forza il    *
      *                  * valore a 1                                  *
      *                  *---------------------------------------------*
           if        w-enc-dpm-val-pos    =    zero
                     move  1              to   w-enc-dpm-val-pos      .
      *                  *---------------------------------------------*
      *                  * Controllo se esiste gia' un record con il   *
      *                  * codice pari al valore incrementato          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMMAP    "         to   f-key                  .
           move      w-enc-dpm-val-pos    to   rf-dpm-num-map         .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
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
           add       1                    to   w-enc-dpm-val-pos      .
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
           move      "dpm "               to   s-nam                  .
           move      w-enc-dpm-val-pos    to   s-num                  .
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
      *              * Lettura codice automatico per [dpm]             *
      *              *-------------------------------------------------*
           move      "Eg"                 to   s-ope                  .
           move      "dpm "               to   s-nam                  .
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
           if        s-num                =    w-enc-dpm-val-pos
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
           move      "dpm "               to   s-nam                  .
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
           move      "dpm "               to   s-nam                  .
           move      w-enc-dpm-val-pre    to   s-num                  .
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
      *    * Subroutines per accettazione codice materia prima         *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/acoddpm0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice classe          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/acmnzm10.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice gruppo          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/acmnzm20.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice sottogruppo     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/acmnzm30.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione unita' di misura           *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acodzum0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione tipo variante              *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acodztv0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice statistico 1    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/acmnzms1.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice statistico 2    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/acmnzms2.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice statistico 3    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/acmnzms3.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice fornitore com-  *
      *    * merciale                                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmndcf0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice casa produttr.  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmnpdt0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice valuta          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acodzvl0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice sottoconto      *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnpdc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice Iva             *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnzci0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del coefficiente di cambio *
      *    * valuta                                                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoecmb0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

