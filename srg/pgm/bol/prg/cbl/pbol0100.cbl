       Identification Division.
       Program-Id.                                 pbol0100           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    bol                 *
      *                                Settore:    tab                 *
      *                                   Fase:    bol010              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 25/01/92    *
      *                       Ultima revisione:    NdK del 31/12/23    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione archivio tipi documento di         *
      *                    trasporto                                   *
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
                     "bol"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "tab"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "bol010"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pbol0100"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "    TIPI MOVIMENTO PER BOLLETTAZIONE    "       .

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
      *            *---------------------------------------------------*
      *            * Da accettazione corpo                             *
      *            *---------------------------------------------------*
               10  w-cnt-tus-acc-cor      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Da accettazione riga corpo                        *
      *            *---------------------------------------------------*
               10  w-cnt-tus-acc-rig      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Da accettazione piede                             *
      *            *---------------------------------------------------*
               10  w-cnt-tus-acc-pie      pic  x(01)                  .
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
      *            *---------------------------------------------------*
      *            * Per tasto Do su riga corpo                        *
      *            *---------------------------------------------------*
               10  w-cnt-tdo-rig-flg      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su modalita' di funzionamento      *
      *        *-------------------------------------------------------*
           05  w-cnt-mfu.
      *            *---------------------------------------------------*
      *            * Tipo impostazione                                 *
      *            * - K : Impostazione campi chiave                   *
      *            * - T : Impostazione campi testata                  *
      *            * - C : Impostazione primo campo riga corpo         *
      *            * - R : Impostazione altri campi riga corpo         *
      *            * - P : Impostazione campi piede                    *
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
      *            * Impostazione corpo                                *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-cor      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Impostazione riga corpo                           *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-rig      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Impostazione piede                                *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-pie      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di inibizione manuale tasto Delt             *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-itd      pic  x(01)                  .
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
      *            *---------------------------------------------------*
      *            * Visualizzazione prompts corpo                     *
      *            *---------------------------------------------------*
               10  w-cnt-sts-pmt-cor      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Visualizzazione prompts riga accettazione corpo   *
      *            *---------------------------------------------------*
               10  w-cnt-sts-pmt-rig      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Visualizzazione prompts piede                     *
      *            *---------------------------------------------------*
               10  w-cnt-sts-pmt-pie      pic  x(01)                  .
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
      *            *---------------------------------------------------*
      *            * Visualizzazione dati corpo                        *
      *            *---------------------------------------------------*
               10  w-cnt-sts-vis-cor      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Visualizzazione dati riga di accettazione corpo   *
      *            *---------------------------------------------------*
               10  w-cnt-sts-vis-rig      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Visualizzazione dati piede                        *
      *            *---------------------------------------------------*
               10  w-cnt-sts-vis-pie      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area di controllo per trattamento corpo               *
      *        *-------------------------------------------------------*
           05  w-cnt-cor.
      *            *---------------------------------------------------*
      *            * Numero riga da accettare                          *
      *            *---------------------------------------------------*
               10  w-cnt-cor-nrg-dac      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Numero riga da visualizzare                       *
      *            *---------------------------------------------------*
               10  w-cnt-cor-nrg-dav      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Tipo visualizzazione riga                         *
      *            *---------------------------------------------------*
               10  w-cnt-cor-tvi-rig      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Numero pagina corpo da visualizzare               *
      *            *---------------------------------------------------*
               10  w-cnt-cor-nrp-dav      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Numero pagina corpo visualizzata                  *
      *            *---------------------------------------------------*
               10  w-cnt-cor-nrp-vis      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Tipo aggiornamento post-riga (+ o -)              *
      *            *---------------------------------------------------*
               10  w-cnt-cor-tip-agg      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area di controllo per variazioni                      *
      *        *-------------------------------------------------------*
           05  w-cnt-var.
      *            *---------------------------------------------------*
      *            * Flag di variazioni in testata che si ripercuotono *
      *            * su tutte le righe corpo                           *
      *            *---------------------------------------------------*
               10  w-cnt-var-tes-cor      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area di controllo per Slct da primo campo riga corpo  *
      *        *-------------------------------------------------------*
           05  w-cnt-slc.
      *            *---------------------------------------------------*
      *            * Numero riga di destinazione per Slct              *
      *            *---------------------------------------------------*
               10  w-cnt-slc-num-rig      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Rappresentazione alfa del numero riga             *
      *            *---------------------------------------------------*
               10  w-cnt-slc-rap-alf.
                   15  w-cnt-slc-rap-chr
                               occurs 05  pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Rappresentazione numerica del numero riga         *
      *            *---------------------------------------------------*
               10  w-cnt-slc-rap-num      pic  9(05)                  .
               10  w-cnt-slc-rap-n02 redefines
                   w-cnt-slc-rap-num.
                   15  w-cnt-slc-rap-cif
                               occurs 05  pic  x(01)                  .
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
               10  w-cnt-wrk-ctr-003      pic  9(05)                  .
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
      *        * [zbi]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfzbi"                          .
      *        *-------------------------------------------------------*
      *        * [zfi]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rfzfi"                          .
      *        *-------------------------------------------------------*
      *        * [zct]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfzct"                          .
      *        *-------------------------------------------------------*
      *        * [zmc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfzmc"                          .
      *        *-------------------------------------------------------*
      *        * [zmm]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfzmm"                          .
      *        *-------------------------------------------------------*
      *        * [zub]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfzub"                          .
      *        *-------------------------------------------------------*
      *        * [zoc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfzoc"                          .
      *        *-------------------------------------------------------*
      *        * [zsc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ods/fls/rec/rfzsc"                          .
      *        *-------------------------------------------------------*
      *        * [ada]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfada"                          .
      *        *-------------------------------------------------------*
      *        * [zls]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzls"                          .

      *    *===========================================================*
      *    * Work-area per bufferizzazione testata                     *
      *    *-----------------------------------------------------------*
       01  w-tes.
      *        *-------------------------------------------------------*
      *        * Valori chiave                                         *
      *        *-------------------------------------------------------*
           05  w-tes-val-key.
               10  w-tes-cod-tmb          pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
               10  w-tes-des-tmb          pic  x(30)                  .
               10  w-tes-des-key          pic  x(30)                  .
               10  w-tes-des-stp          pic  x(25)                  .
               10  w-tes-pwd-tmb          pic  x(08)                  .
               10  w-tes-int-ftr          pic  9(02)                  .
               10  w-tes-tmo-ftr          pic  x(05)                  .
               10  w-tes-tmo-ftr-des      pic  x(30)                  .
               10  w-tes-tmo-ftr-tdo      pic  9(02)                  .
               10  w-tes-tmo-ft2          pic  x(05)                  .
               10  w-tes-snx-acm          pic  x(01)                  .
               10  w-tes-def-tac          pic  9(02)                  .
               10  w-tes-def-ctr          pic  x(03)                  .
               10  w-tes-def-ctr-des      pic  x(20)                  .
               10  w-tes-cau-mag          pic  9(05)                  .
               10  w-tes-cau-mag-des      pic  x(30)                  .
               10  w-tes-cau-mag-ttc      pic  9(02)                  .
               10  w-tes-cau-mag-tpc      pic  x(01)                  .
               10  w-tes-cau-mag-cdc      pic  x(03)                  .
               10  w-tes-cau-mag-vac      pic  x(01)                  .
               10  w-tes-cau-mag-dfa      pic  x(01)                  .
               10  w-tes-cau-mag-vaa      pic  x(01)                  .
               10  w-tes-cau-mag-lsa      pic  x(04)                  .
               10  w-tes-cod-mic          pic  x(03)                  .
               10  w-tes-cod-mic-des      pic  x(20)                  .
               10  w-tes-cam-agg          pic  9(05)                  .
               10  w-tes-cam-agg-des      pic  x(30)                  .
               10  w-tes-cam-agg-dfa      pic  x(01)                  .
               10  w-tes-cam-agg-vaa      pic  x(01)                  .
               10  w-tes-cam-agg-lsa      pic  x(04)                  .
               10  w-tes-def-tar          pic  x(01)                  .
               10  w-tes-snv-tar          pic  x(01)                  .
               10  w-tes-lst-tar          pic  x(04)                  .
               10  w-tes-org-doc          pic  9(02)                  .
               10  w-tes-prv-doc          pic  9(02)                  .
               10  w-tes-sgl-num          pic  x(03)                  .
               10  w-tes-mov-afd          pic  9(02)                  .
               10  w-tes-def-tmf          pic  x(05)                  .
               10  w-tes-snx-prz          pic  9(02)                  .
               10  w-tes-snx-sco          pic  9(02)                  .
               10  w-tes-snx-imp          pic  9(02)                  .
               10  w-tes-snx-civ          pic  9(02)                  .
               10  w-tes-snx-ttd          pic  9(02)                  .
               10  w-tes-snx-dct          pic  9(02)                  .
               10  w-tes-des-dct.
                   15  w-tes-rig-dct occurs 04
                                          pic  x(60)                  .
               10  w-tes-num-cps          pic  9(02)                  .
               10  w-tes-def-tva          pic  9(02)                  .
               10  w-tes-snx-par          pic  x(01)                  .
               10  w-tes-vld-dpz          pic  x(01)                  .
               10  w-tes-snx-age          pic  x(01)                  .
               10  w-tes-snx-ndp          pic  x(01)                  .
               10  w-tes-snx-fop          pic  x(01)                  .
               10  w-tes-snx-lib          pic  x(01)                  .
               10  w-tes-cod-dsl          pic  x(07)                  .
               10  w-tes-def-tpr          pic  x(05)                  .
               10  w-tes-snx-nmm          pic  x(01)                  .
               10  w-tes-snx-ncv          pic  x(01)                  .
               10  w-tes-cod-lst          pic  x(03)                  .
               10  w-tes-cod-lst-des      pic  x(20)                  .
               10  w-tes-snx-nmc          pic  x(01)                  .
               10  w-tes-alx-gen.
                   15  filler occurs 36   pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per bufferizzazione riga corpo                  *
      *    *-----------------------------------------------------------*
       01  w-rig.
      *        *-------------------------------------------------------*
      *        * Area valori attuali e precedenti                      *
      *        *-------------------------------------------------------*
           05  w-rig-val-aep     occurs 2.
               10  w-rig-num-prg          pic  9(05)                  .
               10  w-rig-cod-dpz          pic  9(02)                  .
               10  w-rig-cod-dpz-des      pic  x(20)                  .
               10  w-rig-cod-dsl          pic  x(07)                  .
               10  w-rig-cod-dsl-des      pic  x(30)                  .
               10  w-rig-alx-dpz.
                   15  filler occurs 40   pic  x(01)                  .

      *    *===========================================================*
      *    * Area di comunicazione per gestione catena rig, con buffer *
      *    * dati in grado di ospitare l'area w-rig                    *
      *    *-----------------------------------------------------------*
       01  w-cat-rig.
           05  w-cat-rig-ope              pic  x(02)                  .
           05  w-cat-rig-exs              pic  x(01)                  .
           05  w-cat-rig-num              pic  9(05)                  .
           05  w-cat-rig-cur              pic  9(05)                  .
           05  w-cat-rig-prg              pic  9(05)                  .
           05  w-cat-rig-max              pic  9(05)                  .
           05  w-cat-rig-app              pic  x(01)                  .
           05  w-cat-rig-ins              pic  x(01)                  .
           05  w-cat-rig-new              pic  x(01)                  .
           05  w-cat-rig-lst              pic  x(01)                  .
           05  w-cat-rig-buf.
               10  filler occurs 200      pic  x(01)                  .

      *    *===========================================================*
      *    * Work per linea corpo a video                              *
      *    *-----------------------------------------------------------*
       01  w-lin.
      *        *-------------------------------------------------------*
      *        * Numero righe di corpo effettive visibili contempora-  *
      *        * neamente in una pagina di corpo nell'area di scroll   *
      *        *-------------------------------------------------------*
           05  w-lin-num-lin-vis          pic  9(02)       value 11   .
      *        *-------------------------------------------------------*
      *        * Numero linee di display impegnate per ogni riga corpo *
      *        * nell'area di scroll                                   *
      *        *-------------------------------------------------------*
           05  w-lin-num-lin-prc          pic  9(02)       value 01   .
      *        *-------------------------------------------------------*
      *        * Prima linea di display utilizzata per le righe corpo  *
      *        * nell'area di scroll                                   *
      *        *-------------------------------------------------------*
           05  w-lin-pri-lin-vid          pic  9(02)       value 08   .
      *        *-------------------------------------------------------*
      *        * Immagine di una riga di corpo su video                *
      *        *-------------------------------------------------------*
           05  w-lin-imm.
      *            *---------------------------------------------------*
      *            * Linee di display per ogni riga corpo in scroll    *
      *            *---------------------------------------------------*
               10  w-lin-imm-dsp.
                   15  w-lin-imm-dsp-lin occurs 01
                                          pic  x(80)                  .
      *            *---------------------------------------------------*
      *            * Riga corpo in scroll rappresentata linearmente    *
      *            *---------------------------------------------------*
               10  w-lin-imm-scr redefines
                   w-lin-imm-dsp.
      *                *-----------------------------------------------*
      *                * Numero linea                                  *
      *                *-----------------------------------------------*
                   15  w-lin-imm-num-lin  pic  x(03)                  .
      *                *-----------------------------------------------*
      *                * Altri dati visualizzati                       *
      *                *-----------------------------------------------*
                   15  filler             pic  x(03)                  .
                   15  w-lin-imm-cod-dpz  pic  x(02)                  .
                   15  filler             pic  x(01)                  .
                   15  w-lin-imm-des-dpz  pic  x(20)                  .
                   15  filler             pic  x(03)                  .
                   15  w-lin-imm-cod-dsl  pic  x(07)                  .
                   15  filler             pic  x(01)                  .
                   15  w-lin-imm-des-dsl  pic  x(30)                  .
                   15  filler             pic  x(10)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
           05  w-sav-int-ftr              pic  9(02)                  .
           05  w-sav-snx-acm              pic  x(01)                  .
           05  w-sav-cau-mag              pic  9(05)                  .
           05  w-sav-cam-agg              pic  9(05)                  .
           05  w-sav-def-tar              pic  x(01)                  .
           05  w-sav-mov-afd              pic  9(02)                  .
           05  w-sav-snx-dct              pic  9(02)                  .
           05  w-sav-num-cps              pic  9(02)                  .
           05  w-sav-cod-dpz              pic  9(02)                  .
           05  w-sav-vld-dpz              pic  x(01)                  .
           05  w-sav-cat-rig.
               10  filler  occurs 300     pic  x(01)                  .
           05  w-sav-rig.
               10  filler  occurs 300     pic  x(01)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Interessa la fatturazione                  *
      *        *-------------------------------------------------------*
           05  w-exp-int-ftr.
               10  w-exp-int-ftr-num      pic  9(02)       value 3    .
               10  w-exp-int-ftr-lun      pic  9(02)       value 40   .
               10  w-exp-int-ftr-tbl.
                   15  filler             pic  x(40) value
                     "Non interessa la fatturazione           "       .
                   15  filler             pic  x(40) value
                     "fatturazione Differita successiva       "       .
                   15  filler             pic  x(40) value
                     "il documento stesso e' anche una Fattura"       .
      *        *-------------------------------------------------------*
      *        * Work per : Movimento a fronte di                      *
      *        *-------------------------------------------------------*
           05  w-exp-mov-afd.
               10  w-exp-mov-afd-num      pic  9(02)       value 4    .
               10  w-exp-mov-afd-lun      pic  9(02)       value 20   .
               10  w-exp-mov-afd-tbl.
                   15  filler             pic  x(20) value
                            "Niente              "                    .
                   15  filler             pic  x(20) value
                            "Ordine cliente      "                    .
                   15  filler             pic  x(20) value
                            "ordine di Spedizione"                    .
                   15  filler             pic  x(20) value
                            "Bolla di consegna   "                    .
      *        *-------------------------------------------------------*
      *        * Work per : Validita' per le dipendenze                *
      *        *-------------------------------------------------------*
           05  w-exp-vld-dpz.
               10  w-exp-vld-dpz-num      pic  9(02)       value 4    .
               10  w-exp-vld-dpz-lun      pic  9(02)       value 40   .
               10  w-exp-vld-dpz-tbl.
                   15  filler             pic  x(40) value
                            "valido per Tutte le dipendenze          ".
                   15  filler             pic  x(40) value
                            "valido solo per la Sede                 ".
                   15  filler             pic  x(40) value
                            "valido solo per le Dipendenze           ".
                   15  filler             pic  x(40) value
                            "valido solo per le dipendenze Indicate  ".
      *        *-------------------------------------------------------*
      *        * Work per : Default tipo vendita                       *
      *        *-------------------------------------------------------*
           05  w-exp-def-tva.
               10  w-exp-def-tva-num      pic  9(02)       value 2    .
               10  w-exp-def-tva-lun      pic  9(02)       value 10   .
               10  w-exp-def-tva-tbl.
                   15  filler             pic  x(10) value
                            "Diretta   "                              .
                   15  filler             pic  x(10) value
                            "Indiretta "                              .
      *        *-------------------------------------------------------*
      *        * Work per : Si/No riferimenti all'Agente               *
      *        *-------------------------------------------------------*
           05  w-exp-snx-age.
               10  w-exp-snx-age-num      pic  9(02)       value 4    .
               10  w-exp-snx-age-lun      pic  9(02)       value 40   .
               10  w-exp-snx-age-tbl.
                   15  filler             pic  x(40) value
                            "Si, codice e nominativo                 ".
                   15  filler             pic  x(40) value
                            "No, nessun riferimento                  ".
                   15  filler             pic  x(40) value
                            "Si, ma solo il codice                   ".
                   15  filler             pic  x(40) value
                            "Si, ma solo il nominativo               ".
      *        *-------------------------------------------------------*
      *        * Work per : Si/No generico                             *
      *        *-------------------------------------------------------*
           05  w-exp-snx-gen.
               10  w-exp-snx-gen-num      pic  9(02)       value 2    .
               10  w-exp-snx-gen-lun      pic  9(02)       value 02   .
               10  w-exp-snx-gen-tbl.
                   15  filler             pic  x(02) value
                            "Si"                                      .
                   15  filler             pic  x(02) value
                            "No"                                      .
      *        *-------------------------------------------------------*
      *        * Work per : Default trasporto a cura                   *
      *        *-------------------------------------------------------*
           05  w-exp-def-tac.
               10  w-exp-def-tac-num      pic  9(02)       value 4    .
               10  w-exp-def-tac-lun      pic  9(02)       value 20   .
               10  w-exp-def-tac-tbl.
                   15  filler             pic  x(20) value
                            "                    "                    .
                   15  filler             pic  x(20) value
                            "Mittente            "                    .
                   15  filler             pic  x(20) value
                            "Destinatario        "                    .
                   15  filler             pic  x(20) value
                            "Vettore             "                    .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo archivio di default                   *
      *        *-------------------------------------------------------*
           05  w-exp-def-tar.
               10  w-exp-def-tar-num      pic  9(02)       value 6    .
               10  w-exp-def-tar-lun      pic  9(02)       value 35   .
               10  w-exp-def-tar-tbl.
                   15  filler             pic  x(35) value
                            "Nessun tipo archivio proposto      "     .
                   15  filler             pic  x(35) value
                            "Non c'e tipo archivio              "     .
                   15  filler             pic  x(35) value
                            "Archivio Clienti                   "     .
                   15  filler             pic  x(35) value
                            "Archivio Fornitori                 "     .
                   15  filler             pic  x(35) value
                            "Archivio Dipendenze                "     .
                   15  filler             pic  x(35) value
                            "Archivio Agenti                    "     .
      *        *-------------------------------------------------------*
      *        * Work per : Origine del documento                      *
      *        *-------------------------------------------------------*
           05  w-exp-org-doc.
               10  w-exp-org-doc-num      pic  9(02) value 02         .
               10  w-exp-org-doc-lun      pic  9(02) value 10         .
               10  w-exp-org-doc-tbl.
                   15  filler             pic  x(10) value
                            "Manuale   "                              .
                   15  filler             pic  x(10) value
                            "Automatica"                              .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zfi]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zfi.
               10  w-let-arc-zfi-flg      pic  x(01)                  .
               10  w-let-arc-zfi-cod      pic  x(05)                  .
               10  w-let-arc-zfi-des      pic  x(30)                  .
               10  w-let-arc-zfi-vld      pic  9(02)                  .
               10  w-let-arc-zfi-dpz      pic  9(02)                  .
               10  w-let-arc-zfi-tdo      pic  9(02)                  .
               10  w-let-arc-zfi-ord      pic  9(02)                  .
               10  w-let-arc-zfi-prd      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zct]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zct.
               10  w-let-arc-zct-flg      pic  x(01)                  .
               10  w-let-arc-zct-cod      pic  x(03)                  .
               10  w-let-arc-zct-des      pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zmc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zmc.
               10  w-let-arc-zmc-flg      pic  x(01)                  .
               10  w-let-arc-zmc-cod      pic  9(05)                  .
               10  w-let-arc-zmc-des      pic  x(30)                  .
               10  w-let-arc-zmc-trv      pic  x(01)                  .
               10  w-let-arc-zmc-mdm      pic  9(02)                  .
               10  w-let-arc-zmc-ttc      pic  9(02)                  .
               10  w-let-arc-zmc-tpc      pic  x(01)                  .
               10  w-let-arc-zmc-cdc      pic  x(03)                  .
               10  w-let-arc-zmc-vac      pic  x(01)                  .
               10  w-let-arc-zmc-dfa      pic  x(01)                  .
               10  w-let-arc-zmc-vaa      pic  x(01)                  .
               10  w-let-arc-zmc-lsa      pic  x(04)                  .
               10  w-let-arc-zmc-dfm      pic  x(01)                  .
               10  w-let-arc-zmc-vam      pic  x(01)                  .
               10  w-let-arc-zmc-lsm      pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zmm]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zmm.
               10  w-let-arc-zmm-flg      pic  x(01)                  .
               10  w-let-arc-zmm-cod      pic  x(03)                  .
               10  w-let-arc-zmm-des      pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zub]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zub.
               10  w-let-arc-zub-flg      pic  x(01)                  .
               10  w-let-arc-zub-dpz      pic  9(02)                  .
               10  w-let-arc-zub-cod      pic  x(07)                  .
               10  w-let-arc-zub-des      pic  x(30)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zoc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zoc.
               10  w-let-arc-zoc-flg      pic  x(01)                  .
               10  w-let-arc-zoc-cod      pic  x(05)                  .
               10  w-let-arc-zoc-des      pic  x(30)                  .
               10  w-let-arc-zoc-vld      pic  9(02)                  .
               10  w-let-arc-zoc-dpz      pic  9(02)                  .
               10  w-let-arc-zoc-ord      pic  9(02)                  .
               10  w-let-arc-zoc-prd      pic  9(02)                  .
               10  w-let-arc-zoc-sgl      pic  x(03)                  .
               10  w-let-arc-zoc-dtr      pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zsc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zsc.
               10  w-let-arc-zsc-flg      pic  x(01)                  .
               10  w-let-arc-zsc-cod      pic  x(05)                  .
               10  w-let-arc-zsc-des      pic  x(30)                  .
               10  w-let-arc-zsc-vld      pic  9(02)                  .
               10  w-let-arc-zsc-dpz      pic  9(02)                  .
               10  w-let-arc-zsc-maf      pic  9(02)                  .
               10  w-let-arc-zsc-dmf      pic  x(05)                  .
               10  w-let-arc-zsc-tar      pic  x(01)                  .
               10  w-let-arc-zsc-dtr      pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zls]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zls.
               10  w-let-arc-zls-flg      pic  x(01)                  .
               10  w-let-arc-zls-cod      pic  x(03)                  .
               10  w-let-arc-zls-des      pic  x(20)                  .
               10  w-let-arc-zls-vlt      pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [ada]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-ada.
               10  w-let-arc-ada-flg      pic  x(01)                  .
               10  w-let-arc-ada-cod      pic  9(02)                  .
               10  w-let-arc-ada-des      pic  x(20)                  .

      *    *===========================================================*
      *    * Work per subroutines di Ctl                               *
      *    *-----------------------------------------------------------*
       01  w-ctl.
      *        *-------------------------------------------------------*
      *        * Work per Ctl codice dipendenza                        *
      *        *-------------------------------------------------------*
           05  w-ctl-cod-dpz.
               10  w-ctl-cod-dpz-flg      pic  x(01)                  .
               10  w-ctl-cod-dpz-dpz      pic  9(02)                  .

      *    *===========================================================*
      *    * Work area generica                                        *
      *    *-----------------------------------------------------------*
       01  w-wrk.
      *        *-------------------------------------------------------*
      *        * Work per lista tipi archivio ammessi                  *
      *        *-------------------------------------------------------*
           05  w-wrk-lta.
               10  w-wrk-lta-lst-amm      pic  x(04) value "NCFD"     .
               10  w-wrk-lta-lst-tam.
                   15  w-wrk-lta-lst-eam occurs 04
                                          pic  x(01)                  .
               10  w-wrk-lta-lst-tar.
                   15  w-wrk-lta-lst-ele occurs 04
                                          pic  x(01)                  .
               10  w-wrk-lta-inx-001      pic  9(03)                  .
               10  w-wrk-lta-inx-002      pic  9(03)                  .

      *    *===========================================================*
      *    * Work per routine acc-cau-mag-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-acc-cau-mag.
           05  w-acc-cau-mag-ctr          pic  9(02)                  .

      *    *===========================================================*
      *    * Work per routine acc-cam-agg-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-acc-cam-agg.
           05  w-acc-cam-agg-ctr          pic  9(02)                  .

      *    *===========================================================*
      *    * Area di interfaccia per sottoprogramma         "pazi000d" *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/pazi000d.pgl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice tipo movimento per bol- *
      *    * lettazione                                                *
      *    *-----------------------------------------------------------*
           copy      "pgm/bol/prg/cpy/acdezbi0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice tipo movimento per la   *
      *    * fatturazione                                              *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/acdezfi0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione causale del trasporto          *
      *    *-----------------------------------------------------------*
           copy      "pgm/bol/prg/cpy/acdezct0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice causale di magazzino    *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/acmnzmc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice conto merce             *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/acdezmm0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice ubicazione              *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/acdezub0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice dipendenza dell'azienda *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/acoddpz0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione tipo movimento ordini clienti  *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/acdezoc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice tipo movimento spedi-   *
      *    * zioni clienti                                             *
      *    *-----------------------------------------------------------*
           copy      "pgm/ods/prg/cpy/acdezsc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice listino                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acodzls0.acl"                   .

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
      *              *  - se Impostazione corpo     : non abilitato    *
      *              *  - se Inserimento            : non abilitato    *
      *              *  - se Visualizzazione        : abilitato        *
      *              *  - se Almeno una modifica    : non abilitato    *
      *              *  - altrimenti                : abilitato        *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    "K"   or
                     w-cnt-mfu-tip-imp    =    "C"   or
                     w-cnt-mfu-tip-fun    =    "I"   or
                     w-cnt-acc-flg-aum    not  = spaces
                     go to exe-acc-cmp-080.
           move      "[4] "               to   v-pfk (17)             .
       exe-acc-cmp-080.
      *              *-------------------------------------------------*
      *              * Tasto di funzione Delt :                        *
      *              *  - se Impostazione chiave    : non abilitato    *
      *              *  - se Altri campi corpo      : non abilitato    *
      *              *  - se Inserimento            : non abilitato    *
      *              *  - se Visualizzazione        : non abilitato    *
      *              *  - se Disabilitato manualm.  : non abilitato    *
      *              *  - se Almeno una modifica    : non abilitato    *
      *              *  - altrimenti                : abilitato        *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    "K"      or
                     w-cnt-mfu-tip-imp    =    "R"      or
                     w-cnt-mfu-tip-fun    =    "I"      or
                     w-cnt-mfu-tip-fun    =    "V"      or
                     w-cnt-sts-imp-itd    not  = spaces or
                     w-cnt-acc-flg-aum    not  = spaces
                     go to exe-acc-cmp-100.
           move      "DELT"               to   v-pfk (19)             .
       exe-acc-cmp-100.
      *              *-------------------------------------------------*
      *              * Tasto di funzione Do :                          *
      *              *  - se Visualizzazione     : disabilitato        *
      *              *  - altrimenti             : inalterato          *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "V"
                     move  spaces         to   v-pfk (05)             .
      *              *-------------------------------------------------*
      *              * Tasti di funzione Insr e Remv :                 *
      *              *  - se Visualizzazione e in                      *
      *              *    primo campo riga corpo : disabilitati        *
      *              *  - altrimenti             : inalterati          *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "V" and
                     w-cnt-mfu-tip-imp    =    "C"
                     move  spaces         to   v-pfk (04)
                     move  spaces         to   v-pfk (06)             .
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
                     go to pre-exe-pgm-300.
           move      "EN"                 to   w-dpz-tip-ope          .
           call      "pgm/azi/prg/obj/pazi000d"
                                         using w-dpz                  .
           cancel    "pgm/azi/prg/obj/pazi000d"                       .
           move      "#"                  to   w-cnt-pre-exe-pgm      .
           go to     pre-exe-pgm-999.
       pre-exe-pgm-300.
      *              *-------------------------------------------------*
      *              * Open modulo accettazione tipo movimento per la  *
      *              * bolla                                           *
      *              *-------------------------------------------------*
           perform   cod-des-zbi-opn-000  thru cod-des-zbi-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione tipo movimento per la  *
      *              * fatturazione                                    *
      *              *-------------------------------------------------*
           perform   cod-des-zfi-opn-000  thru cod-des-zfi-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione causale del trasporto  *
      *              *-------------------------------------------------*
           perform   cod-des-zct-opn-000  thru cod-des-zct-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice causale di ge-  *
      *              * stione magazzino                                *
      *              *-------------------------------------------------*
           perform   cod-mne-zmc-opn-000  thru cod-mne-zmc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice conto merce     *
      *              *-------------------------------------------------*
           perform   cod-des-zmm-opn-000  thru cod-des-zmm-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice ubicazione      *
      *              *-------------------------------------------------*
           perform   cod-des-zub-opn-000  thru cod-des-zub-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice dipendenza del- *
      *              * l'azienda                                       *
      *              *-------------------------------------------------*
           perform   cod-cod-dpz-opn-000  thru cod-cod-dpz-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione tipo movimento per gli *
      *              * ordini clienti                                  *
      *              *-------------------------------------------------*
           perform   cod-des-zoc-opn-000  thru cod-des-zoc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione tipo movimento per     *
      *              * spedizioni clienti                              *
      *              *-------------------------------------------------*
           perform   cod-des-zsc-opn-000  thru cod-des-zsc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice listino         *
      *              *-------------------------------------------------*
           perform   cod-cod-zls-opn-000  thru cod-cod-zls-opn-999    .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Operazioni post-esecuzione                      *
      *              *-------------------------------------------------*
      *              *-------------------------------------------------*
      *              * Close modulo accettazione tipo movimento per la *
      *              * bollettazione                                   *
      *              *-------------------------------------------------*
           perform   cod-des-zbi-cls-000  thru cod-des-zbi-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione tipo movimento per la *
      *              * fatturazione                                    *
      *              *-------------------------------------------------*
           perform   cod-des-zfi-cls-000  thru cod-des-zfi-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione causale del trasporto *
      *              *-------------------------------------------------*
           perform   cod-des-zct-cls-000  thru cod-des-zct-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice causale di ge- *
      *              * stione magazzino                                *
      *              *-------------------------------------------------*
           perform   cod-mne-zmc-cls-000  thru cod-mne-zmc-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice conto merce    *
      *              *-------------------------------------------------*
           perform   cod-des-zmm-cls-000  thru cod-des-zmm-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice ubicazione     *
      *              *-------------------------------------------------*
           perform   cod-des-zub-cls-000  thru cod-des-zub-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice dipendenza del-*
      *              * l'azienda                                       *
      *              *-------------------------------------------------*
           perform   cod-cod-dpz-cls-000  thru cod-cod-dpz-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione tipo movimento per    *
      *              * ordini clienti                                  *
      *              *-------------------------------------------------*
           perform   cod-des-zoc-cls-000  thru cod-des-zoc-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione tipo movimento per la *
      *              * bollettazione                                   *
      *              *-------------------------------------------------*
           perform   cod-des-zsc-cls-000  thru cod-des-zsc-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice listino        *
      *              *-------------------------------------------------*
           perform   cod-cod-zls-cls-000  thru cod-cod-zls-cls-999    .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * Open sottoprogramma gestione catena righe       *
      *              *-------------------------------------------------*
           move      "OP"                 to   w-cat-rig-ope          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
      *              *-------------------------------------------------*
      *              * [zbi]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofzbi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbi                 .
      *              *-------------------------------------------------*
      *              * [zfi]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/iofzfi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfi                 .
      *              *-------------------------------------------------*
      *              * [zct]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofzct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zct                 .
      *              *-------------------------------------------------*
      *              * [zmc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzmc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zmc                 .
      *              *-------------------------------------------------*
      *              * [zmm]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzmm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zmm                 .
      *              *-------------------------------------------------*
      *              * [zub]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzub"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zub                 .
      *              *-------------------------------------------------*
      *              * [zoc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofzoc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zoc                 .
      *              *-------------------------------------------------*
      *              * [zsc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofzsc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsc                 .
      *              *-------------------------------------------------*
      *              * [zls]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzls"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zls                 .
      *              *-------------------------------------------------*
      *              * [ada]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * Close sottoprogramma gestione catena righe      *
      *              *-------------------------------------------------*
           move      "CL"                 to   w-cat-rig-ope          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
      *              *-------------------------------------------------*
      *              * Cancellazione dello stesso sottoprogramma       *
      *              *-------------------------------------------------*
           perform   cnc-sub-cat-000      thru cnc-sub-cat-999        .
      *              *-------------------------------------------------*
      *              * [zbi]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofzbi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbi                 .
      *              *-------------------------------------------------*
      *              * [zfi]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/iofzfi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfi                 .
      *              *-------------------------------------------------*
      *              * [zct]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofzct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zct                 .
      *              *-------------------------------------------------*
      *              * [zmc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzmc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zmc                 .
      *              *-------------------------------------------------*
      *              * [zmm]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzmm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zmm                 .
      *              *-------------------------------------------------*
      *              * [zub]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzub"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zub                 .
      *              *-------------------------------------------------*
      *              * [zoc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofzoc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zoc                 .
      *              *-------------------------------------------------*
      *              * [zsc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofzsc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsc                 .
      *              *-------------------------------------------------*
      *              * [zls]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzls"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zls                 .
      *              *-------------------------------------------------*
      *              * [ada]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
       rou-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per gestione catena righe         *
      *    *-----------------------------------------------------------*
       cll-sub-cat-000.
           call      "pgm/bol/prg/obj/pbol0102"
                                         using w-cat-rig              .
       cll-sub-cat-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione sottoprogramma per gestione catena righe    *
      *    *-----------------------------------------------------------*
       cnc-sub-cat-000.
           cancel    "pgm/bol/prg/obj/pbol0102"                       .
       cnc-sub-cat-999.
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
                                               w-cnt-sts-ing-pte
                                               w-cnt-sts-imp-cor
                                               w-cnt-sts-imp-rig
                                               w-cnt-sts-imp-pie
                                               w-cnt-sts-imp-itd      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status visualizzazione prompts  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-pmt-key
                                               w-cnt-sts-pmt-tes
                                               w-cnt-sts-pmt-pte
                                               w-cnt-sts-pmt-cor
                                               w-cnt-sts-pmt-rig
                                               w-cnt-sts-pmt-pie      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status visualizzazione dati     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-vis-key
                                               w-cnt-sts-vis-tes
                                               w-cnt-sts-vis-pte
                                               w-cnt-sts-vis-cor
                                               w-cnt-sts-vis-rig
                                               w-cnt-sts-vis-pie      .
      *              *-------------------------------------------------*
      *              * Normalizzazione area di controllo variazioni    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-var-tes-cor      .
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
      *                  * Codice tipo movimento                       *
      *                  *---------------------------------------------*
           perform   acc-cod-tmb-000      thru acc-cod-tmb-999        .
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
      *              * Codice tipo movimento                           *
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
      *              * Erase linee impegnate                           *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      05                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Codice tipo movimento                           *
      *              *-------------------------------------------------*
           perform   pmt-cod-tmb-000      thru pmt-cod-tmb-999        .
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
      *    * Visualizzazione prompts per Codice tipo movimento         *
      *    *-----------------------------------------------------------*
       pmt-cod-tmb-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice tipo movimento      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-tmb-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Codice tipo movimento         *
      *    *-----------------------------------------------------------*
       acc-cod-tmb-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-tmb-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-zbi-ope      .
           move      w-tes-cod-tmb        to   w-cod-des-zbi-cod      .
           move      04                   to   w-cod-des-zbi-lin      .
           move      30                   to   w-cod-des-zbi-pos      .
           move      06                   to   w-cod-des-zbi-dln      .
           move      30                   to   w-cod-des-zbi-dps      .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-des-zbi-cll-000  thru cod-des-zbi-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-zbi-foi-000  thru cod-des-zbi-foi-999    .
       acc-cod-tmb-110.
           perform   cod-des-zbi-cll-000  thru cod-des-zbi-cll-999    .
           if        w-cod-des-zbi-ope    =    "F+"
                     go to acc-cod-tmb-115.
           if        w-cod-des-zbi-ope    =    "AC"
                     go to acc-cod-tmb-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-tmb-115.
           perform   cod-des-zbi-foi-000  thru cod-des-zbi-foi-999    .
           go to     acc-cod-tmb-110.
       acc-cod-tmb-120.
           move      w-cod-des-zbi-cod    to   v-alf                  .
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
      *    * Visualizzazione campo chiave : Codice tipo movimento      *
      *    *-----------------------------------------------------------*
       vis-cod-tmb-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-tmb        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-tmb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Descrizione tipo movimento *
      *    *-----------------------------------------------------------*
       vis-cod-tmb-des-000.
      *              *-------------------------------------------------*
      *              * Test se da visualizzare                         *
      *              *-------------------------------------------------*
           if        w-cnt-sts-imp-npt    not  > 1
                     go to vis-cod-tmb-des-999.
       vis-cod-tmb-des-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-tes-des-tmb (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
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
      *              * fica o visualizzazione, per testata e corpo     *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "M" or
                     w-cnt-mfu-tip-fun    =    "V"
                     move  "#"            to   w-cnt-sts-imp-tes
                     move  all "#"        to   w-cnt-sts-imp-pte
                     move  all "#"        to   w-cnt-sts-ing-pte
                     move  "#"            to   w-cnt-sts-imp-cor      .
      *              *-------------------------------------------------*
      *              * Assestamento status di impostazione per piede   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-pie      .
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
      *                  * Assestamento flags di controllo visualizza- *
      *                  * zione prompts e dati corpo                  *
      *                  *---------------------------------------------*
           move      spaces               to   w-cnt-sts-pmt-cor      
                                               w-cnt-sts-vis-cor      .
      *                  *---------------------------------------------*
      *                  * Assestamento flags di controllo visualizza- *
      *                  * zione prompts e dati riga corpo             *
      *                  *---------------------------------------------*
           move      spaces               to   w-cnt-sts-pmt-rig      
                                               w-cnt-sts-vis-rig      .
      *                  *---------------------------------------------*
      *                  * Assestamento flags di controllo visualizza- *
      *                  * zione prompts e dati piede                  *
      *                  *---------------------------------------------*
           move      spaces               to   w-cnt-sts-pmt-pie      
                                               w-cnt-sts-vis-pie      .
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
                     go to acc-nok-reg-225
           else      go to acc-nok-reg-250.
       acc-nok-reg-225.
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
       acc-nok-reg-230.
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
                     go to acc-nok-reg-230.
      *                          *-------------------------------------*
      *                          * Riciclo ad impostazione testata     *
      *                          *-------------------------------------*
           go to     acc-nok-reg-200.
       acc-nok-reg-250.
      *                      *-----------------------------------------*
      *                      * Se spostamento a pagina successiva      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Salvataggio numero pagina attuale   *
      *                          *-------------------------------------*
           move      w-cnt-sts-imp-npt    to   w-cnt-sts-imp-svp      .
       acc-nok-reg-260.
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
      *                          * si va' ad accettazione corpo        *
      *                          *-------------------------------------*
           if        w-cnt-sts-imp-npt    not  < w-cnt-sts-imp-mpt
                     move  w-cnt-sts-imp-svp
                                          to   w-cnt-sts-imp-npt
                     go to acc-nok-reg-300.
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
                     go to acc-nok-reg-260.
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
       acc-nok-reg-300.
      *                  *---------------------------------------------*
      *                  * Numero riga corpo da accettare : 1          *
      *                  *---------------------------------------------*
           move       1                   to   w-cnt-cor-nrg-dac      .
       acc-nok-reg-400.
      *              *-------------------------------------------------*
      *              * Trattamento corpo                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Assestamento flags di controllo visualizza- *
      *                  * zione prompts e dati chiave                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-cnt-sts-pmt-key      
                                               w-cnt-sts-vis-key      .
      *                  *---------------------------------------------*
      *                  * Assestamento flags di controllo visualizza- *
      *                  * zione prompts e dati testata                *
      *                  *---------------------------------------------*
           move      spaces               to   w-cnt-sts-pmt-tes
                                               w-cnt-sts-pmt-pte
                                               w-cnt-sts-vis-tes
                                               w-cnt-sts-vis-pte      .
      *                  *---------------------------------------------*
      *                  * Assestamento flags di controllo visualizza- *
      *                  * zione prompts e dati piede                  *
      *                  *---------------------------------------------*
           move      spaces               to   w-cnt-sts-pmt-pie      
                                               w-cnt-sts-vis-pie      .
      *                  *---------------------------------------------*
      *                  * Accettazione dati corpo                     *
      *                  *---------------------------------------------*
           perform   acc-cor-reg-000      thru acc-cor-reg-999        .
      *                  *---------------------------------------------*
      *                  * Se tipo uscita definitivo                   *
      *                  *---------------------------------------------*
           if        w-cnt-tus-acc-cor    =    "S" or
                     w-cnt-tus-acc-cor    =    "X" or
                     w-cnt-tus-acc-cor    =    "E"
                     move  w-cnt-tus-acc-cor
                                          to   w-cnt-tus-acc-nok
                     go to acc-nok-reg-999.
      *                  *---------------------------------------------*
      *                  * Se tipo uscita "-"                          *
      *                  *---------------------------------------------*
           if        w-cnt-tus-acc-cor    =    "-"
                     go to acc-nok-reg-200.
       acc-nok-reg-800.
      *              *-------------------------------------------------*
      *              * Conferma impostazioni                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Se attualmente visualizzato il corpo si e-  *
      *                  * segue la visualizzazione dei prompts della  *
      *                  * riga corpo espansa                          *
      *                  *---------------------------------------------*
           if        w-cnt-sts-pmt-cor    =    spaces
                     go to acc-nok-reg-805.
           perform   pmt-rig-cor-000      thru pmt-rig-cor-999        .
       acc-nok-reg-805.
      *                  *---------------------------------------------*
      *                  * Cancellazione eventuali note operative      *
      *                  *---------------------------------------------*
           move      "NT"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
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
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "Conferma impostazioni (S/N/E) ?"
                                          to   v-not
           else      move  "Conferma impostazioni (N/E) ?"
                                          to   v-not                  .
           move      spaces               to   v-alf                  .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "SNE"          to   v-msk
           else      move  "NE"           to   v-msk                  .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
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
           move      "S"                  to   w-cnt-tus-acc-nok      .
           perform   cnt-tdo-nok-000      thru cnt-tdo-nok-999        .
           if        w-cnt-tdo-nok-flg    not  = spaces
                     go to acc-nok-reg-870.
           go to     acc-nok-reg-999.
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
      *                      * Nr riga corpo da accettare : max + 1    *
      *                      *-----------------------------------------*
           move      w-cat-rig-max        to   w-cnt-cor-nrg-dac      .
           if        w-cat-rig-app        =    spaces
                     add   1              to   w-cnt-cor-nrg-dac      .
      *                      *-----------------------------------------*
      *                      * Ad accettazione corpo                   *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
           go to     acc-nok-reg-400.
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
      *              * Test per la pagina 1                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     snp-tes-reg-999.
       snp-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Test per la pagina 2                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     snp-tes-reg-999.
       snp-tes-reg-300.
      *              *-------------------------------------------------*
      *              * Test per la pagina 3                            *
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
      *                  * Descrizione tipo movimento                  *
      *                  *---------------------------------------------*
           perform   acc-des-tmb-000      thru acc-des-tmb-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-105.
      *                  *---------------------------------------------*
      *                  * Password per il movimento                   *
      *                  *---------------------------------------------*
           perform   acc-pwd-tmb-000      thru acc-pwd-tmb-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-100.
       acc-tes-reg-110.
      *                  *---------------------------------------------*
      *                  * Descrizione per la stampa                   *
      *                  *---------------------------------------------*
           perform   acc-des-stp-000      thru acc-des-stp-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-105.
       acc-tes-reg-115.
      *                  *---------------------------------------------*
      *                  * Interessa la fatturazione                   *
      *                  *---------------------------------------------*
           perform   acc-int-ftr-000      thru acc-int-ftr-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-110.
       acc-tes-reg-120.
      *                  *---------------------------------------------*
      *                  * Tipo movimento per fatturazione             *
      *                  *---------------------------------------------*
           perform   acc-tmo-ftr-000      thru acc-tmo-ftr-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-115.
       acc-tes-reg-125.
      *                  *---------------------------------------------*
      *                  * Si/No accompagnamento merce                 *
      *                  *---------------------------------------------*
           perform   acc-snx-acm-000      thru acc-snx-acm-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-120.
       acc-tes-reg-130.
      *                  *---------------------------------------------*
      *                  * Default trasporto a cura                    *
      *                  *---------------------------------------------*
           perform   acc-def-tac-000      thru acc-def-tac-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-125.
       acc-tes-reg-135.
      *                  *---------------------------------------------*
      *                  * Default causale trasporto                   *
      *                  *---------------------------------------------*
           perform   acc-def-ctr-000      thru acc-def-ctr-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-130.
       acc-tes-reg-140.
      *                  *---------------------------------------------*
      *                  * Causale magazzino                           *
      *                  *---------------------------------------------*
           perform   acc-cau-mag-000      thru acc-cau-mag-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-135.
       acc-tes-reg-145.
      *                  *---------------------------------------------*
      *                  * Codice c/merce                              *
      *                  *---------------------------------------------*
           perform   acc-cod-mic-000      thru acc-cod-mic-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-140.
       acc-tes-reg-150.
      *                  *---------------------------------------------*
      *                  * Causale di magazzino aggiuntiva             *
      *                  *---------------------------------------------*
           perform   acc-cam-agg-000      thru acc-cam-agg-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-145.
       acc-tes-reg-155.
      *                  *---------------------------------------------*
      *                  * Default tipo archivio                       *
      *                  *---------------------------------------------*
           perform   acc-def-tar-000      thru acc-def-tar-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-150.
       acc-tes-reg-160.
      *                  *---------------------------------------------*
      *                  * Tipo archivio modificabile                  *
      *                  *---------------------------------------------*
           perform   acc-snv-tar-000      thru acc-snv-tar-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-155.
       acc-tes-reg-165.
      *                  *---------------------------------------------*
      *                  * Tipi archivio ammessi                       *
      *                  *---------------------------------------------*
           perform   acc-lst-tar-000      thru acc-lst-tar-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-160.
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
                     go to acc-tes-reg-165.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
           go to     acc-tes-reg-999.
       acc-tes-reg-200.
      *                  *---------------------------------------------*
      *                  * Origine del documento                       *
      *                  *---------------------------------------------*
           perform   acc-org-doc-000      thru acc-org-doc-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     move  "-"            to   w-cnt-tus-acc-tes
                     go to acc-tes-reg-999.
       acc-tes-reg-205.
      *                  *---------------------------------------------*
      *                  * Sigla numerazione                           *
      *                  *---------------------------------------------*
           perform   acc-sgl-num-000      thru acc-sgl-num-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-200.
       acc-tes-reg-210.
      *                  *---------------------------------------------*
      *                  * Movimento a fronte di                       *
      *                  *---------------------------------------------*
           perform   acc-mov-afd-000      thru acc-mov-afd-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-205.
       acc-tes-reg-215.
      *                  *---------------------------------------------*
      *                  * Default tipo movimento a fronte             *
      *                  *---------------------------------------------*
           perform   acc-def-tmf-000      thru acc-def-tmf-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-210.
       acc-tes-reg-220.
      *                  *---------------------------------------------*
      *                  * Validita' per le dipendenze                 *
      *                  *---------------------------------------------*
           perform   acc-vld-dpz-000      thru acc-vld-dpz-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-215.
       acc-tes-reg-225.
      *                  *---------------------------------------------*
      *                  * Default per accettazione tipo riga corpo    *
      *                  *---------------------------------------------*
           perform   acc-def-tpr-000      thru acc-def-tpr-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-220.
       acc-tes-reg-230.
      *                  *---------------------------------------------*
      *                  * Default per tipo vendita                    *
      *                  *---------------------------------------------*
           perform   acc-def-tva-000      thru acc-def-tva-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-225.
       acc-tes-reg-235.
      *                  *---------------------------------------------*
      *                  * Si/no default data inizio trasporto         *
      *                  *---------------------------------------------*
           perform   acc-snx-ddi-000      thru acc-snx-ddi-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-230.
       acc-tes-reg-240.
      *                  *---------------------------------------------*
      *                  * Codice listino da proporre                  *
      *                  *---------------------------------------------*
           perform   acc-cod-lst-000      thru acc-cod-lst-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-235.
       acc-tes-reg-245.
      *                  *---------------------------------------------*
      *                  * Si/no numerazione modificabile              *
      *                  *---------------------------------------------*
           perform   acc-snx-nmm-000      thru acc-snx-nmm-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-240.
       acc-tes-reg-250.
      *                  *---------------------------------------------*
      *                  * Si/no numerazione controllata               *
      *                  *---------------------------------------------*
           perform   acc-snx-nmc-000      thru acc-snx-nmc-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-245.
       acc-tes-reg-280.
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
                     go to acc-tes-reg-250.
       acc-tes-reg-290.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-tes-reg-999.
       acc-tes-reg-300.
      *                  *---------------------------------------------*
      *                  * Numero copie da stampare                    *
      *                  *---------------------------------------------*
           perform   acc-num-cps-000      thru acc-num-cps-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     move  "-"            to   w-cnt-tus-acc-tes
                     go to acc-tes-reg-999.
       acc-tes-reg-305.
      *                  *---------------------------------------------*
      *                  * Si/no controllo di stampa gia' effettuata   *
      *                  *---------------------------------------------*
           perform   acc-snx-lib-000      thru acc-snx-lib-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-300.
       acc-tes-reg-310.
      *                  *---------------------------------------------*
      *                  * Si/no numero copie variabile                *
      *                  *---------------------------------------------*
           perform   acc-snx-ncv-000      thru acc-snx-ncv-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-305.
       acc-tes-reg-315.
      *                  *---------------------------------------------*
      *                  * Si/no stampa prezzo                         *
      *                  *---------------------------------------------*
           perform   acc-snx-prz-000      thru acc-snx-prz-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-310.
       acc-tes-reg-320.
      *                  *---------------------------------------------*
      *                  * Si/no stampa sconti                         *
      *                  *---------------------------------------------*
           perform   acc-snx-sco-000      thru acc-snx-sco-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-315.
       acc-tes-reg-325.
      *                  *---------------------------------------------*
      *                  * Si/no stampa importo                        *
      *                  *---------------------------------------------*
           perform   acc-snx-imp-000      thru acc-snx-imp-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-320.
       acc-tes-reg-330.
      *                  *---------------------------------------------*
      *                  * Si/no stampa codice iva                     *
      *                  *---------------------------------------------*
           perform   acc-snx-civ-000      thru acc-snx-civ-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-325.
       acc-tes-reg-335.
      *                  *---------------------------------------------*
      *                  * Si/no stampa totale documento               *
      *                  *---------------------------------------------*
           perform   acc-snx-ttd-000      thru acc-snx-ttd-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-330.
       acc-tes-reg-340.
      *                  *---------------------------------------------*
      *                  * Si/no stampa dicitura                       *
      *                  *---------------------------------------------*
           perform   acc-snx-dct-000      thru acc-snx-dct-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-335.
       acc-tes-reg-345.
      *                  *---------------------------------------------*
      *                  * Descrizione dicitura                        *
      *                  *---------------------------------------------*
           perform   acc-des-dct-000      thru acc-des-dct-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-340.
       acc-tes-reg-350.
      *                  *---------------------------------------------*
      *                  * Si/no stampa Agente                         *
      *                  *---------------------------------------------*
           perform   acc-snx-age-000      thru acc-snx-age-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-345.
       acc-tes-reg-355.
      *                  *---------------------------------------------*
      *                  * Si/no stampa ns. dipendenza                 *
      *                  *---------------------------------------------*
           perform   acc-snx-ndp-000      thru acc-snx-ndp-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-350.
       acc-tes-reg-360.
      *                  *---------------------------------------------*
      *                  * Si/No stampa forma di pagamento             *
      *                  *---------------------------------------------*
           perform   acc-snx-fop-000      thru acc-snx-fop-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-355.
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
                     go to acc-tes-reg-360.
       acc-tes-reg-390.
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
                     depending            on   w-cnt-sts-imp-npt      .
           go to     vis-tes-reg-999.
       vis-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Descrizione tipo movimento                      *
      *              *-------------------------------------------------*
           perform   vis-des-tmb-000      thru vis-des-tmb-999        .
      *              *-------------------------------------------------*
      *              * Password per il movimento                       *
      *              *-------------------------------------------------*
           perform   vis-pwd-tmb-000      thru vis-pwd-tmb-999        .
      *              *-------------------------------------------------*
      *              * Descrizione per la stampa                       *
      *              *-------------------------------------------------*
           perform   vis-des-stp-000      thru vis-des-stp-999        .
      *              *-------------------------------------------------*
      *              * Interessa la fatturazione                       *
      *              *-------------------------------------------------*
           perform   vis-int-ftr-000      thru vis-int-ftr-999        .
      *              *-------------------------------------------------*
      *              * Tipo movimento per fatturazione                 *
      *              *-------------------------------------------------*
           perform   vis-tmo-ftr-000      thru vis-tmo-ftr-999        .
           perform   vis-tmo-ftr-des-000  thru vis-tmo-ftr-des-999    .
      *              *-------------------------------------------------*
      *              * Si/No accompagnamento merce                     *
      *              *-------------------------------------------------*
           perform   vis-snx-acm-000      thru vis-snx-acm-999        .
      *              *-------------------------------------------------*
      *              * Default trasporto a cura                        *
      *              *-------------------------------------------------*
           perform   vis-def-tac-000      thru vis-def-tac-999        .
      *              *-------------------------------------------------*
      *              * Default causale trasporto                       *
      *              *-------------------------------------------------*
           perform   vis-def-ctr-000      thru vis-def-ctr-999        .
           perform   vis-def-ctr-des-000  thru vis-def-ctr-des-999    .
      *              *-------------------------------------------------*
      *              * Causale magazzino                               *
      *              *-------------------------------------------------*
           perform   vis-cau-mag-000      thru vis-cau-mag-999        .
           perform   vis-cau-mag-des-000  thru vis-cau-mag-des-999    .
      *              *-------------------------------------------------*
      *              * Codice c/merce                                  *
      *              *-------------------------------------------------*
           perform   vis-cod-mic-000      thru vis-cod-mic-999        .
           perform   vis-cod-mic-des-000  thru vis-cod-mic-des-999    .
      *              *-------------------------------------------------*
      *              * Causale magazzino aggiuntiva                    *
      *              *-------------------------------------------------*
           perform   vis-cam-agg-000      thru vis-cam-agg-999        .
           perform   vis-cam-agg-des-000  thru vis-cam-agg-des-999    .
      *              *-------------------------------------------------*
      *              * Default tipo archivio                           *
      *              *-------------------------------------------------*
           perform   vis-def-tar-000      thru vis-def-tar-999        .
      *              *-------------------------------------------------*
      *              * Tipo archivio modificabile                      *
      *              *-------------------------------------------------*
           perform   vis-snv-tar-000      thru vis-snv-tar-999        .
      *              *-------------------------------------------------*
      *              * Tipi archivio ammessi                           *
      *              *-------------------------------------------------*
           perform   vis-lst-tar-000      thru vis-lst-tar-999        .
           go to     vis-tes-reg-999.
       vis-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Origine del documento                           *
      *              *-------------------------------------------------*
           perform   vis-org-doc-000      thru vis-org-doc-999        .
      *              *-------------------------------------------------*
      *              * Sigla numerazione                               *
      *              *-------------------------------------------------*
           perform   vis-sgl-num-000      thru vis-sgl-num-999        .
      *              *-------------------------------------------------*
      *              * Movimento a fronte di                           *
      *              *-------------------------------------------------*
           perform   vis-mov-afd-000      thru vis-mov-afd-999        .
      *              *-------------------------------------------------*
      *              * Default tipo movimento a fronte                 *
      *              *-------------------------------------------------*
           perform   vis-def-tmf-000      thru vis-def-tmf-999        .
      *              *-------------------------------------------------*
      *              * Validita' per le dipendenze                     *
      *              *-------------------------------------------------*
           perform   vis-vld-dpz-000      thru vis-vld-dpz-999        .
      *              *-------------------------------------------------*
      *              * Default per accettazione tipo riga corpo        *
      *              *-------------------------------------------------*
           perform   vis-def-tpr-000      thru vis-def-tpr-999        .
      *              *-------------------------------------------------*
      *              * Default per tipo vendita                        *
      *              *-------------------------------------------------*
           perform   vis-def-tva-000      thru vis-def-tva-999        .
      *              *-------------------------------------------------*
      *              * Si/no default data inizio trasporto             *
      *              *-------------------------------------------------*
           perform   vis-snx-ddi-000      thru vis-snx-ddi-999        .
      *              *-------------------------------------------------*
      *              * Codice listino da proporre                      *
      *              *-------------------------------------------------*
           perform   vis-cod-lst-000      thru vis-cod-lst-999        .
           perform   vis-cod-lst-des-000  thru vis-cod-lst-des-999    .
      *              *-------------------------------------------------*
      *              * Si/no numerazione modificabile                  *
      *              *-------------------------------------------------*
           perform   vis-snx-nmm-000      thru vis-snx-nmm-999        .
      *              *-------------------------------------------------*
      *              * Si/no numerazione controllata                   *
      *              *-------------------------------------------------*
           perform   vis-snx-nmc-000      thru vis-snx-nmc-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-tes-reg-999.
       vis-tes-reg-300.
      *              *-------------------------------------------------*
      *              * Numero copie da stampare                        *
      *              *-------------------------------------------------*
           perform   vis-num-cps-000      thru vis-num-cps-999        .
      *              *-------------------------------------------------*
      *              * Si/no controllo di stampa gia' effettuata       *
      *              *-------------------------------------------------*
           perform   vis-snx-lib-000      thru vis-snx-lib-999        .
      *              *-------------------------------------------------*
      *              * Si/no numero copie variabile                    *
      *              *-------------------------------------------------*
           perform   vis-snx-ncv-000      thru vis-snx-ncv-999        .
      *              *-------------------------------------------------*
      *              * Si/no stampa prezzo                             *
      *              *-------------------------------------------------*
           perform   vis-snx-prz-000      thru vis-snx-prz-999        .
      *              *-------------------------------------------------*
      *              * Si/no stampa sconti                             *
      *              *-------------------------------------------------*
           perform   vis-snx-sco-000      thru vis-snx-sco-999        .
      *              *-------------------------------------------------*
      *              * Si/no stampa importo                            *
      *              *-------------------------------------------------*
           perform   vis-snx-imp-000      thru vis-snx-imp-999        .
      *              *-------------------------------------------------*
      *              * Si/no stampa codice iva                         *
      *              *-------------------------------------------------*
           perform   vis-snx-civ-000      thru vis-snx-civ-999        .
      *              *-------------------------------------------------*
      *              * Si/no stampa totale documento                   *
      *              *-------------------------------------------------*
           perform   vis-snx-ttd-000      thru vis-snx-ttd-999        .
      *              *-------------------------------------------------*
      *              * Si/no stampa dicitura                           *
      *              *-------------------------------------------------*
           perform   vis-snx-dct-000      thru vis-snx-dct-999        .
      *              *-------------------------------------------------*
      *              * Descrizione dicitura                            *
      *              *-------------------------------------------------*
           perform   vis-des-dct-000      thru vis-des-dct-999        .
      *              *-------------------------------------------------*
      *              * Si/No stampa Agente                             *
      *              *-------------------------------------------------*
           perform   vis-snx-age-000      thru vis-snx-age-999        .
      *              *-------------------------------------------------*
      *              * Si/No stampa ns. dipendenza                     *
      *              *-------------------------------------------------*
           perform   vis-snx-ndp-000      thru vis-snx-ndp-999        .
      *              *-------------------------------------------------*
      *              * Si/No stampa forma di pagamento                 *
      *              *-------------------------------------------------*
           perform   vis-snx-fop-000      thru vis-snx-fop-999        .
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
      *              * Erase linee impegnate dalla testata             *
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
      *              * Descrizione tipo movimento                      *
      *              *-------------------------------------------------*
           perform   pmt-des-tmb-000      thru pmt-des-tmb-999        .
      *              *-------------------------------------------------*
      *              * Password per il movimento                       *
      *              *-------------------------------------------------*
           perform   pmt-pwd-tmb-000      thru pmt-pwd-tmb-999        .
      *              *-------------------------------------------------*
      *              * Descrizione per la stampa                       *
      *              *-------------------------------------------------*
           perform   pmt-des-stp-000      thru pmt-des-stp-999        .
      *              *-------------------------------------------------*
      *              * Interessa la fatturazione                       *
      *              *-------------------------------------------------*
           perform   pmt-int-ftr-000      thru pmt-int-ftr-999        .
      *              *-------------------------------------------------*
      *              * Tipo movimento per fatturazione                 *
      *              *-------------------------------------------------*
           perform   pmt-tmo-ftr-000      thru pmt-tmo-ftr-999        .
      *              *-------------------------------------------------*
      *              * Si/No accompagnamento merce                     *
      *              *-------------------------------------------------*
           perform   pmt-snx-acm-000      thru pmt-snx-acm-999        .
      *              *-------------------------------------------------*
      *              * Default trasporto a cura                        *
      *              *-------------------------------------------------*
           perform   pmt-def-tac-000      thru pmt-def-tac-999        .
      *              *-------------------------------------------------*
      *              * Default causale trasporto                       *
      *              *-------------------------------------------------*
           perform   pmt-def-ctr-000      thru pmt-def-ctr-999        .
      *              *-------------------------------------------------*
      *              * Causale magazzino                               *
      *              *-------------------------------------------------*
           perform   pmt-cau-mag-000      thru pmt-cau-mag-999        .
      *              *-------------------------------------------------*
      *              * Codice c/merce                                  *
      *              *-------------------------------------------------*
           perform   pmt-cod-mic-000      thru pmt-cod-mic-999        .
      *              *-------------------------------------------------*
      *              * Causale magazzino aggiuntiva                    *
      *              *-------------------------------------------------*
           perform   pmt-cam-agg-000      thru pmt-cam-agg-999        .
      *              *-------------------------------------------------*
      *              * Default tipo archivio                           *
      *              *-------------------------------------------------*
           perform   pmt-def-tar-000      thru pmt-def-tar-999        .
      *              *-------------------------------------------------*
      *              * Tipo archivio modificabile                      *
      *              *-------------------------------------------------*
           perform   pmt-snv-tar-000      thru pmt-snv-tar-999        .
      *              *-------------------------------------------------*
      *              * Tipi archivio ammessi                           *
      *              *-------------------------------------------------*
           perform   pmt-lst-tar-000      thru pmt-lst-tar-999        .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Origine del documento                           *
      *              *-------------------------------------------------*
           perform   pmt-org-doc-000      thru pmt-org-doc-999        .
      *              *-------------------------------------------------*
      *              * Sigla numerazione                               *
      *              *-------------------------------------------------*
           perform   pmt-sgl-num-000      thru pmt-sgl-num-999        .
      *              *-------------------------------------------------*
      *              * Movimento a fronte di                           *
      *              *-------------------------------------------------*
           perform   pmt-mov-afd-000      thru pmt-mov-afd-999        .
      *              *-------------------------------------------------*
      *              * Default tipo movimento a fronte                 *
      *              *-------------------------------------------------*
           perform   pmt-def-tmf-000      thru pmt-def-tmf-999        .
      *              *-------------------------------------------------*
      *              * Validita' per le dipendenze                     *
      *              *-------------------------------------------------*
           perform   pmt-vld-dpz-000      thru pmt-vld-dpz-999        .
      *              *-------------------------------------------------*
      *              * Default per accettazione tipo riga corpo        *
      *              *-------------------------------------------------*
           perform   pmt-def-tpr-000      thru pmt-def-tpr-999        .
      *              *-------------------------------------------------*
      *              * Default per tipo vendita                        *
      *              *-------------------------------------------------*
           perform   pmt-def-tva-000      thru pmt-def-tva-999        .
      *              *-------------------------------------------------*
      *              * Si/no default data inizio trasporto             *
      *              *-------------------------------------------------*
           perform   pmt-snx-ddi-000      thru pmt-snx-ddi-999        .
      *              *-------------------------------------------------*
      *              * Codice listino da proporre                      *
      *              *-------------------------------------------------*
           perform   pmt-cod-lst-000      thru pmt-cod-lst-999        .
      *              *-------------------------------------------------*
      *              * Si/no numerazione modificabile                  *
      *              *-------------------------------------------------*
           perform   pmt-snx-nmm-000      thru pmt-snx-nmm-999        .
      *              *-------------------------------------------------*
      *              * Si/no numerazione controllata                   *
      *              *-------------------------------------------------*
           perform   pmt-snx-nmc-000      thru pmt-snx-nmc-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-tes-reg-999.
       pmt-tes-reg-300.
      *              *-------------------------------------------------*
      *              * Personalizzazioni stampa corpo                  *
      *              *-------------------------------------------------*
           perform   pmt-prs-stc-000      thru pmt-prs-stc-999        .
      *              *-------------------------------------------------*
      *              * Numero copie da stampare                        *
      *              *-------------------------------------------------*
           perform   pmt-num-cps-000      thru pmt-num-cps-999        .
      *              *-------------------------------------------------*
      *              * Si/no controllo di stampa gia' effettuata       *
      *              *-------------------------------------------------*
           perform   pmt-snx-lib-000      thru pmt-snx-lib-999        .
      *              *-------------------------------------------------*
      *              * Si/no numero copie variabile                    *
      *              *-------------------------------------------------*
           perform   pmt-snx-ncv-000      thru pmt-snx-ncv-999        .
      *              *-------------------------------------------------*
      *              * Si/no stampa prezzo                             *
      *              *-------------------------------------------------*
           perform   pmt-snx-prz-000      thru pmt-snx-prz-999        .
      *              *-------------------------------------------------*
      *              * Si/no stampa sconti                             *
      *              *-------------------------------------------------*
           perform   pmt-snx-sco-000      thru pmt-snx-sco-999        .
      *              *-------------------------------------------------*
      *              * Si/no stampa importo                            *
      *              *-------------------------------------------------*
           perform   pmt-snx-imp-000      thru pmt-snx-imp-999        .
      *              *-------------------------------------------------*
      *              * Si/no stampa codice iva                         *
      *              *-------------------------------------------------*
           perform   pmt-snx-civ-000      thru pmt-snx-civ-999        .
      *              *-------------------------------------------------*
      *              * Si/no stampa totale documento                   *
      *              *-------------------------------------------------*
           perform   pmt-snx-ttd-000      thru pmt-snx-ttd-999        .
      *              *-------------------------------------------------*
      *              * Si/no stampa dicitura                           *
      *              *-------------------------------------------------*
           perform   pmt-snx-dct-000      thru pmt-snx-dct-999        .
      *              *-------------------------------------------------*
      *              * Descrizione dicitura                            *
      *              *-------------------------------------------------*
           perform   pmt-des-dct-000      thru pmt-des-dct-999        .
      *              *-------------------------------------------------*
      *              * Personalizzazioni stampa testata e piede        *
      *              *-------------------------------------------------*
           perform   pmt-prs-stp-000      thru pmt-prs-stp-999        .
      *              *-------------------------------------------------*
      *              * Si/No stampa Agente                             *
      *              *-------------------------------------------------*
           perform   pmt-snx-age-000      thru pmt-snx-age-999        .
      *              *-------------------------------------------------*
      *              * Si/No stampa ns. dipendenza                     *
      *              *-------------------------------------------------*
           perform   pmt-snx-ndp-000      thru pmt-snx-ndp-999        .
      *              *-------------------------------------------------*
      *              * Si/No stampa forma di pagamento                 *
      *              *-------------------------------------------------*
           perform   pmt-snx-fop-000      thru pmt-snx-fop-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-tes-reg-999.
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Descrizione tipo movimento       *
      *    *-----------------------------------------------------------*
       pmt-des-tmb-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Descrizione tipo movimento :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-des-tmb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Password per il movimento        *
      *    *-----------------------------------------------------------*
       pmt-pwd-tmb-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      62                   to   v-pos                  .
           move      "          "         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-pwd-tmb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Descrizione per la stampa        *
      *    *-----------------------------------------------------------*
       pmt-des-stp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Descrizione per la stampa  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-des-stp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Interessa la fatturazione        *
      *    *-----------------------------------------------------------*
       pmt-int-ftr-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Modalita' di fatturazione  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-int-ftr-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tipo movimento per fatturazione  *
      *    *-----------------------------------------------------------*
       pmt-tmo-ftr-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Movimento da utilizzare in :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "   fatturazione automatica  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tmo-ftr-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/No accompagnamento merce      *
      *    *-----------------------------------------------------------*
       pmt-snx-acm-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Accompagnamento merce      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-acm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Default trasporto a cura         *
      *    *-----------------------------------------------------------*
       pmt-def-tac-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      "Trasporto a cura  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-def-tac-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Default causale trasporto        *
      *    *-----------------------------------------------------------*
       pmt-def-ctr-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      "Causale trasporto :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-def-ctr-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Causale magazzino                *
      *    *-----------------------------------------------------------*
       pmt-cau-mag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice causale magazzino   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cau-mag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice c/merce magazzino         *
      *    *-----------------------------------------------------------*
       pmt-cod-mic-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice c/merce magazzino   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-mic-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Causale magazzino aggiuntiva     *
      *    *-----------------------------------------------------------*
       pmt-cam-agg-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Causale magazz. aggiuntiva :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cam-agg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Default tipo archivio            *
      *    *-----------------------------------------------------------*
       pmt-def-tar-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo archivio              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-def-tar-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tipo archivio modificabile       *
      *    *-----------------------------------------------------------*
       pmt-snv-tar-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo archivio modificabile :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snv-tar-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tipi archivio ammessi            *
      *    *-----------------------------------------------------------*
       pmt-lst-tar-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipi archivio ammessi      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-lst-tar-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Origine del documento            *
      *    *-----------------------------------------------------------*
       pmt-org-doc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Origine del documento      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-org-doc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Sigla numerazione                *
      *    *-----------------------------------------------------------*
       pmt-sgl-num-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Sigla numerazione          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-sgl-num-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Movimento a fronte di            *
      *    *-----------------------------------------------------------*
       pmt-mov-afd-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Movimento a fronte di      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-mov-afd-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Default tipo movimento a fronte  *
      *    *-----------------------------------------------------------*
       pmt-def-tmf-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo movimento a fronte    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-def-tmf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Validita' per le dipendenze      *
      *    *-----------------------------------------------------------*
       pmt-vld-dpz-000.
      *              *-------------------------------------------------*
      *              * Test se prompt da visualizzare                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su contatore dipendenze                *
      *                  *---------------------------------------------*
           if        w-dpz-ctr-dpz        >    1
                     go to pmt-vld-dpz-100.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-vld-dpz-999.
       pmt-vld-dpz-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione prompt                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Validita' per le dipendenze:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-vld-dpz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Default per accettazione tipo    *
      *    *                          riga corpo                       *
      *    *-----------------------------------------------------------*
       pmt-def-tpr-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Valori da proporre          "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "- Tipo riga corpo          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-def-tpr-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Default per tipo vendita         *
      *    *-----------------------------------------------------------*
       pmt-def-tva-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "- Tipo vendita per l'agente:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-def-tva-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no default inizio trasporto   *
      *    *-----------------------------------------------------------*
       pmt-snx-ddi-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "- Data inizio trasporto    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-ddi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice listino da proporre       *
      *    *-----------------------------------------------------------*
       pmt-cod-lst-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "- Codice listino proposto  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-lst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no numerazione modificabile   *
      *    *-----------------------------------------------------------*
       pmt-snx-nmm-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Altre opzioni               "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "- Numerazione modificabile :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-nmm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no numerazione controllata    *
      *    *-----------------------------------------------------------*
       pmt-snx-nmc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "- Numerazione controllata  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-nmc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Personalizzazioni stampa corpo   *
      *    *-----------------------------------------------------------*
       pmt-prs-stc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Personalizzazioni per stampa documento"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-prs-stc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Numero copie da stampare         *
      *    *-----------------------------------------------------------*
       pmt-num-cps-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "- Numero copie da stampare :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-num-cps-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no controllo di stampa gia'   *
      *    *                          eseguita                         *
      *    *-----------------------------------------------------------*
       pmt-snx-lib-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      49                   to   v-pos                  .
           move      "- Controllo su stampa gia' :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      49                   to   v-pos                  .
           move      "                 eseguita   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-lib-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Numero copie variabile           *
      *    *-----------------------------------------------------------*
       pmt-snx-ncv-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "- Numero copie variabile   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-ncv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no stampa prezzo              *
      *    *-----------------------------------------------------------*
       pmt-snx-prz-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Per stampa corpo            "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "- Stampa prezzo unitario   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-prz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no stampa sconti              *
      *    *-----------------------------------------------------------*
       pmt-snx-sco-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "- Stampa sconti            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-sco-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no stampa importo             *
      *    *-----------------------------------------------------------*
       pmt-snx-imp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "- Stampa importo           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-imp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no stampa codice iva          *
      *    *-----------------------------------------------------------*
       pmt-snx-civ-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      49                   to   v-pos                  .
           move      "- Stampa codice iva        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-civ-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no stampa totale documento    *
      *    *-----------------------------------------------------------*
       pmt-snx-ttd-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      49                   to   v-pos                  .
           move      "- Stampa totale documento  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-ttd-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no stampa dicitura            *
      *    *-----------------------------------------------------------*
       pmt-snx-dct-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      49                   to   v-pos                  .
           move      "- Stampa dicitura          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-dct-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Descrizione dicitura             *
      *    *-----------------------------------------------------------*
       pmt-des-dct-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "- Dicitura  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-des-dct-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Personalizzazioni stampa testata *
      *    *                          e piede                          *
      *    *-----------------------------------------------------------*
       pmt-prs-stp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Per stampa testata e piede  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-prs-stp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no stampa Agente              *
      *    *-----------------------------------------------------------*
       pmt-snx-age-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "- Stampa agente            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no stampa ns. dipendenza      *
      *    *-----------------------------------------------------------*
       pmt-snx-ndp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "- Stampa ns. dipendenza    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-ndp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no stampa forma di pagamento  *
      *    *-----------------------------------------------------------*
       pmt-snx-fop-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      49                   to   v-pos                  .
           move      "- Stampa forma di pagamento:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-fop-999.
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
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
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
      *    * Accettazione campo testata : Descrizione tipo movimento   *
      *    *-----------------------------------------------------------*
       acc-des-tmb-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-des-tmb-100.
      *              *-------------------------------------------------*
      *              * Test se inserimento o modifica                  *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "I"
                     go to acc-des-tmb-120.
      *              *-------------------------------------------------*
      *              * Note operative                                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "[F4] per copiare il tipo movimento"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-des-tmb-120.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-des-tmb (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Cancellazione note operative attuali            *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      spaces               to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-des-tmb-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-des-tmb-999.
       acc-des-tmb-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-des-tmb (1)      .
       acc-des-tmb-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione         *
      *                  *---------------------------------------------*
           if        w-tes-des-tmb (1)    =    spaces
                     go to acc-des-tmb-100.
      *                  *---------------------------------------------*
      *                  * Se valore a non spaces il primo carattere   *
      *                  * non deve essere a spaces                    *
      *                  *---------------------------------------------*
           if        w-tes-des-tmb (1)
                    (01 : 01)             =    spaces
                     go to acc-des-tmb-100.
       acc-des-tmb-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione descrizione in uppercase       *
      *                  *---------------------------------------------*
           move      w-tes-des-tmb (1)    to   w-all-str-alf          .
           move      30                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-tes-des-key (1)      .
       acc-des-tmb-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-des-tmb-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-des-tmb-100.
       acc-des-tmb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione tipo movimento*
      *    *-----------------------------------------------------------*
       vis-des-tmb-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-des-tmb (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-tmb-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Descrizione per la stampa    *
      *    *-----------------------------------------------------------*
       acc-des-stp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione valore default                 *
      *                  *---------------------------------------------*
           if        w-tes-des-stp (1)    not  = spaces
                     go to acc-des-stp-100.
           move      w-tes-des-tmb (1)    to   w-tes-des-stp (1)      .
       acc-des-stp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      25                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-des-stp (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-des-stp-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-des-stp-999.
       acc-des-stp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-des-stp (1)      .
       acc-des-stp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-des-stp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-des-stp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-des-stp-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-des-stp-100.
       acc-des-stp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione tipo movimento*
      *    *-----------------------------------------------------------*
       vis-des-stp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      25                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-des-stp (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-stp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Password per tipo movimento  *
      *    *-----------------------------------------------------------*
       acc-pwd-tmb-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campo momentaneamente non accettato         *
      *                  *---------------------------------------------*
           go to     acc-pwd-tmb-999.
       acc-pwd-tmb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Password per tipo movim.  *
      *    *-----------------------------------------------------------*
       vis-pwd-tmb-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      w-tes-pwd-tmb (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-pwd-tmb-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Interessa la fatturazione    *
      *    *-----------------------------------------------------------*
       acc-int-ftr-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-int-ftr (1)    to   w-sav-int-ftr          .
       acc-int-ftr-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-int-ftr-lun    to   v-car                  .
           move      w-exp-int-ftr-num    to   v-ldt                  .
           move      "NDF#"               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-int-ftr-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-int-ftr (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-int-ftr-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-int-ftr-999.
       acc-int-ftr-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-int-ftr (1)      .
       acc-int-ftr-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : reimpostazione                    *
      *                  *---------------------------------------------*
           if        w-tes-int-ftr (1)    not  = zero
                     go to acc-int-ftr-600.
           if        v-key                =    "UP  "
                     go to acc-int-ftr-600
           else      go to acc-int-ftr-100.
       acc-int-ftr-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore attuale uguale a precedente :     *
      *                  * oltre                                       *
      *                  *---------------------------------------------*
           if        w-tes-int-ftr (1)    =    w-sav-int-ftr
                     go to acc-int-ftr-800.
      *                      *-----------------------------------------*
      *                      * Se il movimento non interessa la fattu- *
      *                      * razione                                 *
      *                      *-----------------------------------------*
           if        w-tes-int-ftr (1)    not  = 01
                     go to acc-int-ftr-700.
      *                          *-------------------------------------*
      *                          * Normalizzazione codice movimento    *
      *                          * fatturazione                        *
      *                          *-------------------------------------*
           move      spaces               to   w-tes-tmo-ftr (1)      .
           perform   vis-tmo-ftr-000      thru vis-tmo-ftr-999        .
           move      spaces               to   w-tes-tmo-ftr-des (1)  .
           perform   vis-tmo-ftr-des-000  thru vis-tmo-ftr-des-999    .
      *                          *-------------------------------------*
      *                          * Normalizzazione personalizzazioni   *
      *                          * per stampa documento                *
      *                          *-------------------------------------*
           move      02                   to   w-tes-snx-prz (1)      .
           move      02                   to   w-tes-snx-sco (1)      .
           move      02                   to   w-tes-snx-imp (1)      .
           move      02                   to   w-tes-snx-civ (1)      .
           move      02                   to   w-tes-snx-ttd (1)      .
           move      "N"                  to   w-tes-snx-fop (1)      .
      *                          *-------------------------------------*
      *                          * Oltre                               *
      *                          *-------------------------------------*
           go to     acc-int-ftr-800.
       acc-int-ftr-700.
      *                      *-----------------------------------------*
      *                      * Se il movimento interessa la fattura-   *
      *                      * zione                                   *
      *                      *-----------------------------------------*
           if        w-tes-int-ftr (1)    not  = 02
                     go to acc-int-ftr-750.
      *                          *-------------------------------------*
      *                          * Trattamento dati relativi all'ar-   *
      *                          * chivio                              *
      *                          *-------------------------------------*
           move      "C"                  to   w-tes-def-tar (1)      .
           move      "N"                  to   w-tes-snv-tar (1)      .
           move      "C"                  to   w-tes-lst-tar (1)      .
           perform   vis-def-tar-000      thru vis-def-tar-999        .
           perform   vis-snv-tar-000      thru vis-snv-tar-999        .
           perform   vis-lst-tar-000      thru vis-lst-tar-999        .
      *                          *-------------------------------------*
      *                          * Normalizzazione personalizzazioni   *
      *                          * per stampa corpo documento          *
      *                          *-------------------------------------*
           move      02                   to   w-tes-snx-prz (1)      .
           move      02                   to   w-tes-snx-sco (1)      .
           move      02                   to   w-tes-snx-imp (1)      .
           move      02                   to   w-tes-snx-civ (1)      .
           move      02                   to   w-tes-snx-ttd (1)      .
           move      "N"                  to   w-tes-snx-fop (1)      .
      *                          *-------------------------------------*
      *                          * Normalizzazione causale aggiuntiva  *
      *                          *-------------------------------------*
           move      zero                 to   w-tes-cam-agg (1)      .
           move      spaces               to   w-tes-cam-agg-des (1)  .
           perform   vis-cam-agg-000      thru vis-cam-agg-999        .
           perform   vis-cam-agg-des-000  thru vis-cam-agg-des-999    .
      *                          *-------------------------------------*
      *                          * Oltre                               *
      *                          *-------------------------------------*
           go to     acc-int-ftr-800.
       acc-int-ftr-750.
      *                      *-----------------------------------------*
      *                      * Se il movimento e' bolla/fattura        *
      *                      *-----------------------------------------*
           if        w-tes-int-ftr (1)    not  = 03
                     go to acc-int-ftr-800.
      *                          *-------------------------------------*
      *                          * Trattamento dati relativi all'ar-   *
      *                          * chivio                              *
      *                          *-------------------------------------*
           move      "C"                  to   w-tes-def-tar (1)      .
           move      "N"                  to   w-tes-snv-tar (1)      .
           move      "C"                  to   w-tes-lst-tar (1)      .
           perform   vis-def-tar-000      thru vis-def-tar-999        .
           perform   vis-snv-tar-000      thru vis-snv-tar-999        .
           perform   vis-lst-tar-000      thru vis-lst-tar-999        .
      *                          *-------------------------------------*
      *                          * Normalizzazione personalizzazioni   *
      *                          * per stampa documento                *
      *                          *-------------------------------------*
           move      01                   to   w-tes-snx-prz (1)      .
           move      01                   to   w-tes-snx-sco (1)      .
           move      01                   to   w-tes-snx-imp (1)      .
           move      01                   to   w-tes-snx-civ (1)      .
           move      02                   to   w-tes-snx-ttd (1)      .
           move      "S"                  to   w-tes-snx-fop (1)      .
      *                          *-------------------------------------*
      *                          * Normalizzazione causale aggiuntiva  *
      *                          *-------------------------------------*
           move      zero                 to   w-tes-cam-agg (1)      .
           move      spaces               to   w-tes-cam-agg-des (1)  .
           perform   vis-cam-agg-000      thru vis-cam-agg-999        .
           perform   vis-cam-agg-des-000  thru vis-cam-agg-des-999    .
      *                          *-------------------------------------*
      *                          * Oltre                               *
      *                          *-------------------------------------*
           go to     acc-int-ftr-800.
       acc-int-ftr-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-int-ftr-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-int-ftr-100.
       acc-int-ftr-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Interessa la fatturazione *
      *    *-----------------------------------------------------------*
       vis-int-ftr-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-int-ftr-lun    to   v-car                  .
           move      w-exp-int-ftr-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-int-ftr-tbl    to   v-txt                  .
           move      w-tes-int-ftr (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-int-ftr-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Tipo movimento per fattura-  *
      *    *                              zione                        *
      *    *-----------------------------------------------------------*
       acc-tmo-ftr-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-int-ftr (1)    =    01
                     go to acc-tmo-ftr-999.
       acc-tmo-ftr-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-zfi-ope      .
           move      w-tes-tmo-ftr (1)    to   w-cod-des-zfi-cod      .
           move      10                   to   w-cod-des-zfi-lin      .
           move      30                   to   w-cod-des-zfi-pos      .
           move      10                   to   w-cod-des-zfi-dln      .
           move      37                   to   w-cod-des-zfi-dps      .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           perform   cod-des-zfi-cll-000  thru cod-des-zfi-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-zfi-foi-000  thru cod-des-zfi-foi-999    .
       acc-tmo-ftr-110.
           perform   cod-des-zfi-cll-000  thru cod-des-zfi-cll-999    .
           if        w-cod-des-zfi-ope    =    "F+"
                     go to acc-tmo-ftr-115.
           if        w-cod-des-zfi-ope    =    "AC"
                     go to acc-tmo-ftr-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-tmo-ftr-115.
           perform   cod-des-zfi-foi-000  thru cod-des-zfi-foi-999    .
           go to     acc-tmo-ftr-110.
       acc-tmo-ftr-120.
           move      w-cod-des-zfi-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-tmo-ftr-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-tmo-ftr-999.
       acc-tmo-ftr-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-tmo-ftr (1)      .
       acc-tmo-ftr-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zfi]                      *
      *                  *---------------------------------------------*
           move      w-tes-tmo-ftr (1)    to   w-let-arc-zfi-cod      .
           perform   let-arc-zfi-000      thru let-arc-zfi-999        .
      *                  *---------------------------------------------*
      *                  * Trattamento descrizione                     *
      *                  *---------------------------------------------*
           move      w-let-arc-zfi-des    to   w-tes-tmo-ftr-des (1)  .
           perform   vis-tmo-ftr-des-000  thru vis-tmo-ftr-des-999    .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : reimpostazione          *
      *                  *---------------------------------------------*
           if        w-let-arc-zfi-flg    not  = spaces
                     go to acc-tmo-ftr-100.
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione, a meno *
      *                  * che non si sia in Up                        *
      *                  *---------------------------------------------*
           if        w-tes-tmo-ftr (1)    not  = spaces
                     go to acc-tmo-ftr-420.
           if        v-key                =    "UP  "
                     go to acc-tmo-ftr-600
           else      go to acc-tmo-ftr-100.
       acc-tmo-ftr-420.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione tipo documento per fattura- *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           move      w-let-arc-zfi-tdo    to   w-tes-tmo-ftr-tdo (1)  .
      *                  *---------------------------------------------*
      *                  * Controllo tipo documento : puo' essere solo *
      *                  * 01 - Fattura                                *
      *                  * 03 - Nota di Accredito                      *
      *                  *---------------------------------------------*
           if        w-tes-tmo-ftr-tdo (1)
                                          not  = 01 and
                     w-tes-tmo-ftr-tdo (1)
                                          not  = 03
                     go to acc-tmo-ftr-500.
      *                  *---------------------------------------------*
      *                  * Controllo origine del documento             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se documento da sottoporre a fatturazio-*
      *                      * ne differita, l'origine deve essere :   *
      *                      * 11 - Automatica                         *
      *                      *-----------------------------------------*
           if        w-tes-int-ftr (1)    =    02 and
                     w-let-arc-zfi-ord    not  = 11
                     go to acc-tmo-ftr-500.
      *                      *-----------------------------------------*
      *                      * Se documento e' anche fattura, l'origine*
      *                      * deve essere :                           *
      *                      * 02 - Manuale, fattura+bolla di consegna *
      *                      *-----------------------------------------*
           if        w-tes-int-ftr (1)    =    03 and
                     w-let-arc-zfi-ord    not  = 02
                     go to acc-tmo-ftr-500.
      *                  *---------------------------------------------*
      *                  * Controllo provenienza del documento         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se origine del documento :              *
      *                      * 11 - Automatica,                        *
      *                      * la provenienza deve essere :            *
      *                      * 01 - Fatturazione automatica da bolle di*
      *                      *      consegna                           *
      *                      *-----------------------------------------*
           if        w-let-arc-zfi-ord    =    11 and
                     w-let-arc-zfi-prd    not  = 01
                     go to acc-tmo-ftr-500.
      *                  *---------------------------------------------*
      *                  * A dipendenze dall'impostazione              *
      *                  *---------------------------------------------*
           go to     acc-tmo-ftr-600.
       acc-tmo-ftr-500.
      *                  *---------------------------------------------*
      *                  * Trattamento errore                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Tipo movimento per la fatturazione incompatibile !
      -              "      "             to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * Reimpostazione                          *
      *                      *-----------------------------------------*
           go to     acc-tmo-ftr-100.
       acc-tmo-ftr-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo documento per fatturazione diffe-   *
      *                  * rita : 03 - Nota di Accredito, si norma-    *
      *                  * lizzano i dati relativi al trasporto        *
      *                  *---------------------------------------------*
            if       w-tes-tmo-ftr-tdo (1)
                                          not  = 03
                     go to acc-tmo-ftr-800.
      *                      *-----------------------------------------*
      *                      * Si/No accompagna merce                  *
      *                      *-----------------------------------------*
            move     "N"                  to   w-tes-snx-acm (1)      .
            perform  vis-snx-acm-000      thru vis-snx-acm-999        .
      *                      *-----------------------------------------*
      *                      * Default trasporto a cura                *
      *                      *-----------------------------------------*
            move     zero                 to   w-tes-def-tac (1)      .
            perform  vis-def-tac-000      thru vis-def-tac-999        .
      *                      *-----------------------------------------*
      *                      * Default causale del trasporto           *
      *                      *-----------------------------------------*
           move      spaces               to   w-tes-def-ctr (1)      .
           move      spaces               to   w-tes-def-ctr-des (1)  .
           perform   vis-def-ctr-000      thru vis-def-ctr-999        .
           perform   vis-def-ctr-des-000  thru vis-def-ctr-des-999    .
       acc-tmo-ftr-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-tmo-ftr-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-tmo-ftr-100.
       acc-tmo-ftr-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Tipo movimento per fattu- *
      *    *                                 razione                   *
      *    *-----------------------------------------------------------*
       vis-tmo-ftr-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-tmo-ftr (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tmo-ftr-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione tipo movimento*
      *    *                                 per fatturazione          *
      *    *-----------------------------------------------------------*
       vis-tmo-ftr-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-tes-tmo-ftr-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tmo-ftr-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Si/No accompagnamento merce  *
      *    *-----------------------------------------------------------*
       acc-snx-acm-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-tmo-ftr-tdo (1)
                                          =    03
                     go to acc-snx-acm-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-snx-acm (1)    to   w-sav-snx-acm          .
       acc-snx-acm-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      "SN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           if        w-tes-snx-acm (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-acm (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-snx-acm-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-acm-999.
       acc-snx-acm-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   w-tes-snx-acm (1)
           else if   v-num                =    02
                     move  "N"            to   w-tes-snx-acm (1)
           else      move  spaces         to   w-tes-snx-acm (1)      .
       acc-snx-acm-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se spaces : reimpostazione, a meno che non  *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        w-tes-snx-acm (1)    not  = spaces
                     go to acc-snx-acm-600.
           if        v-key                =    "UP  "
                     go to acc-snx-acm-600
           else      go to acc-snx-acm-100.
       acc-snx-acm-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore attuale uguale a precedente :     *
      *                  * oltre                                       *
      *                  *---------------------------------------------*
           if        w-tes-snx-acm (1)    =    w-sav-snx-acm
                     go to acc-snx-acm-800.
      *                  *---------------------------------------------*
      *                  * Se il documento accompagna merce : oltre    *
      *                  *---------------------------------------------*
           if        w-tes-snx-acm (1)    =    "S"
                     go to acc-snx-acm-800.
      *                  *---------------------------------------------*
      *                  * Trattamento default per trasporto a cura    *
      *                  *---------------------------------------------*
           if        w-tes-def-tac (1)    =    zero
                     go to acc-snx-acm-620.
           move      zero                 to   w-tes-def-tac (1)      .
           perform   vis-def-tac-000      thru vis-def-tac-999        .
       acc-snx-acm-620.
      *                  *---------------------------------------------*
      *                  * Trattamento default per cusale trasporto    *
      *                  *---------------------------------------------*
           if        w-tes-def-ctr (1)    =    spaces
                     go to acc-snx-acm-800.
           move      spaces               to   w-tes-def-ctr (1)      .
           move      spaces               to   w-tes-def-ctr-des (1)  .
           perform   vis-def-ctr-000      thru vis-def-ctr-999        .
           perform   vis-def-ctr-des-000  thru vis-def-ctr-des-999    .
       acc-snx-acm-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-acm-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-acm-100.
       acc-snx-acm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Si/No accompagn. merce    *
      *    *-----------------------------------------------------------*
       vis-snx-acm-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           if        w-tes-snx-acm (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-acm (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-acm-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Default trasporto a cura     *
      *    *-----------------------------------------------------------*
       acc-def-tac-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-tmo-ftr-tdo (1)
                                          =    03
                     go to acc-def-tac-999.
           if        w-tes-snx-acm (1)    not  = "S"
                     go to acc-def-tac-999.
       acc-def-tac-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-def-tac-lun    to   v-car                  .
           move      w-exp-def-tac-num    to   v-ldt                  .
           move      "MDV#"               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      12                   to   v-lin                  .
           move      56                   to   v-pos                  .
           move      w-exp-def-tac-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           if        w-tes-def-tac (1)    =    01
                     move  01             to   v-num
           else if   w-tes-def-tac (1)    =    10
                     move  02             to   v-num
           else if   w-tes-def-tac (1)    =    20
                     move  03             to   v-num
           else if   w-tes-def-tac (1)    =    30
                     move  04             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-def-tac-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-def-tac-999.
       acc-def-tac-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  01             to   w-tes-def-tac (1)
           else if   v-num                =    02
                     move  10             to   w-tes-def-tac (1)
           else if   v-num                =    03
                     move  20             to   w-tes-def-tac (1)
           else if   v-num                =    04
                     move  30             to   w-tes-def-tac (1)
           else      move  zero           to   w-tes-def-tac (1)      .
       acc-def-tac-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-def-tac-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-def-tac-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-def-tac-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-def-tac-100.
       acc-def-tac-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Default trasporto a cura  *
      *    *-----------------------------------------------------------*
       vis-def-tac-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-def-tac-lun    to   v-car                  .
           move      w-exp-def-tac-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      12                   to   v-lin                  .
           move      56                   to   v-pos                  .
           move      w-exp-def-tac-tbl    to   v-txt                  .
           if        w-tes-def-tac (1)    =    01
                     move  01             to   v-num
           else if   w-tes-def-tac (1)    =    10
                     move  02             to   v-num
           else if   w-tes-def-tac (1)    =    20
                     move  03             to   v-num
           else if   w-tes-def-tac (1)    =    30
                     move  04             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-def-tac-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Default causale trasporto    *
      *    *-----------------------------------------------------------*
       acc-def-ctr-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-tmo-ftr-tdo (1)
                                          =    03
                     go to acc-def-ctr-999.
           if        w-tes-snx-acm (1)    not  = "S"
                     go to acc-def-ctr-999.
       acc-def-ctr-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-zct-ope      .
           move      w-tes-def-ctr (1)    to   w-cod-des-zct-cod      .
           move      13                   to   w-cod-des-zct-lin      .
           move      56                   to   w-cod-des-zct-pos      .
           move      13                   to   w-cod-des-zct-dln      .
           move      61                   to   w-cod-des-zct-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           perform   cod-des-zct-cll-000  thru cod-des-zct-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-zct-foi-000  thru cod-des-zct-foi-999    .
       acc-def-ctr-110.
           perform   cod-des-zct-cll-000  thru cod-des-zct-cll-999    .
           if        w-cod-des-zct-ope    =    "F+"
                     go to acc-def-ctr-115.
           if        w-cod-des-zct-ope    =    "AC"
                     go to acc-def-ctr-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-def-ctr-115.
           perform   cod-des-zct-foi-000  thru cod-des-zct-foi-999    .
           go to     acc-def-ctr-110.
       acc-def-ctr-120.
           move      w-cod-des-zct-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-def-ctr-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-def-ctr-999.
      *              *-------------------------------------------------*
      *              * Valore impostato                                *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-def-ctr (1)      .
       acc-def-ctr-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella                             *
      *                  *---------------------------------------------*
           move      w-tes-def-ctr (1)    to   w-let-arc-zct-cod      .
           perform   let-arc-zct-000      thru let-arc-zct-999        .
           move      w-let-arc-zct-des    to   w-tes-def-ctr-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-def-ctr-des-000  thru vis-def-ctr-des-999    .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zct-flg    not  = spaces
                     go to acc-def-ctr-100.
       acc-def-ctr-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-def-ctr-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-def-ctr-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-def-ctr-100.
       acc-def-ctr-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Default causale del tra-  *
      *    *                                 sporto                    *
      *    *-----------------------------------------------------------*
       vis-def-ctr-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      56                   to   v-pos                  .
           move      w-tes-def-ctr (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-def-ctr-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione causale del   *
      *    * trasporto                                                 *
      *    *-----------------------------------------------------------*
       vis-def-ctr-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      61                   to   v-pos                  .
           move      w-tes-def-ctr-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-def-ctr-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Causale di magazzino         *
      *    *-----------------------------------------------------------*
       acc-cau-mag-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-cau-mag (1)    to   w-sav-cau-mag          .
       acc-cau-mag-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zmc-ope      .
           move      w-tes-cau-mag (1)    to   w-cod-mne-zmc-cod      .
           move      15                   to   w-cod-mne-zmc-lin      .
           move      30                   to   w-cod-mne-zmc-pos      .
           move      15                   to   w-cod-mne-zmc-dln      .
           move      37                   to   w-cod-mne-zmc-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           perform   cod-mne-zmc-cll-000  thru cod-mne-zmc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zmc-foi-000  thru cod-mne-zmc-foi-999    .
       acc-cau-mag-110.
           perform   cod-mne-zmc-cll-000  thru cod-mne-zmc-cll-999    .
           if        w-cod-mne-zmc-ope    =    "F+"
                     go to acc-cau-mag-115.
           if        w-cod-mne-zmc-ope    =    "AC"
                     go to acc-cau-mag-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cau-mag-115.
           perform   cod-mne-zmc-foi-000  thru cod-mne-zmc-foi-999    .
           go to     acc-cau-mag-110.
       acc-cau-mag-120.
           move      w-cod-mne-zmc-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cau-mag-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cau-mag-999.
       acc-cau-mag-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cau-mag (1)      .
       acc-cau-mag-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zmc]                      *
      *                  *---------------------------------------------*
           move      w-tes-cau-mag (1)    to   w-let-arc-zmc-cod      .
           perform   let-arc-zmc-000      thru let-arc-zmc-999        .
      *                  *---------------------------------------------*
      *                  * Trattamento descrizione                     *
      *                  *---------------------------------------------*
           move      w-let-arc-zmc-des    to   w-tes-cau-mag-des (1)  .
           perform   vis-cau-mag-des-000  thru vis-cau-mag-des-999    .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : reimpostazione          *
      *                  *---------------------------------------------*
           if        w-let-arc-zmc-flg    not  = spaces
                     go to acc-cau-mag-100.
      *                  *---------------------------------------------*
      *                  * Se causale di magazzino a zero : si saltano *
      *                  * ulteriori controlli                         *
      *                  *---------------------------------------------*
           if        w-tes-cau-mag (1)    =    zero
                     go to acc-cau-mag-600.
      *                  *---------------------------------------------*
      *                  * Controllo che non sia una causale di trat-  *
      *                  * tamento valore                              *
      *                  *---------------------------------------------*
           if        w-let-arc-zmc-trv    not  = "N"
                     go to acc-cau-mag-500.
      *                  *---------------------------------------------*
      *                  * Controllo che tipo movimentazione di magaz- *
      *                  * zino sia :                                  *
      *                  * 02 - Scarico oppure                         *
      *                  * 01 - Carico, se tipo movimento per fattura- *
      *                  *      zione differita 03 cioe' Nota di Ac-   *
      *                  *      credito                                *
      *                  * 01 - Carico, se tipo movimento di merce in  *
      *                  *      conto                                  *
      *                  *---------------------------------------------*
           if        w-tes-tmo-ftr-tdo (1)
                                          =    03 and
                     w-let-arc-zmc-mdm    =    01
                     go to acc-cau-mag-420.
           if        w-let-arc-zmc-ttc    =    03 and
                     w-let-arc-zmc-mdm    =    01
                     go to acc-cau-mag-480.
           if        w-let-arc-zmc-mdm    not  = 02
                     go to acc-cau-mag-500.
       acc-cau-mag-420.
      *                  *---------------------------------------------*
      *                  * Se documento che interessa la fatturazione, *
      *                  * controllo che tipo archivio sia 'C'         *
      *                  *---------------------------------------------*
           if        w-tes-int-ftr (1)    =    01
                     go to acc-cau-mag-480.
      *                      *-----------------------------------------*
      *                      * Se il tipo archivio di default non e'   *
      *                      * variabile                               *
      *                      *-----------------------------------------*
           if        w-let-arc-zmc-vaa    =    "N"
                     if    w-let-arc-zmc-dfa
                                          =    "C"
                           go to acc-cau-mag-480
                     else  go to acc-cau-mag-500.
      *                      *-----------------------------------------*
      *                      * Se il tipo archivio di default e' varia-*
      *                      * bile                                    *
      *                      *-----------------------------------------*
           if        w-let-arc-zmc-dfa    =    "C"
                     go to acc-cau-mag-480.
           if        w-let-arc-zmc-lsa    =    spaces
                     go to acc-cau-mag-480.
           move      zero                 to   w-acc-cau-mag-ctr      .
           inspect   w-let-arc-zmc-lsa
                                      tallying w-acc-cau-mag-ctr
                                      for all  "C"                    .
           if        w-acc-cau-mag-ctr    =    zero
                     go to acc-cau-mag-500.
       acc-cau-mag-480.
      *                  *---------------------------------------------*
      *                  * Controlli superati                          *
      *                  *---------------------------------------------*
           go to     acc-cau-mag-600.
       acc-cau-mag-500.
      *                  *---------------------------------------------*
      *                  * Messaggio di avviso                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "La causale di magazzino potrebbe essere incorretta
      -              "!"                  to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     acc-cau-mag-600.
       acc-cau-mag-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se causale di magazzino a zero              *
      *                  *---------------------------------------------*
           if        w-tes-cau-mag (1)    not  = zero
                     go to acc-cau-mag-620.
      *                      *-----------------------------------------*
      *                      * Normalizzazione c/merce                 *
      *                      *-----------------------------------------*
           if        w-tes-cod-mic (1)    =    spaces
                     go to acc-cau-mag-610.
           move      spaces               to   w-tes-cod-mic (1)      .
           move      spaces               to   w-tes-cod-mic-des (1)  .
           perform   vis-cod-mic-000      thru vis-cod-mic-999        .
           perform   vis-cod-mic-des-000  thru vis-cod-mic-des-999    .
       acc-cau-mag-610.
      *                      *-----------------------------------------*
      *                      * Trattamento causale aggiuntiva          *
      *                      *-----------------------------------------*
           if        w-tes-cam-agg (1)    =    zero
                     go to acc-cau-mag-615.
           move      zero                 to   w-tes-cam-agg (1)      .
           move      spaces               to   w-tes-cam-agg-des (1)  .
           perform   vis-cam-agg-000      thru vis-cam-agg-999        .
           perform   vis-cam-agg-des-000  thru vis-cam-agg-des-999    .
       acc-cau-mag-615.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     acc-cau-mag-800.
       acc-cau-mag-620.
      *                  *---------------------------------------------*
      *                  * Se causale di magazzino esistente           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Bufferizzazione valori per il c/merce   *
      *                      *-----------------------------------------*
           move      w-let-arc-zmc-ttc    to   w-tes-cau-mag-ttc (1)  .
           move      w-let-arc-zmc-tpc    to   w-tes-cau-mag-tpc (1)  .
           move      w-let-arc-zmc-cdc    to   w-tes-cau-mag-cdc (1)  .
           move      w-let-arc-zmc-vac    to   w-tes-cau-mag-vac (1)  .
      *                      *-----------------------------------------*
      *                      * Trattamento c/merce                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se causale non di c/merce           *
      *                          *-------------------------------------*
           if        w-let-arc-zmc-ttc    not  = 02
                     go to acc-cau-mag-670.
           if        w-tes-cod-mic (1)    =    spaces
                     go to acc-cau-mag-660.
           move      spaces               to   w-tes-cod-mic (1)      .
           move      spaces               to   w-tes-cod-mic-des (1)  .
           perform   vis-cod-mic-000      thru vis-cod-mic-999        .
           perform   vis-cod-mic-des-000  thru vis-cod-mic-des-999    .
       acc-cau-mag-660.
           go to     acc-cau-mag-700.
       acc-cau-mag-670.
      *                          *-------------------------------------*
      *                          * Se causale di c/merce               *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se codice c/merce modificabile :*
      *                              * oltre                           *
      *                              *---------------------------------*
           if        w-let-arc-zmc-vac    =    "S"
                     go to  acc-cau-mag-700.
      *                              *---------------------------------*
      *                              * Se codice c/merce non modifica- *
      *                              * bile                            *
      *                              *---------------------------------*
           if        w-let-arc-zmc-cdc    =    w-tes-cod-mic (1)
                     go to  acc-cau-mag-700.
           move      w-let-arc-zmc-cdc    to   w-tes-cod-mic (1)      .
           move      w-let-arc-zmc-cdc    to   w-let-arc-zmm-cod      .
           perform   let-arc-zmm-000      thru let-arc-zmm-999        .
           move      w-let-arc-zmm-des    to   w-tes-cod-mic-des (1)  .
      *                              *---------------------------------*
      *                              * Visualizzazione                 *
      *                              *---------------------------------*
           perform   vis-cod-mic-000      thru vis-cod-mic-999        .
           perform   vis-cod-mic-des-000  thru vis-cod-mic-des-999    .
       acc-cau-mag-700.
      *                      *-----------------------------------------*
      *                      * Trattamento tipo archivio               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se il tipo movimento interessa la   *
      *                          * fatturazione i dati relativi al-    *
      *                          * l'archivio sono gia' stati forzati  *
      *                          *-------------------------------------*
           if        w-tes-int-ftr (1)    =    02 or
                     w-tes-int-ftr (1)    =    03
                     go to acc-cau-mag-750.
      *                          *-------------------------------------*
      *                          * Bufferizzazione valori per tipo ar- *
      *                          * chivio                              *
      *                          *-------------------------------------*
           move      w-let-arc-zmc-dfa    to   w-tes-def-tar (1)      .
           move      w-let-arc-zmc-vaa    to   w-tes-snv-tar (1)      .
           move      w-let-arc-zmc-lsa    to   w-tes-lst-tar (1)      .
      *                          *-------------------------------------*
      *                          * Visualizzazione dati                *
      *                          *-------------------------------------*
           perform   vis-def-tar-000      thru vis-def-tar-999        .
           perform   vis-snv-tar-000      thru vis-snv-tar-999        .
           perform   vis-lst-tar-000      thru vis-lst-tar-999        .
       acc-cau-mag-750.
      *                      *-----------------------------------------*
      *                      * Trattamento causale aggiuntiva          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test su conto merce                 *
      *                          *-------------------------------------*
           if        w-tes-cod-mic (1)    not  = spaces
                     go to acc-cau-mag-775.
      *                          *-------------------------------------*
      *                          * Test su movimento che interessa la  *
      *                          * fatturazione                        *
      *                          *-------------------------------------*
           if        w-tes-int-ftr (1)    not  = 01
                     go to acc-cau-mag-775.
      *                          *-------------------------------------*
      *                          * Test su tipo archivio ammesso dalla *
      *                          * causale principale                  *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se il tipo archivio di default  *
      *                              * non e' variabile                *
      *                              *---------------------------------*
           if        w-tes-snv-tar (1)    =    "N" and
                     w-tes-def-tar (1)    not  = "D"
                     go to acc-cau-mag-775.
      *                              *---------------------------------*
      *                              * Se il tipo archivio di default  *
      *                              * e' variabile                    *
      *                              *---------------------------------*
           if        w-tes-def-tar (1)    =    "D"
                     go to acc-cau-mag-800.
           if        w-tes-def-tar (1)    =    spaces
                     go to acc-cau-mag-800.
           move      zero                 to   w-acc-cam-agg-ctr      .
           inspect   w-tes-lst-tar (1)
                                      tallying w-acc-cam-agg-ctr
                                      for all  "D"                    .
           if        w-acc-cam-agg-ctr    =    zero
                     go to acc-cau-mag-775
           else      go to acc-cau-mag-800.
       acc-cau-mag-775.
      *                          *-------------------------------------*
      *                          * Normalizzazione causale aggiuntiva, *
      *                          * se non gia' normalizzata            *
      *                          *-------------------------------------*
           if        w-tes-cam-agg (1)    =    zero
                     go to acc-cau-mag-800.
           move      zero                 to   w-tes-cam-agg (1)      .
           move      spaces               to   w-tes-cam-agg-des (1)  .
           perform   vis-cam-agg-000      thru vis-cam-agg-999        .
           perform   vis-cam-agg-des-000  thru vis-cam-agg-des-999    .
       acc-cau-mag-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cau-mag-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cau-mag-100.
       acc-cau-mag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Causale di magazzino      *
      *    *-----------------------------------------------------------*
       vis-cau-mag-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cau-mag (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cau-mag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione causale di    *
      *    *                                 magazzino                 *
      *    *-----------------------------------------------------------*
       vis-cau-mag-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-tes-cau-mag-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-cau-mag-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice merce in conto        *
      *    *-----------------------------------------------------------*
       acc-cod-mic-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se causale di magazzino non esistente : *
      *                      * uscita                                  *
      *                      *-----------------------------------------*
           if        w-tes-cau-mag (1)    =    zero
                     go to acc-cod-mic-999.
      *                      *-----------------------------------------*
      *                      * Test su trattamento c/merce della cau-  *
      *                      * sale di magazzino                       *
      *                      *-----------------------------------------*
           if        w-tes-cau-mag-ttc (1)
                                          =    02
                     go to acc-cod-mic-999.
      *                      *-----------------------------------------*
      *                      * Se codice c/merce non variabile : uscita*
      *                      *-----------------------------------------*
           if        w-tes-cau-mag-vac (1)
                                          =    "N"
                     go to acc-cod-mic-999.
      *                  *---------------------------------------------*
      *                  * Preparazione valore di default              *
      *                  *---------------------------------------------*
           if        w-tes-cod-mic (1)    =    spaces
                     move  w-tes-cau-mag-cdc (1)
                                          to   w-tes-cod-mic (1)      .
       acc-cod-mic-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-zmm-ope      .
           move      w-tes-cod-mic (1)    to   w-cod-des-zmm-cod      .
           move      16                   to   w-cod-des-zmm-lin      .
           move      30                   to   w-cod-des-zmm-pos      .
           move      16                   to   w-cod-des-zmm-dln      .
           move      37                   to   w-cod-des-zmm-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           perform   cod-des-zmm-cll-000  thru cod-des-zmm-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-zmm-foi-000  thru cod-des-zmm-foi-999    .
       acc-cod-mic-110.
           perform   cod-des-zmm-cll-000  thru cod-des-zmm-cll-999    .
           if        w-cod-des-zmm-ope    =    "F+"
                     go to acc-cod-mic-115.
           if        w-cod-des-zmm-ope    =    "AC"
                     go to acc-cod-mic-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-mic-115.
           perform   cod-des-zmm-foi-000  thru cod-des-zmm-foi-999    .
           go to     acc-cod-mic-110.
       acc-cod-mic-120.
           move      w-cod-des-zmm-cod    to   v-alf                  .
       acc-cod-mic-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-mic-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-mic-999.
       acc-cod-mic-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-cod-mic (1)      .
       acc-cod-mic-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella [zmm]                       *
      *                  *---------------------------------------------*
           move      w-tes-cod-mic (1)    to   w-let-arc-zmm-cod      .
           perform   let-arc-zmm-000      thru let-arc-zmm-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zmm-des    to   w-tes-cod-mic-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-cod-mic-des-000  thru vis-cod-mic-des-999    .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : reimpostazione          *
      *                  *---------------------------------------------*
           if        w-let-arc-zmm-flg    not  = spaces
                     go to acc-cod-mic-100.
      *                  *---------------------------------------------*
      *                  * Valore a spaces ammesso, purche' il tratta- *
      *                  * mento merce in conto sia :                  *
      *                  * 01 : Puo' riferirsi ad un altro documento;  *
      *                  * altrimenti reimpostazione, a meno che non si*
      *                  * sia in Up                                   *
      *                  *---------------------------------------------*
           if        w-tes-cod-mic (1)    not  = spaces
                     go to acc-cod-mic-600.
           if        w-tes-cau-mag-ttc (1)
                                          =    01
                     go to acc-cod-mic-600.
           if        v-key                =    "UP  "
                     go to acc-cod-mic-600
           else      go to acc-cod-mic-100.
       acc-cod-mic-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Trattamento causale aggiuntiva              *
      *                  *---------------------------------------------*
           if        w-tes-cod-mic (1)    =    spaces
                     go to acc-cod-mic-800.
           if        w-tes-cam-agg (1)    =    zero
                     go to acc-cod-mic-800.
           move      zero                 to   w-tes-cam-agg (1)      .
           move      spaces               to   w-tes-cam-agg-des (1)  .
           perform   vis-cam-agg-000      thru vis-cam-agg-999        .
           perform   vis-cam-agg-des-000  thru vis-cam-agg-des-999    .
       acc-cod-mic-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-mic-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-mic-100.
       acc-cod-mic-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice merce in conto     *
      *    *-----------------------------------------------------------*
       vis-cod-mic-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-mic (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-mic-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione per codice    *
      *    *                                 merce in conto            *
      *    *-----------------------------------------------------------*
       vis-cod-mic-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-tes-cod-mic-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-mic-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Causale magazzino aggiuntiva *
      *    *-----------------------------------------------------------*
       acc-cam-agg-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su causale di magazzino            *
      *                      *-----------------------------------------*
           if        w-tes-cau-mag (1)    =    zero
                     go to acc-cam-agg-999.
      *                      *-----------------------------------------*
      *                      * Test su conto merce                     *
      *                      *-----------------------------------------*
           if        w-tes-cod-mic (1)    not  = spaces
                     go to acc-cam-agg-999.
      *                      *-----------------------------------------*
      *                      * Test su movimento che interessa la fat- *
      *                      * turazione                               *
      *                      *-----------------------------------------*
           if        w-tes-int-ftr (1)    not  = 01
                     go to acc-cam-agg-999.
      *                      *-----------------------------------------*
      *                      * Test su tipo archivio ammesso dalla     *
      *                      * causale principale                      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se il tipo archivio di default non  *
      *                          * e' variabile                        *
      *                          *-------------------------------------*
           if        w-tes-snv-tar (1)    =    "N" and
                     w-tes-def-tar (1)    not  = "D"
                     go to acc-cam-agg-999.
      *                          *-------------------------------------*
      *                          * Se il tipo archivio di default e'   *
      *                          * variabile                           *
      *                          *-------------------------------------*
           if        w-tes-def-tar (1)    =    "D"
                     go to acc-cam-agg-050.
           if        w-tes-def-tar (1)    =    spaces
                     go to acc-cam-agg-050.
           move      zero                 to   w-acc-cam-agg-ctr      .
           inspect   w-tes-lst-tar (1)
                                      tallying w-acc-cam-agg-ctr
                                      for all  "D"                    .
           if        w-acc-cam-agg-ctr    =    zero
                     go to acc-cam-agg-999.
       acc-cam-agg-050.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-cam-agg (1)    to   w-sav-cam-agg          .
       acc-cam-agg-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zmc-ope      .
           move      w-tes-cam-agg (1)    to   w-cod-mne-zmc-cod      .
           move      17                   to   w-cod-mne-zmc-lin      .
           move      30                   to   w-cod-mne-zmc-pos      .
           move      17                   to   w-cod-mne-zmc-dln      .
           move      37                   to   w-cod-mne-zmc-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           perform   cod-mne-zmc-cll-000  thru cod-mne-zmc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zmc-foi-000  thru cod-mne-zmc-foi-999    .
       acc-cam-agg-110.
           perform   cod-mne-zmc-cll-000  thru cod-mne-zmc-cll-999    .
           if        w-cod-mne-zmc-ope    =    "F+"
                     go to acc-cam-agg-115.
           if        w-cod-mne-zmc-ope    =    "AC"
                     go to acc-cam-agg-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cam-agg-115.
           perform   cod-mne-zmc-foi-000  thru cod-mne-zmc-foi-999    .
           go to     acc-cam-agg-110.
       acc-cam-agg-120.
           move      w-cod-mne-zmc-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cam-agg-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cam-agg-999.
       acc-cam-agg-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cam-agg (1)      .
       acc-cam-agg-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zmc]                      *
      *                  *---------------------------------------------*
           move      w-tes-cam-agg (1)    to   w-let-arc-zmc-cod      .
           perform   let-arc-zmc-000      thru let-arc-zmc-999        .
      *                  *---------------------------------------------*
      *                  * Trattamento descrizione                     *
      *                  *---------------------------------------------*
           move      w-let-arc-zmc-des    to   w-tes-cam-agg-des (1)  .
           perform   vis-cam-agg-des-000  thru vis-cam-agg-des-999    .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : reimpostazione          *
      *                  *---------------------------------------------*
           if        w-let-arc-zmc-flg    not  = spaces
                     go to acc-cam-agg-100.
      *                  *---------------------------------------------*
      *                  * Se causale di magazzino a zero : si saltano *
      *                  * ulteriori controlli                         *
      *                  *---------------------------------------------*
           if        w-tes-cam-agg (1)    =    zero
                     go to acc-cam-agg-600.
      *                  *---------------------------------------------*
      *                  * Controllo che non sia una causale di trat-  *
      *                  * tamento valore                              *
      *                  *---------------------------------------------*
           if        w-let-arc-zmc-trv    not  = "N"
                     go to acc-cam-agg-500.
      *                  *---------------------------------------------*
      *                  * Controllo che tipo movimentazione di magaz- *
      *                  * zino sia :                                  *
      *                  * 01 - Carico                                 *
      *                  *---------------------------------------------*
           if        w-let-arc-zmc-mdm    not  = 01
                     go to acc-cam-agg-500.
      *                  *---------------------------------------------*
      *                  * Controllo se causale di giroconto merce     *
      *                  *---------------------------------------------*
           if        w-let-arc-zmc-ttc    not  = 02
                     go to acc-cam-agg-500.
      *                  *---------------------------------------------*
      *                  * Controllo che tipo archivio sia 'D'         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il tipo archivio di default non e'   *
      *                      * variabile                               *
      *                      *-----------------------------------------*
           if        w-let-arc-zmc-vaa    =    "N"
                     if    w-let-arc-zmc-dfa
                                          =    "D"
                           go to acc-cam-agg-480
                     else  go to acc-cam-agg-500.
      *                      *-----------------------------------------*
      *                      * Se il tipo archivio di default e' varia-*
      *                      * bile                                    *
      *                      *-----------------------------------------*
           if        w-let-arc-zmc-dfa    =    "D"
                     go to acc-cam-agg-480.
           if        w-let-arc-zmc-lsa    =    spaces
                     go to acc-cam-agg-480.
           move      zero                 to   w-acc-cam-agg-ctr      .
           inspect   w-let-arc-zmc-lsa
                                      tallying w-acc-cam-agg-ctr
                                      for all  "D"                    .
           if        w-acc-cam-agg-ctr    =    zero
                     go to acc-cam-agg-500.
       acc-cam-agg-480.
      *                  *---------------------------------------------*
      *                  * Controlli superati                          *
      *                  *---------------------------------------------*
           go to     acc-cam-agg-600.
       acc-cam-agg-500.
      *                  *---------------------------------------------*
      *                  * Controlli non superati                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "La causale di magazzino potrebbe essere incorretta
      -              "!"                  to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     acc-cam-agg-600.
       acc-cam-agg-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se causale di magazzino a zero              *
      *                  *---------------------------------------------*
           if        w-tes-cam-agg (1)    not  = zero
                     go to acc-cam-agg-620.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     acc-cam-agg-800.
       acc-cam-agg-620.
      *                  *---------------------------------------------*
      *                  * Se causale di magazzino esistente           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Forzatura valori per tipo archivio      *
      *                      *-----------------------------------------*
           move      "D"                  to   w-tes-def-tar (1)      .
           move      "N"                  to   w-tes-snv-tar (1)      .
           move      "D   "               to   w-tes-lst-tar (1)      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione dati                    *
      *                      *-----------------------------------------*
           perform   vis-def-tar-000      thru vis-def-tar-999        .
           perform   vis-snv-tar-000      thru vis-snv-tar-999        .
           perform   vis-lst-tar-000      thru vis-lst-tar-999        .
       acc-cam-agg-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cam-agg-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cam-agg-100.
       acc-cam-agg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Causale magazzino aggiun- *
      *    * tiva                                                      *
      *    *-----------------------------------------------------------*
       vis-cam-agg-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cam-agg (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cam-agg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione causale di    *
      *    *                                 magazzino aggiuntiva      *
      *    *-----------------------------------------------------------*
       vis-cam-agg-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-tes-cam-agg-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-cam-agg-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Tipo archivio di default     *
      *    *-----------------------------------------------------------*
       acc-def-tar-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-int-ftr (1)    =    02 or
                     w-tes-int-ftr (1)    =    03
                     go to acc-def-tar-999.
           if        w-tes-cau-mag (1)    not  = zero
                     go to acc-def-tar-999.
           if        w-tes-cam-agg (1)    not  = zero
                     go to acc-def-tar-999.
       acc-def-tar-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-def-tar-lun    to   v-car                  .
           move      w-exp-def-tar-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-def-tar-tbl    to   v-txt                  .
           if        w-tes-def-tar (1)    =    spaces
                     move  01             to   v-num
           else if   w-tes-def-tar (1)    =    "N"
                     move  02             to   v-num
           else if   w-tes-def-tar (1)    =    "C"
                     move  03             to   v-num
           else if   w-tes-def-tar (1)    =    "F"
                     move  04             to   v-num
           else if   w-tes-def-tar (1)    =    "D"
                     move  05             to   v-num
           else if   w-tes-def-tar (1)    =    "A"
                     move  06             to   v-num
           else      move  zero           to   v-num                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-def-tar-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-def-tar-999.
       acc-def-tar-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  spaces         to   w-tes-def-tar (1)
           else if   v-num                =    02
                     move  "N"            to   w-tes-def-tar (1)
           else if   v-num                =    03
                     move  "C"            to   w-tes-def-tar (1)
           else if   v-num                =    04
                     move  "F"            to   w-tes-def-tar (1)
           else if   v-num                =    05
                     move  "D"            to   w-tes-def-tar (1)
           else if   v-num                =    06
                     move  "A"            to   w-tes-def-tar (1)
           else      move  spaces         to   w-tes-def-tar (1)      .
       acc-def-tar-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se a zero : reimpostazione                  *
      *                  *---------------------------------------------*
           if        v-num                not  = zero
                     go to acc-def-tar-600.
           if        v-key                =    "UP  "
                     go to acc-def-tar-600
           else      go to acc-def-tar-100.
       acc-def-tar-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se "Nessun tipo archivio di default"        *
      *                  *---------------------------------------------*
           if        w-tes-def-tar (1)    not  = spaces
                     go to acc-def-tar-800.
      *                      *-----------------------------------------*
      *                      * Tipo archivio modificabile : 'S'        *
      *                      *-----------------------------------------*
           move      "S"                  to   w-tes-snv-tar (1)      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           perform   vis-snv-tar-000      thru vis-snv-tar-999        .
       acc-def-tar-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-def-tar-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-def-tar-100.
       acc-def-tar-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Tipo archivio di default  *
      *    *-----------------------------------------------------------*
       vis-def-tar-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-def-tar-lun    to   v-car                  .
           move      w-exp-def-tar-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-def-tar-tbl    to   v-txt                  .
           if        w-tes-def-tar (1)    =    spaces
                     move  01             to   v-num
           else if   w-tes-def-tar (1)    =    "N"
                     move  02             to   v-num
           else if   w-tes-def-tar (1)    =    "C"
                     move  03             to   v-num
           else if   w-tes-def-tar (1)    =    "F"
                     move  04             to   v-num
           else if   w-tes-def-tar (1)    =    "D"
                     move  05             to   v-num
           else if   w-tes-def-tar (1)    =    "A"
                     move  06             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-def-tar-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Tipo archivio modificabile   *
      *    *-----------------------------------------------------------*
       acc-snv-tar-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-int-ftr (1)    =    02 or
                     w-tes-int-ftr (1)    =    03
                     go to acc-snv-tar-999.
           if        w-tes-cau-mag (1)    not  = zero
                     go to acc-snv-tar-999.
           if        w-tes-def-tar (1)    =    spaces
                     go to acc-snv-tar-999.
       acc-snv-tar-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      "SN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           if        w-tes-snv-tar (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snv-tar (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-snv-tar-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snv-tar-999.
       acc-snv-tar-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   w-tes-snv-tar (1)
           else if   v-num                =    02
                     move  "N"            to   w-tes-snv-tar (1)
           else      move  spaces         to   w-tes-snv-tar (1)      .
       acc-snv-tar-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se a zero : reimpostazione                  *
      *                  *---------------------------------------------*
           if        v-num                not  = zero
                     go to acc-snv-tar-600.
           if        v-key                =    "UP  "
                     go to acc-snv-tar-600
           else      go to acc-snv-tar-100.
       acc-snv-tar-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snv-tar-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snv-tar-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snv-tar-100.
       acc-snv-tar-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Tipo archivio modific.    *
      *    *-----------------------------------------------------------*
       vis-snv-tar-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           if        w-tes-snv-tar (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snv-tar (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snv-tar-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipi archivio ammessi                *
      *    *-----------------------------------------------------------*
       acc-lst-tar-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-int-ftr (1)    =    02 or
                     w-tes-int-ftr (1)    =    03
                     go to acc-lst-tar-999.
           if        w-tes-cau-mag (1)    not  = zero
                     go to acc-lst-tar-999.
           if        w-tes-def-tar (1)    =    "N"
                     go to acc-lst-tar-999.
           if        w-tes-snv-tar (1)    not  = "N"
                     go to acc-lst-tar-050.
           move      w-tes-def-tar (1)    to   w-tes-lst-tar (1)      .
           perform   vis-lst-tar-000      thru vis-lst-tar-999        .
           go to     acc-lst-tar-999.
       acc-lst-tar-050.
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        w-tes-lst-tar (1)    not  = spaces
                     go to acc-lst-tar-100.
           if        w-tes-def-tar (1)    =    spaces
                     move  w-wrk-lta-lst-amm
                                          to   w-tes-lst-tar (1)
           else      move  w-tes-def-tar (1)
                                          to   w-tes-lst-tar (1)      .
       acc-lst-tar-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-lst-tar (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-lst-tar-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-lst-tar-999.
       acc-lst-tar-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-lst-tar (1)      .
       acc-lst-tar-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-lst-tar-420.
      *                  *---------------------------------------------*
      *                  * Test che il valore non sia a Spaces         *
      *                  *---------------------------------------------*
           if        w-tes-lst-tar (1)    =    spaces
                     go to acc-lst-tar-100.
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      w-tes-lst-tar (1)    to   w-all-str-alf          .
           move      04                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-lst-tar-100.
       acc-lst-tar-440.
      *                  *---------------------------------------------*
      *                  * Test se singoli caratteri accettabili       *
      *                  *---------------------------------------------*
           move      w-tes-lst-tar (1)    to   w-wrk-lta-lst-tar      .
           move      zero                 to   w-wrk-lta-inx-001      .
       acc-lst-tar-442.
           add       1                    to   w-wrk-lta-inx-001      .
           if        w-wrk-lta-inx-001    >    4
                     go to acc-lst-tar-460.
           if        w-wrk-lta-lst-ele
                    (w-wrk-lta-inx-001)   =    spaces
                     go to acc-lst-tar-442.
           move      zero                 to   w-wrk-lta-inx-002      .
           inspect   w-wrk-lta-lst-amm
                                      tallying w-wrk-lta-inx-002
                     for                  all  w-wrk-lta-lst-ele
                                              (w-wrk-lta-inx-001)     .
           if        w-wrk-lta-inx-002    =    zero
                     go to acc-lst-tar-100.
           go to     acc-lst-tar-442.
       acc-lst-tar-460.
      *                  *---------------------------------------------*
      *                  * Test che lo stesso tipo archivio non si ri- *
      *                  * peta piu' di una volta                      *
      *                  *---------------------------------------------*
           move      w-wrk-lta-lst-amm    to   w-wrk-lta-lst-tam      .
           move      zero                 to   w-wrk-lta-inx-001      .
       acc-lst-tar-462.
           add       1                    to   w-wrk-lta-inx-001      .
           if        w-wrk-lta-inx-001    >    4
                     go to acc-lst-tar-480.
           if        w-wrk-lta-lst-eam
                    (w-wrk-lta-inx-001)   =    spaces
                     go to acc-lst-tar-462.
           move      zero                 to   w-wrk-lta-inx-002      .
           inspect   w-wrk-lta-lst-tar
                                      tallying w-wrk-lta-inx-002
                     for                  all  w-wrk-lta-lst-eam
                                              (w-wrk-lta-inx-001)     .
           if        w-wrk-lta-inx-002    >    1
                     go to acc-lst-tar-100.
           go to     acc-lst-tar-462.
       acc-lst-tar-480.
      *                  *---------------------------------------------*
      *                  * Test che sia presente il tipo archivio di   *
      *                  * default                                     *
      *                  *---------------------------------------------*
           if        w-tes-def-tar (1)    =    spaces
                     go to acc-lst-tar-600.
           move      zero                 to   w-wrk-lta-inx-001      .
           inspect   w-tes-lst-tar (1)    
                                      tallying w-wrk-lta-inx-001
                     for                  all  w-tes-def-tar (1)      .
           if        w-wrk-lta-inx-001    =    zero
                     go to acc-lst-tar-100.
       acc-lst-tar-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-lst-tar-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-lst-tar-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-lst-tar-100.
       acc-lst-tar-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tipi archivio ammessi             *
      *    *-----------------------------------------------------------*
       vis-lst-tar-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-lst-tar (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-lst-tar-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Origine del documento        *
      *    *-----------------------------------------------------------*
       acc-org-doc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-org-doc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-org-doc-lun    to   v-car                  .
           move      w-exp-org-doc-num    to   v-ldt                  .
           move      "MA#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-org-doc-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           if        w-tes-org-doc (1)    =    01
                     move  01             to   v-num
           else if   w-tes-org-doc (1)    =    11
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-org-doc-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-org-doc-999.
       acc-org-doc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  01             to   w-tes-org-doc (1)
           else if   v-num                =    02
                     move  11             to   w-tes-org-doc (1)
           else      move  zero           to   w-tes-org-doc (1)      .
       acc-org-doc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : reimpostazione                    *
      *                  *---------------------------------------------*
           if        w-tes-org-doc (1)    not  = zero
                     go to acc-org-doc-600.
           if        v-key                =    "UP  "
                     go to acc-org-doc-600
           else      go to acc-org-doc-100.
       acc-org-doc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Provenienza del documento                   *
      *                  *---------------------------------------------*
           if        w-tes-org-doc (1)    =    11
                     move  01             to   w-tes-prv-doc (1)
           else      move  zero           to   w-tes-prv-doc (1)      .
       acc-org-doc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-org-doc-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-org-doc-100.
       acc-org-doc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Origine del documento     *
      *    *-----------------------------------------------------------*
       vis-org-doc-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-org-doc-lun    to   v-car                  .
           move      w-exp-org-doc-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-org-doc-tbl    to   v-txt                  .
           if        w-tes-org-doc (1)    =    01
                     move  01             to   v-num
           else if   w-tes-org-doc (1)    =    11
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-org-doc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Sigla numerazione            *
      *    *-----------------------------------------------------------*
       acc-sgl-num-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-sgl-num-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-sgl-num (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-sgl-num-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-sgl-num-999.
       acc-sgl-num-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-sgl-num (1)      .
       acc-sgl-num-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      w-tes-sgl-num (1)    to   w-all-str-alf          .
           move      03                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-sgl-num-100.
       acc-sgl-num-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sgl-num-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-sgl-num-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-sgl-num-100.
       acc-sgl-num-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Sigla numerazione         *
      *    *-----------------------------------------------------------*
       vis-sgl-num-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-sgl-num (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sgl-num-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Movimento a fronte di        *
      *    *-----------------------------------------------------------*
       acc-mov-afd-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-mov-afd (1)    to   w-sav-mov-afd          .
       acc-mov-afd-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-mov-afd-lun    to   v-car                  .
           move      w-exp-mov-afd-num    to   v-ldt                  .
           move      "NOSB#"              to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-mov-afd-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-mov-afd (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-mov-afd-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-mov-afd-999.
       acc-mov-afd-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-mov-afd (1)      .
       acc-mov-afd-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : reimpostazione, a meno che non    *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        w-tes-mov-afd (1)    not  = zero
                     go to acc-mov-afd-500.
           if        v-key                =    "UP  "
                     go to acc-mov-afd-600
           else      go to acc-mov-afd-100.
       acc-mov-afd-500.
      *                  *---------------------------------------------*
      *                  * Se tipo movimento per fatturazione differi- *
      *                  * ta 03, Nota di Accredito, il documento de-  *
      *                  * ve essre a fronte di : Niente               *
      *                  *---------------------------------------------*
           if        w-tes-tmo-ftr-tdo (1)
                                          not  = 03
                     go to acc-mov-afd-600.
           if        w-tes-mov-afd (1)    =    01
                     go to acc-mov-afd-600
           else      go to acc-mov-afd-100.
       acc-mov-afd-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore attuale come precedente : oltre   *
      *                  *---------------------------------------------*
           if        w-tes-mov-afd (1)    =    w-sav-mov-afd
                     go to acc-mov-afd-800.
      *                  *---------------------------------------------*
      *                  * Default tipo movimento a fronte             *
      *                  *---------------------------------------------*
           move      spaces               to   w-tes-def-tmf (1)      .
           perform   vis-def-tmf-000      thru vis-def-tmf-999        .
       acc-mov-afd-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-mov-afd-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-mov-afd-100.
       acc-mov-afd-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Movimento a fronte di     *
      *    *-----------------------------------------------------------*
       vis-mov-afd-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-mov-afd-lun    to   v-car                  .
           move      w-exp-mov-afd-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-mov-afd-tbl    to   v-txt                  .
           move      w-tes-mov-afd (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-mov-afd-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Default tipo movimento a     *
      *    * fronte                                                    *
      *    *-----------------------------------------------------------*
       acc-def-tmf-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-mov-afd (1)    =    01
                     go to acc-def-tmf-999.
       acc-def-tmf-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-def-tmf (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-def-tmf-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-def-tmf-999.
       acc-def-tmf-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-def-tmf (1)      .
       acc-def-tmf-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      w-tes-def-tmf (1)    to   w-all-str-alf          .
           move      05                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-def-tmf-100.
       acc-def-tmf-410.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo movimento   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-tes-mov-afd (1)    =    01 or
                     w-tes-mov-afd (1)    =    04
                     go to acc-def-tmf-600
           else if   w-tes-mov-afd (1)    =    02
                     go to acc-def-tmf-420
           else if   w-tes-mov-afd (1)    =    03
                     go to acc-def-tmf-430.
           go to     acc-def-tmf-600.
       acc-def-tmf-420.
      *                      *-----------------------------------------*
      *                      * Se a fronte ordine cliente              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se a spazi                     *
      *                          *-------------------------------------*
           if        w-tes-def-tmf (1)    =    spaces
                     go to acc-def-tmf-600.
      *                          *-------------------------------------*
      *                          * Lettura                             *
      *                          *-------------------------------------*
           move      w-tes-def-tmf (1)    to   w-let-arc-zoc-cod      .
           perform   let-arc-zoc-000      thru let-arc-zoc-999        .
      *                          *-------------------------------------*
      *                          * Se codice errato : reimpostazione   *
      *                          *-------------------------------------*
           if        w-let-arc-zoc-flg    not  = spaces
                     go to acc-def-tmf-100.
      *                          *-------------------------------------*
      *                          * Oltre                               *
      *                          *-------------------------------------*
           go to     acc-def-tmf-600.
       acc-def-tmf-430.
      *                      *-----------------------------------------*
      *                      * Se a fronte ordine di spedizione        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se a spazi                     *
      *                          *-------------------------------------*
           if        w-tes-def-tmf (1)    =    spaces
                     go to acc-def-tmf-600.
      *                          *-------------------------------------*
      *                          * Lettura                             *
      *                          *-------------------------------------*
           move      w-tes-def-tmf (1)    to   w-let-arc-zsc-cod      .
           perform   let-arc-zsc-000      thru let-arc-zsc-999        .
      *                          *-------------------------------------*
      *                          * Se codice errato : reimpostazione   *
      *                          *-------------------------------------*
           if        w-let-arc-zsc-flg    not  = spaces
                     go to acc-def-tmf-100.
      *                          *-------------------------------------*
      *                          * Oltre                               *
      *                          *-------------------------------------*
           go to     acc-def-tmf-600.
       acc-def-tmf-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-def-tmf-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-def-tmf-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-def-tmf-100.
       acc-def-tmf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Default tipo movimento a  *
      *    * fronte                                                    *
      *    *-----------------------------------------------------------*
       vis-def-tmf-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-def-tmf (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-def-tmf-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Validita' per le dipendenze  *
      *    *-----------------------------------------------------------*
       acc-vld-dpz-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su contatore dipendenze            *
      *                      *-----------------------------------------*
           if        w-dpz-ctr-dpz        >    1
                     go to acc-vld-dpz-050.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     acc-vld-dpz-999.
       acc-vld-dpz-050.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-vld-dpz (1)    to   w-sav-vld-dpz          .
       acc-vld-dpz-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-vld-dpz-lun    to   v-car                  .
           move      w-exp-vld-dpz-num    to   v-ldt                  .
           move      "TSDI#"              to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-vld-dpz-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           if        w-tes-vld-dpz (1)    =    spaces
                     move  01             to   v-num
           else if   w-tes-vld-dpz (1)    =    "S"
                     move  02             to   v-num
           else if   w-tes-vld-dpz (1)    =    "D"
                     move  03             to   v-num
           else if   w-tes-vld-dpz (1)    =    "X"
                     move  04             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-vld-dpz-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-vld-dpz-999.
       acc-vld-dpz-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  spaces         to   w-tes-vld-dpz (1)
           else if   v-num                =    02
                     move  "S"            to   w-tes-vld-dpz (1)
           else if   v-num                =    03
                     move  "D"            to   w-tes-vld-dpz (1)
           else if   v-num                =    04
                     move  "X"            to   w-tes-vld-dpz (1)
           else      move  spaces         to   w-tes-vld-dpz (1)      .
       acc-vld-dpz-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-vld-dpz-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore attuale uguale a precedente :     *
      *                  * oltre                                       *
      *                  *---------------------------------------------*
           if        w-tes-vld-dpz (1)    =    w-sav-vld-dpz
                     go to acc-vld-dpz-800.
       acc-vld-dpz-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-vld-dpz-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-vld-dpz-100.
       acc-vld-dpz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Validita' per le dipendenze       *
      *    *-----------------------------------------------------------*
       vis-vld-dpz-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su contatore dipendenze                *
      *                  *---------------------------------------------*
           if        w-dpz-ctr-dpz        >    1
                     go to vis-vld-dpz-100.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-vld-dpz-999.
       vis-vld-dpz-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione campo                           *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-vld-dpz-lun    to   v-car                  .
           move      w-exp-vld-dpz-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-vld-dpz-tbl    to   v-txt                  .
           if        w-tes-vld-dpz (1)    =    spaces
                     move  01             to   v-num
           else if   w-tes-vld-dpz (1)    =    "S"
                     move  02             to   v-num
           else if   w-tes-vld-dpz (1)    =    "D"
                     move  03             to   v-num
           else if   w-tes-vld-dpz (1)    =    "X"
                     move  04             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-vld-dpz-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Default per accettazione ti- *
      *    *                              po riga corpo                *
      *    *-----------------------------------------------------------*
       acc-def-tpr-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-def-tpr-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-def-tpr (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-def-tpr-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-def-tpr-999.
       acc-def-tpr-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-def-tpr (1)      .
       acc-def-tpr-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-def-tpr-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-def-tpr-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-def-tpr-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-def-tpr-100.
       acc-def-tpr-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Default per accettazione tipo ri- *
      *    *                         ga corpo                          *
      *    *-----------------------------------------------------------*
       vis-def-tpr-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-def-tpr (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-def-tpr-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Default per tipo vendita     *
      *    *-----------------------------------------------------------*
       acc-def-tva-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-def-tva-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-def-tva-lun    to   v-car                  .
           move      w-exp-def-tva-num    to   v-ldt                  .
           move      "DI#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-def-tva-tbl    to   v-txt                  .
           move      w-tes-def-tva (1)    to   v-num                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-def-tva-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-def-tva-999.
       acc-def-tva-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-def-tva (1)      .
       acc-def-tva-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-def-tva-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-def-tva-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-def-tva-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-def-tva-100.
       acc-def-tva-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Default per tipo vendita  *
      *    *-----------------------------------------------------------*
       vis-def-tva-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-def-tva-lun    to   v-car                  .
           move      w-exp-def-tva-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-def-tva-tbl    to   v-txt                  .
           move      w-tes-def-tva (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-def-tva-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Si/No default data inizio    *
      *    *                              trasporto                    *
      *    *-----------------------------------------------------------*
       acc-snx-ddi-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-snx-ddi-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      "SN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
      *
           if        w-tes-snx-par (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-par (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-snx-ddi-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-ddi-999.
       acc-snx-ddi-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   w-tes-snx-par (1)
           else if   v-num                =    02
                     move  "N"            to   w-tes-snx-par (1)
           else      move  spaces         to   w-tes-snx-par (1)      .
       acc-snx-ddi-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se spaces : reimpostazione, a meno che non  *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        w-tes-snx-par (1)    not  = spaces
                     go to acc-snx-ddi-600.
           if        v-key                =    "UP  "
                     go to acc-snx-ddi-600
           else      go to acc-snx-ddi-100.
       acc-snx-ddi-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-ddi-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-ddi-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-ddi-100.
       acc-snx-ddi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Si/No default data inizio trasp.  *
      *    *-----------------------------------------------------------*
       vis-snx-ddi-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
      *
           if        w-tes-snx-par (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-par (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-ddi-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice listino da proporre   *
      *    *-----------------------------------------------------------*
       acc-cod-lst-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-lst-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-lst-ope      .
           move      w-tes-cod-lst (1)    to   w-cod-cod-lst-cod      .
           move      17                   to   w-cod-cod-lst-lin      .
           move      30                   to   w-cod-cod-lst-pos      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           perform   cod-cod-zls-cll-000  thru cod-cod-zls-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-zls-foi-000  thru cod-cod-zls-foi-999    .
       acc-cod-lst-110.
           perform   cod-cod-zls-cll-000  thru cod-cod-zls-cll-999    .
           if        w-cod-cod-lst-ope    =    "F+"
                     go to acc-cod-lst-115.
           if        w-cod-cod-lst-ope    =    "AC"
                     go to acc-cod-lst-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-lst-115.
           perform   cod-cod-zls-foi-000  thru cod-cod-zls-foi-999    .
           go to     acc-cod-lst-110.
       acc-cod-lst-120.
           move      w-cod-cod-lst-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-lst-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-lst-999.
       acc-cod-lst-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-cod-lst (1)      .
       acc-cod-lst-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zls]                      *
      *                  *---------------------------------------------*
           move      w-tes-cod-lst (1)    to   w-let-arc-zls-cod      .
           perform   let-arc-zls-000      thru let-arc-zls-999        .
      *
           if        w-tes-cod-lst (1)    =    spaces
                     move  "(nessun default)"
                                          to   w-tes-cod-lst-des (1)
           else      move  w-let-arc-zls-des
                                          to   w-tes-cod-lst-des (1)  .
       acc-cod-lst-420.
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-cod-lst-des-000  thru vis-cod-lst-des-999    .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zls-flg    not  = spaces
                     go to acc-cod-lst-100.
       acc-cod-lst-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-lst-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-lst-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-lst-100.
       acc-cod-lst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice listino da proporre        *
      *    *-----------------------------------------------------------*
       vis-cod-lst-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-lst (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-lst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Descrizione listino               *
      *    *-----------------------------------------------------------*
       vis-cod-lst-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-tes-cod-lst-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-lst-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Si/No numerazione modifica-  *
      *    *                              bile                         *
      *    *-----------------------------------------------------------*
       acc-snx-nmm-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-snx-nmm-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      "SN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           if        w-tes-snx-nmm (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-nmm (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-snx-nmm-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-nmm-999.
       acc-snx-nmm-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   w-tes-snx-nmm (1)
           else if   v-num                =    02
                     move  "N"            to   w-tes-snx-nmm (1)
           else      move  spaces         to   w-tes-snx-nmm (1)      .
       acc-snx-nmm-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-snx-nmm-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-nmm-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-nmm-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-nmm-100.
       acc-snx-nmm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Si/No numerazione modifi- *
      *    *                                 cabile                    *
      *    *-----------------------------------------------------------*
       vis-snx-nmm-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           if        w-tes-snx-nmm (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-nmm (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-nmm-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Si/No numerazione controllata        *
      *    *-----------------------------------------------------------*
       acc-snx-nmc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-snx-nmc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      "SN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
      *
           if        w-tes-snx-nmc (1)    =    "S" or
                     w-tes-snx-nmc (1)    =    spaces
                     move  01             to   v-num
           else if   w-tes-snx-nmc (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-snx-nmc-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-nmc-999.
       acc-snx-nmc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   w-tes-snx-nmc (1)
           else if   v-num                =    02
                     move  "N"            to   w-tes-snx-nmc (1)
           else      move  spaces         to   w-tes-snx-nmc (1)      .
       acc-snx-nmc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-snx-nmc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-nmc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-nmc-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-nmc-100.
       acc-snx-nmc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Si/No numerazione controllata     *
      *    *-----------------------------------------------------------*
       vis-snx-nmc-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
      *
           if        w-tes-snx-nmc (1)    =    "S" or
                     w-tes-snx-nmc (1)    =    spaces
                     move  01             to   v-num
           else if   w-tes-snx-nmc (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-nmc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Numero copie da stampare     *
      *    *-----------------------------------------------------------*
       acc-num-cps-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Default                                     *
      *                  *---------------------------------------------*
           if        w-tes-num-cps (1)    =    zero
                     move  01             to   w-tes-num-cps (1)      .
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-num-cps (1)    to   w-sav-num-cps          .
       acc-num-cps-100.
      *              *-------------------------------------------------*
      *              * Note operative                                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "N.B.: significativo solo per 'bol300'"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-num-cps (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Cancellazione note operative attuali            *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      spaces               to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-num-cps-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-num-cps-999.
       acc-num-cps-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-num-cps (1)      .
       acc-num-cps-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : reimpostazione, a meno che non    *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        w-tes-num-cps (1)    not  = zero
                     go to acc-num-cps-500.
           if        v-key                =    "UP  "
                     go to acc-num-cps-600
           else      go to acc-num-cps-100.
       acc-num-cps-500.
      *                  *---------------------------------------------*
      *                  * Che non superi 5                            *
      *                  *---------------------------------------------*
           if        w-tes-num-cps (1)    >    5
                     go to acc-num-cps-100.
       acc-num-cps-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Si/no numero copie variabile                *
      *                  *---------------------------------------------*
           if        w-tes-num-cps (1)    >    2
                     go to acc-num-cps-800.
      *                  *---------------------------------------------*
      *                  * Normalizzazione numero copie variabile      *
      *                  *---------------------------------------------*
           move      "N"                  to   w-tes-snx-ncv (1)      .
           perform   vis-snx-ncv-000      thru vis-snx-ncv-999        .
       acc-num-cps-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-num-cps-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-num-cps-100.
       acc-num-cps-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Numero copie da stampare  *
      *    *-----------------------------------------------------------*
       vis-num-cps-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-num-cps (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-cps-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Si/No controllo stampa gia'  *
      *    *                              eseguita                     *
      *    *-----------------------------------------------------------*
       acc-snx-lib-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-snx-lib-100.
      *              *-------------------------------------------------*
      *              * Note operative                                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "N.B.: significativo solo per 'bol300'"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      "SN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      08                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           if        w-tes-snx-lib (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-lib (1)    =    spaces
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Cancellazione note operative attuali            *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      spaces               to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-snx-lib-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-lib-999.
       acc-snx-lib-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   w-tes-snx-lib (1)
           else if   v-num                =    02
                     move  spaces         to   w-tes-snx-lib (1)
           else      move  spaces         to   w-tes-snx-lib (1)      .
       acc-snx-lib-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-snx-lib-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-lib-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-lib-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-lib-100.
       acc-snx-lib-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Si/No controllo stampa    *
      *    *                                 gia' eseguita             *
      *    *-----------------------------------------------------------*
       vis-snx-lib-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      08                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           if        w-tes-snx-lib (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-lib (1)    =    spaces
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-lib-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Si/No numero copie variabile         *
      *    *-----------------------------------------------------------*
       acc-snx-ncv-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-num-cps (1)    not  > 02
                     go to acc-snx-ncv-999.
       acc-snx-ncv-100.
      *              *-------------------------------------------------*
      *              * Note operative                                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "Il numero copie dipende dall'indicatore del Traspo
      -              "rto a cura"         to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      "SN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           if        w-tes-snx-ncv (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-ncv (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Cancellazione note operative attuali            *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      spaces               to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-snx-ncv-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-ncv-999.
       acc-snx-ncv-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   w-tes-snx-ncv (1)
           else if   v-num                =    02
                     move  "N"            to   w-tes-snx-ncv (1)
           else      move  spaces         to   w-tes-snx-ncv (1)      .
       acc-snx-ncv-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-snx-ncv-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-ncv-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-ncv-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-ncv-100.
       acc-snx-ncv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Si/No numero copie variabile      *
      *    *-----------------------------------------------------------*
       vis-snx-ncv-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           if        w-tes-snx-ncv (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-ncv (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-ncv-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Si/No stampa prezzo          *
      *    *-----------------------------------------------------------*
       acc-snx-prz-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-snx-prz-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      "SN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-snx-prz (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-snx-prz-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-prz-999.
       acc-snx-prz-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-snx-prz (1)      .
       acc-snx-prz-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : reimpostazione, a meno che non    *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        w-tes-snx-prz (1)    not  = zero
                     go to acc-snx-prz-600.
           if        v-key                =    "UP  "
                     go to acc-snx-prz-600
           else      go to acc-snx-prz-100.
       acc-snx-prz-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-prz-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-prz-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-prz-100.
       acc-snx-prz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Si/No stampa prezzo       *
      *    *-----------------------------------------------------------*
       vis-snx-prz-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           move      w-tes-snx-prz (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-prz-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Si/No stampa sconti          *
      *    *-----------------------------------------------------------*
       acc-snx-sco-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-snx-sco-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      "SN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-snx-sco (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-snx-sco-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-sco-999.
       acc-snx-sco-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-snx-sco (1)      .
       acc-snx-sco-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : reimpostazione, a meno che non    *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        w-tes-snx-sco (1)    not  = zero
                     go to acc-snx-sco-600.
           if        v-key                =    "UP  "
                     go to acc-snx-sco-600
           else      go to acc-snx-sco-100.
       acc-snx-sco-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-sco-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-sco-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-sco-100.
       acc-snx-sco-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Si/No stampa sconti       *
      *    *-----------------------------------------------------------*
       vis-snx-sco-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           move      w-tes-snx-sco (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-sco-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Si/No stampa importo         *
      *    *-----------------------------------------------------------*
       acc-snx-imp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-snx-imp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      "SN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-snx-imp (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-snx-imp-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-imp-999.
       acc-snx-imp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-snx-imp (1)      .
       acc-snx-imp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : reimpostazione, a meno che non    *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        w-tes-snx-imp (1)    not  = zero
                     go to acc-snx-imp-600.
           if        v-key                =    "UP  "
                     go to acc-snx-imp-600
           else      go to acc-snx-imp-100.
       acc-snx-imp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-imp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-imp-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-imp-100.
       acc-snx-imp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Si/No stampa importo      *
      *    *-----------------------------------------------------------*
       vis-snx-imp-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           move      w-tes-snx-imp (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-imp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Si/No stampa codice iva      *
      *    *-----------------------------------------------------------*
       acc-snx-civ-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-snx-civ-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      "SN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      12                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-snx-civ (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-snx-civ-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-civ-999.
       acc-snx-civ-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-snx-civ (1)      .
       acc-snx-civ-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : reimpostazione, a meno che non    *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        w-tes-snx-civ (1)    not  = zero
                     go to acc-snx-civ-600.
           if        v-key                =    "UP  "
                     go to acc-snx-civ-600
           else      go to acc-snx-civ-100.
       acc-snx-civ-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-civ-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-civ-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-civ-100.
       acc-snx-civ-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Si/No stampa codice iva   *
      *    *-----------------------------------------------------------*
       vis-snx-civ-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      12                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           move      w-tes-snx-civ (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-civ-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Si/No stampa totale docu-    *
      *    * mento                                                     *
      *    *-----------------------------------------------------------*
       acc-snx-ttd-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-snx-ttd-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      "SN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      13                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-snx-ttd (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-snx-ttd-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-ttd-999.
       acc-snx-ttd-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-snx-ttd (1)      .
       acc-snx-ttd-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : reimpostazione, a meno che non    *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        w-tes-snx-ttd (1)    not  = zero
                     go to acc-snx-ttd-600.
           if        v-key                =    "UP  "
                     go to acc-snx-ttd-600
           else      go to acc-snx-ttd-100.
       acc-snx-ttd-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-ttd-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-ttd-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-ttd-100.
       acc-snx-ttd-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Si/No stampa totale docu- *
      *    * mento                                                     *
      *    *-----------------------------------------------------------*
       vis-snx-ttd-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      13                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           move      w-tes-snx-ttd (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-ttd-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Si/No stampa dicitura        *
      *    *-----------------------------------------------------------*
       acc-snx-dct-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        w-tes-snx-dct (1)    =    zero
                     move  02             to   w-tes-snx-dct (1)      .
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-snx-dct (1)    to   w-sav-snx-dct          .
       acc-snx-dct-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      "SN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      14                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-snx-dct (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-snx-dct-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-dct-999.
       acc-snx-dct-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-snx-dct (1)      .
       acc-snx-dct-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : reimpostazione, a meno che non    *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        w-tes-snx-dct (1)    not  = zero
                     go to acc-snx-dct-600.
           if        v-key                =    "UP  "
                     go to acc-snx-dct-600
           else      go to acc-snx-dct-100.
       acc-snx-dct-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore attuale come precedente : oltre   *
      *                  *---------------------------------------------*
           if        w-tes-snx-dct (1)    =    w-sav-snx-dct
                     go to acc-snx-dct-800.
      *                  *---------------------------------------------*
      *                  * Se No stampa dicitura                       *
      *                  *---------------------------------------------*
           if        w-tes-snx-dct (1)    =    01
                     go to acc-snx-dct-800.
      *                  *---------------------------------------------*
      *                  * Normalizzazione descrizione dicitura        *
      *                  *---------------------------------------------*
           if        w-tes-des-dct (1)    =    spaces
                     go to acc-snx-dct-800.
           move      spaces               to   w-tes-des-dct (1)      .
           perform   vis-des-dct-000      thru vis-des-dct-999        .
       acc-snx-dct-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-dct-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-dct-100.
       acc-snx-dct-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Si/No stampa dicitura     *
      *    *-----------------------------------------------------------*
       vis-snx-dct-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      14                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           move      w-tes-snx-dct (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-dct-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Descrizione dicitura         *
      *    *-----------------------------------------------------------*
       acc-des-dct-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-snx-dct (1)    not  = 01
                     go to acc-des-dct-999.
       acc-des-dct-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "T"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      04                   to   v-ldt                  .
           move      16                   to   v-lin                  .
           move      15                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-des-dct (1)    to   v-txt                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-des-dct-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-des-dct-999.
       acc-des-dct-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-txt                to   w-tes-des-dct (1)      .
       acc-des-dct-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se spaces : reimpostazione, a meno che non  *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        w-tes-des-dct (1)    not  = spaces
                     go to acc-des-dct-600.
           if        v-key                =    "UP  "
                     go to acc-des-dct-600
           else      go to acc-des-dct-100.
       acc-des-dct-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-des-dct-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-des-dct-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-des-dct-100.
       acc-des-dct-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Si/No stampa dicitura     *
      *    *-----------------------------------------------------------*
       vis-des-dct-000.
           move      "DS"                 to   v-ope                  .
           move      "T"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      04                   to   v-ldt                  .
           move      16                   to   v-lin                  .
           move      15                   to   v-pos                  .
           move      w-tes-des-dct (1)    to   v-txt                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-dct-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Si/No stampa Agente          *
      *    *-----------------------------------------------------------*
       acc-snx-age-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        w-tes-snx-age (1)    not  = spaces
                     go to acc-snx-age-100.
           move      "N"                  to   w-tes-snx-age (1)      .
       acc-snx-age-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-age-lun    to   v-car                  .
           move      w-exp-snx-age-num    to   v-ldt                  .
           move      spaces               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-age-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           if        w-tes-snx-age (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-age (1)    =    "N"
                     move  02             to   v-num
           else if   w-tes-snx-age (1)    =    "C"
                     move  03             to   v-num
           else if   w-tes-snx-age (1)    =    "T"
                     move  04             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-snx-age-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-age-999.
       acc-snx-age-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   w-tes-snx-age (1)
           else if   v-num                =    02
                     move  "N"            to   w-tes-snx-age (1)
           else if   v-num                =    03
                     move  "C"            to   w-tes-snx-age (1)
           else if   v-num                =    04
                     move  "T"            to   w-tes-snx-age (1)
           else      move  spaces         to   w-tes-snx-age (1)      .
       acc-snx-age-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se spaces : reimpostazione, a meno che non  *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        w-tes-snx-age (1)    not  = spaces
                     go to acc-snx-age-600.
           if        v-key                =    "UP  "
                     go to acc-snx-age-600
           else      go to acc-snx-age-100.
       acc-snx-age-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-age-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-age-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-age-100.
       acc-snx-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Si/No stampa Agente       *
      *    *-----------------------------------------------------------*
       vis-snx-age-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-age-lun    to   v-car                  .
           move      w-exp-snx-age-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-age-tbl    to   v-txt                  .
           if        w-tes-snx-age (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-age (1)    =    "N"
                     move  02             to   v-num
           else if   w-tes-snx-age (1)    =    "C"
                     move  03             to   v-num
           else if   w-tes-snx-age (1)    =    "T"
                     move  04             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-age-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Si/No stampa ns. dipendenza  *
      *    *-----------------------------------------------------------*
       acc-snx-ndp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        w-tes-snx-ndp (1)    not  = spaces
                     go to acc-snx-ndp-100.
           move      "N"                  to   w-tes-snx-ndp (1)      .
       acc-snx-ndp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      "SN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           if        w-tes-snx-ndp (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-ndp (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-snx-ndp-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-ndp-999.
       acc-snx-ndp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   w-tes-snx-ndp (1)
           else if   v-num                =    02
                     move  "N"            to   w-tes-snx-ndp (1)
           else      move  spaces         to   w-tes-snx-ndp (1)      .
       acc-snx-ndp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se spaces : reimpostazione, a meno che non  *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        w-tes-snx-ndp (1)    not  = spaces
                     go to acc-snx-ndp-600.
           if        v-key                =    "UP  "
                     go to acc-snx-ndp-600
           else      go to acc-snx-ndp-100.
       acc-snx-ndp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-ndp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-ndp-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-ndp-100.
       acc-snx-ndp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Si/No stampa ns. dipendenza       *
      *    *-----------------------------------------------------------*
       vis-snx-ndp-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           if        w-tes-snx-ndp (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-ndp (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-ndp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Si/No stampa forma di pagamento      *
      *    *-----------------------------------------------------------*
       acc-snx-fop-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-snx-fop-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      "SN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      21                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           if        w-tes-snx-fop (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-fop (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-snx-fop-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-fop-999.
       acc-snx-fop-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   w-tes-snx-fop (1)
           else if   v-num                =    02
                     move  "N"            to   w-tes-snx-fop (1)
           else      move  spaces         to   w-tes-snx-fop (1)      .
       acc-snx-fop-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se spaces : reimpostazione, a meno che non  *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        w-tes-snx-fop (1)    not  = spaces
                     go to acc-snx-fop-600.
           if        v-key                =    "UP  "
                     go to acc-snx-fop-600
           else      go to acc-snx-fop-100.
       acc-snx-fop-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-fop-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-fop-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-fop-100.
       acc-snx-fop-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Si/No stampa forma di pagamento   *
      *    *-----------------------------------------------------------*
       vis-snx-fop-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      21                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           if        w-tes-snx-fop (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-fop (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-fop-999.
           exit.

      *    *===========================================================*
      *    * Accettazione corpo a partire dalla riga con numero d'or-  *
      *    * dine : w-cnt-cor-nrg-dac                                  *
      *    *-----------------------------------------------------------*
       acc-cor-reg-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione tipo uscita da corpo            *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tus-acc-cor      .
      *              *-------------------------------------------------*
      *              * Confronto tra numero d'ordine riga da accettare *
      *              * e numero d'ordine dell'ultima riga              *
      *              *-------------------------------------------------*
           if        w-cnt-cor-nrg-dac    not  > w-cat-rig-max
                     go to acc-cor-reg-025.
      *              *-------------------------------------------------*
      *              * Se oltre numero riga massimo e Append non pos-  *
      *              * sibile si esce con status di uscita "+"         *
      *              *-------------------------------------------------*
           if        w-cat-rig-app        not  = spaces
                     move  "+"            to   w-cnt-tus-acc-cor
                     go to acc-cor-reg-990.
      *              *-------------------------------------------------*
      *              * Se oltre numero riga massimo e Append possibile *
      *              * si esegue l'operazione di Append                *
      *              *-------------------------------------------------*
           move      w-cat-rig-max        to   w-cnt-cor-nrg-dac      .
           add       1                    to   w-cnt-cor-nrg-dac      .
           move      "AP"                 to   w-cat-rig-ope          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
       acc-cor-reg-025.
      *              *-------------------------------------------------*
      *              * Video in 'OFF'                                  *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Prompts per corpo                               *
      *              *-------------------------------------------------*
           if        w-cnt-sts-pmt-cor    =    spaces
                     perform pmt-cor-reg-000
                                          thru pmt-cor-reg-999
                     move    "#"          to   w-cnt-sts-pmt-cor      .
      *              *-------------------------------------------------*
      *              * Visualizzazione dati corpo                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione nr pagina da visualizzare    *
      *                  *---------------------------------------------*
           move      w-cnt-cor-nrg-dac    to   w-cnt-cor-nrp-dav      .
           add       w-lin-num-lin-vis    to   w-cnt-cor-nrp-dav      .
           subtract  1                    from w-cnt-cor-nrp-dav      .
           divide    w-lin-num-lin-vis    into w-cnt-cor-nrp-dav      .
      *                  *---------------------------------------------*
      *                  * Se non e' visualizzata nessuna pagina corpo *
      *                  * o una pagina corpo diversa da quella da vi- *
      *                  * sualizzare si procede alla visualizzazione  *
      *                  *---------------------------------------------*
           if        w-cnt-sts-vis-cor    =    spaces           or
                     w-cnt-cor-nrp-vis    not  = w-cnt-cor-nrp-dav
                     perform vis-cor-reg-000
                                          thru vis-cor-reg-999
                     move    "#"          to   w-cnt-sts-vis-cor
                     move    w-cnt-cor-nrp-dav
                                          to   w-cnt-cor-nrp-vis      .
      *              *-------------------------------------------------*
      *              * Lettura riga da catena                          *
      *              *-------------------------------------------------*
           move      "RD"                 to   w-cat-rig-ope          .
           move      w-cnt-cor-nrg-dac    to   w-cat-rig-num          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
           move      w-cat-rig-buf        to   w-rig                  .
      *              *-------------------------------------------------*
      *              * Se riga nuova si normalizza il corpo            *
      *              *-------------------------------------------------*
           if        w-cat-rig-new        =    spaces
                     go to acc-cor-reg-050.
           perform   nor-nok-rig-000      thru nor-nok-rig-999        .
           move      w-rig-val-aep (1)    to   w-rig-val-aep (2)      .
       acc-cor-reg-050.
      *              *-------------------------------------------------*
      *              * Routine pre-accettazione riga corpo             *
      *              *-------------------------------------------------*
           perform   pre-rig-cor-000      thru pre-rig-cor-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts per riga d'accettazione *
      *              *-------------------------------------------------*
           perform   pmt-rig-cor-000      thru pmt-rig-cor-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione dati per riga di accettazione   *
      *              *-------------------------------------------------*
           perform   vis-rig-cor-000      thru vis-rig-cor-999        .
      *              *-------------------------------------------------*
      *              * Video in 'ON'                                   *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Flag di status impostazione corpo               *
      *              *                                                 *
      *              * N.B. : si accettano anche zero righe corpo      *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-cor      .
      *              *-------------------------------------------------*
      *              * Flag di status impostione riga attuale          *
      *              *-------------------------------------------------*
           if        w-cat-rig-new        =    spaces
                     move  "#"            to   w-cnt-sts-imp-rig
           else      move  spaces         to   w-cnt-sts-imp-rig      .
      *              *-------------------------------------------------*
      *              * Indicatore linea in corso di trattamento        *
      *              *-------------------------------------------------*
           move      "I"                  to   w-cnt-cor-tvi-rig      .
           move      w-cnt-cor-nrg-dac    to   w-cnt-cor-nrg-dav      .
           perform   vis-lin-cor-000      thru vis-lin-cor-999        .
      *              *-------------------------------------------------*
      *              * Normalizzazione tipo uscita da riga corpo       *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tus-acc-rig      .
       acc-cor-reg-100.
      *              *-------------------------------------------------*
      *              * Accettazioni                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo impostazione : primo campo riga corpo  *
      *                  *---------------------------------------------*
           move      "C"                  to   w-cnt-mfu-tip-imp      .
      *                  *---------------------------------------------*
      *                  * Accettazione codice dipendenza              *
      *                  *---------------------------------------------*
           perform   acc-cod-dpz-000      thru acc-cod-dpz-999        .
           if        w-cnt-tus-acc-rig    not  = spaces
                     go to acc-cor-reg-925.
      *                  *---------------------------------------------*
      *                  * Tipo impostazione : altri campi riga corpo  *
      *                  *---------------------------------------------*
           move      "R"                  to   w-cnt-mfu-tip-imp      .
       acc-cor-reg-200.
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Accettazione codice ubicazione              *
      *                  *---------------------------------------------*
           perform   acc-cod-dsl-000      thru acc-cod-dsl-999        .
           if        w-cnt-tus-acc-rig    not  = spaces
                     go to acc-cor-reg-900.
       acc-cor-reg-800.
      *              *-------------------------------------------------*
      *              * Controllo finale su riga impostata              *
      *              *-------------------------------------------------*
           perform   cnt-tdo-rig-000      thru cnt-tdo-rig-999        .
           if        w-cnt-tdo-rig-flg    not  = spaces
                     move  spaces         to   w-cnt-tdo-rig-flg
                     move  "UP  "         to   v-key
                     go to acc-cor-reg-200.
       acc-cor-reg-900.
      *              *-------------------------------------------------*
      *              * Tests per uscita significativa da interno riga  *
      *              *-------------------------------------------------*
           if        w-cnt-tus-acc-rig    =    spaces or
                     w-cnt-tus-acc-rig    =    "S"
                     go to acc-cor-reg-905
           else if   w-cnt-tus-acc-rig    =    "R"
                     go to acc-cor-reg-910.
      *                  *---------------------------------------------*
      *                  * Se Exit da interno riga                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se riga New funzionamento come per Remv *
      *                      *-----------------------------------------*
           if        w-cat-rig-new        not  = spaces
                     go to acc-cor-reg-915.
      *                      *-----------------------------------------*
      *                      * Se riga non New                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Visualizzazione numero riga         *
      *                          *-------------------------------------*
           move      "N"                  to   w-cnt-cor-tvi-rig      .
           move      w-cnt-cor-nrg-dac    to   w-cnt-cor-nrg-dav      .
           perform   vis-lin-cor-000      thru vis-lin-cor-999        .
           go to     acc-cor-reg-000.
       acc-cor-reg-905.
      *                  *---------------------------------------------*
      *                  * Se Do da interno riga                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se record di tipo New attribuzione del  *
      *                      * numero progressivo per la riga          *
      *                      *-----------------------------------------*
           if        w-cat-rig-new        =    spaces
                     go to acc-cor-reg-907.
           perform   prg-rig-cor-000      thru prg-rig-cor-999        .
       acc-cor-reg-907.
      *                      *-----------------------------------------*
      *                      * Routine post-accettazione riga          *
      *                      *-----------------------------------------*
           move      "+"                  to   w-cnt-cor-tip-agg      .
           perform   pos-rig-cor-000      thru pos-rig-cor-999        .
      *                      *-----------------------------------------*
      *                      * Update riga in catena movimenti         *
      *                      *-----------------------------------------*
           move      "UP"                 to   w-cat-rig-ope          .
           move      w-cnt-cor-nrg-dac    to   w-cat-rig-num          .
           move      w-rig                to   w-cat-rig-buf          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione linea corpo             *
      *                      *-----------------------------------------*
           move      "R"                  to   w-cnt-cor-tvi-rig      .
           move      w-cnt-cor-nrg-dac    to   w-cnt-cor-nrg-dav      .
           perform   vis-lin-cor-000      thru vis-lin-cor-999        .
      *                      *-----------------------------------------*
      *                      * Incremento del numero d'ordine della    *
      *                      * riga da accettare                       *
      *                      *-----------------------------------------*
           add       1                    to   w-cnt-cor-nrg-dac      .
           go to     acc-cor-reg-000.
       acc-cor-reg-910.
      *                  *---------------------------------------------*
      *                  * Se Remv da interno riga                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se riga non new : routine post-accetta- *
      *                      * zione riga                              *
      *                      *-----------------------------------------*
           if        w-cat-rig-new        not  = spaces
                     go to acc-cor-reg-915.
           move      "-"                  to   w-cnt-cor-tip-agg      .
           perform   pos-rig-cor-000      thru pos-rig-cor-999        .
       acc-cor-reg-915.
      *                      *-----------------------------------------*
      *                      * Visualizzazione numero riga a spaces se *
      *                      * si e' in Append                         *
      *                      *-----------------------------------------*
           if        w-cat-rig-new        not  = spaces and
                     w-cat-rig-lst        not  = spaces
                     move    "S"          to   w-cnt-cor-tvi-rig
                     move    w-cnt-cor-nrg-dac
                                          to   w-cnt-cor-nrg-dav
                     perform vis-lin-cor-000
                                          thru vis-lin-cor-999        .
      *                      *-----------------------------------------*
      *                      * Numero pagina attualmente  visualizzata *
      *                      * a zero per forzare la rivisualizzazione *
      *                      * a meno che non si sia in Append         *
      *                      *-----------------------------------------*
           if        w-cat-rig-new        =    spaces or
                     w-cat-rig-lst        =    spaces
                     move  zero           to   w-cnt-cor-nrp-vis      .
      *                      *-----------------------------------------*
      *                      * Rimozione record dalla catena           *
      *                      *-----------------------------------------*
           move      "RM"                 to   w-cat-rig-ope          .
           move      w-cnt-cor-nrg-dac    to   w-cat-rig-num          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
      *                      *-----------------------------------------*
      *                      * Segnale di almeno una modifica          *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-acc-flg-aum      .
           go to     acc-cor-reg-000.
       acc-cor-reg-925.
      *              *-------------------------------------------------*
      *              * Tests per uscita significativa dal primo campo  *
      *              * di impostazione della riga                      *
      *              *-------------------------------------------------*
           if        w-cnt-tus-acc-rig    =    "U"
                     go to acc-cor-reg-935
           else if   w-cnt-tus-acc-rig    =    "D"
                     go to acc-cor-reg-940
           else if   w-cnt-tus-acc-rig    =    "S"
                     go to acc-cor-reg-945
           else if   w-cnt-tus-acc-rig    =    "X"
                     go to acc-cor-reg-950
           else if   w-cnt-tus-acc-rig    =    "I"
                     go to acc-cor-reg-955
           else if   w-cnt-tus-acc-rig    =    "R"
                     go to acc-cor-reg-960
           else if   w-cnt-tus-acc-rig    =    "T"
                     go to acc-cor-reg-965
           else if   w-cnt-tus-acc-rig    =    "B"
                     go to acc-cor-reg-970
           else if   w-cnt-tus-acc-rig    =    "N"
                     go to acc-cor-reg-975
           else if   w-cnt-tus-acc-rig    =    "P"
                     go to acc-cor-reg-980
           else if   w-cnt-tus-acc-rig    =    "."
                     go to acc-cor-reg-985.
       acc-cor-reg-930.
      *                  *---------------------------------------------*
      *                  * Se Exit da primo campo di impostazione riga *
      *                  *---------------------------------------------*
           move      "E"                  to   w-cnt-tus-acc-cor      .
           go to     acc-cor-reg-990.
       acc-cor-reg-935.
      *                  *---------------------------------------------*
      *                  * Se Up da primo campo di impostazione riga   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se record New                           *
      *                      *-----------------------------------------*
           if        w-cat-rig-new        =    spaces
                     go to acc-cor-reg-936.
      *                          *-------------------------------------*
      *                          * Visualizzazione del numero riga a   *
      *                          * spaces se si e' in Append           *
      *                          *-------------------------------------*
           if        w-cat-rig-new        not  = spaces and
                     w-cat-rig-lst        not  = spaces
                     move    "S"          to   w-cnt-cor-tvi-rig
                     move    w-cnt-cor-nrg-dac
                                          to   w-cnt-cor-nrg-dav
                     perform vis-lin-cor-000
                                          thru vis-lin-cor-999        .
      *                          *-------------------------------------*
      *                          * Numero pagina attualmente visualiz- *
      *                          * zata a zero per forzare la rivisua- *
      *                          * lizzazione,  a meno che non si sia  *
      *                          * in Append                           *
      *                          *-------------------------------------*
           if        w-cat-rig-new        =    spaces or
                     w-cat-rig-lst        =    spaces
                     move  zero           to   w-cnt-cor-nrp-vis      .
      *                          *-------------------------------------*
      *                          * Rimozione record dalla catena       *
      *                          *-------------------------------------*
           move      "RM"                 to   w-cat-rig-ope          .
           move      w-cnt-cor-nrg-dac    to   w-cat-rig-num          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
           go to     acc-cor-reg-937.
       acc-cor-reg-936.
      *                      *-----------------------------------------*
      *                      * Se record non New                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Visualizzazione del numero riga     *
      *                          *-------------------------------------*
           move      "N"                  to   w-cnt-cor-tvi-rig      .
           move      w-cnt-cor-nrg-dac    to   w-cnt-cor-nrg-dav      .
           perform   vis-lin-cor-000      thru vis-lin-cor-999        .
       acc-cor-reg-937.
      *                          *-------------------------------------*
      *                          * Se si e' sulla riga numero 1        *
      *                          *-------------------------------------*
           if        w-cnt-cor-nrg-dac    =    1
                     move  "-"            to   w-cnt-tus-acc-cor
                     go to acc-cor-reg-990.
      *                          *-------------------------------------*
      *                          * Se non si e' sulla riga numero 1    *
      *                          *-------------------------------------*
           subtract  1                    from w-cnt-cor-nrg-dac      .
           go to     acc-cor-reg-000.
       acc-cor-reg-940.
      *                  *---------------------------------------------*
      *                  * Se Down da primo campo di impostazione riga *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se record New                           *
      *                      *-----------------------------------------*
           if        w-cat-rig-new        =    spaces
                     go to acc-cor-reg-941.
      *                          *-------------------------------------*
      *                          * Se si e' in Append si prepara il    *
      *                          * tipo uscita "+" e si visualizza il  *
      *                          * numero riga a spaces                *
      *                          *-------------------------------------*
           if        w-cat-rig-new        not  = spaces and
                     w-cat-rig-lst        not  = spaces
                     move    "+"          to   w-cnt-tus-acc-cor
                     move    "S"          to   w-cnt-cor-tvi-rig
                     move    w-cnt-cor-nrg-dac
                                          to   w-cnt-cor-nrg-dav
                     perform vis-lin-cor-000
                                          thru vis-lin-cor-999
      *                          *-------------------------------------*
      *                          * Altrimenti si prepara il numero pa- *
      *                          * gina attualmente visualizzata a ze- *
      *                          * ro per forzare la rivisualizzazione *
      *                          *-------------------------------------*
           else      move  zero           to   w-cnt-cor-nrp-vis      .
      *                          *-------------------------------------*
      *                          * Rimozione record dalla catena       *
      *                          *-------------------------------------*
           move      "RM"                 to   w-cat-rig-ope          .
           move      w-cnt-cor-nrg-dac    to   w-cat-rig-num          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
      *                          *-------------------------------------*
      *                          * Se si era in Append : uscita        *
      *                          *-------------------------------------*
           if        w-cnt-tus-acc-cor    =    "+"
                     go to acc-cor-reg-990
      *                          *-------------------------------------*
      *                          * Altrimenti : riciclo                *
      *                          *-------------------------------------*
           else      go to acc-cor-reg-000.
       acc-cor-reg-941.
      *                      *-----------------------------------------*
      *                      * Se record non New                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Visualizzazione del numero riga     *
      *                          *-------------------------------------*
           move      "N"                  to   w-cnt-cor-tvi-rig      .
           move      w-cnt-cor-nrg-dac    to   w-cnt-cor-nrg-dav      .
           perform   vis-lin-cor-000      thru vis-lin-cor-999        .
      *                          *-------------------------------------*
      *                          * Incremento del numero riga          *
      *                          *-------------------------------------*
           add       1                    to   w-cnt-cor-nrg-dac      .
           go to     acc-cor-reg-000.
       acc-cor-reg-945.
      *                  *---------------------------------------------*
      *                  * Se Do da primo campo di impostazione riga   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se record New                           *
      *                      *-----------------------------------------*
           if        w-cat-rig-new        =    spaces
                     go to acc-cor-reg-946.
      *                          *-------------------------------------*
      *                          * Visualizzazione del numero riga a   *
      *                          * spaces                              *
      *                          *-------------------------------------*
           move      "S"                  to   w-cnt-cor-tvi-rig      .
           move      w-cnt-cor-nrg-dac    to   w-cnt-cor-nrg-dav      .
           perform   vis-lin-cor-000      thru vis-lin-cor-999        .
      *                          *-------------------------------------*
      *                          * Rimozione record dalla catena       *
      *                          *-------------------------------------*
           move      "RM"                 to   w-cat-rig-ope          .
           move      w-cnt-cor-nrg-dac    to   w-cat-rig-num          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
       acc-cor-reg-946.
      *                      *-----------------------------------------*
      *                      * Se record non New                       *
      *                      *-----------------------------------------*
           move      "S"                  to   w-cnt-tus-acc-cor      .
           go to     acc-cor-reg-990.
       acc-cor-reg-950.
      *                  *---------------------------------------------*
      *                  * Se Delt da primo campo di impostazione riga *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se record New                           *
      *                      *-----------------------------------------*
           if        w-cat-rig-new        =    spaces
                     go to acc-cor-reg-951.
      *                          *-------------------------------------*
      *                          * Visualizzazione del numero riga a   *
      *                          * spaces                              *
      *                          *-------------------------------------*
           move      "S"                  to   w-cnt-cor-tvi-rig      .
           move      w-cnt-cor-nrg-dac    to   w-cnt-cor-nrg-dav      .
           perform   vis-lin-cor-000      thru vis-lin-cor-999        .
      *                          *-------------------------------------*
      *                          * Rimozione record dalla catena       *
      *                          *-------------------------------------*
           move      "RM"                 to   w-cat-rig-ope          .
           move      w-cnt-cor-nrg-dac    to   w-cat-rig-num          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
       acc-cor-reg-951.
      *                      *-----------------------------------------*
      *                      * Se record non New                       *
      *                      *-----------------------------------------*
           move      "X"                  to   w-cnt-tus-acc-cor      .
           go to     acc-cor-reg-990.
       acc-cor-reg-955.
      *                  *---------------------------------------------*
      *                  * Se Insr da primo campo di impostazione riga *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Inserimento record nella catena         *
      *                      *-----------------------------------------*
           move      "IN"                 to   w-cat-rig-ope          .
           move      w-cnt-cor-nrg-dac    to   w-cat-rig-num          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
      *                      *-----------------------------------------*
      *                      * Numero pagina attualmente  visualizzata *
      *                      * a zero per forzare la rivisualizzazione *
      *                      *-----------------------------------------*
           move      zero                 to   w-cnt-cor-nrp-vis      .
           go to     acc-cor-reg-000.
       acc-cor-reg-960.
      *                  *---------------------------------------------*
      *                  * Se Remv da primo campo di impostazione riga *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se riga non new : routine post-accetta- *
      *                      * zione riga                              *
      *                      *-----------------------------------------*
           if        w-cat-rig-new        not  = spaces
                     go to acc-cor-reg-962.
           move      "-"                  to   w-cnt-cor-tip-agg      .
           perform   pos-rig-cor-000      thru pos-rig-cor-999        .
       acc-cor-reg-962.
      *                      *-----------------------------------------*
      *                      * Se in Append : visualizzazione del nu-  *
      *                      * mero riga a spaces                      *
      *                      *-----------------------------------------*
           if        w-cat-rig-new        not  = spaces and
                     w-cat-rig-lst        not  = spaces
                     move    "S"          to   w-cnt-cor-tvi-rig
                     move    w-cnt-cor-nrg-dac
                                          to   w-cnt-cor-nrg-dav
                     perform vis-lin-cor-000
                                          thru vis-lin-cor-999
      *                      *-----------------------------------------*
      *                      * Altrimenti si prepara il numero pagina  *
      *                      * attualmente visualizzata a zero per     *
      *                      * forzare la rivisualizzazione            *
      *                      *-----------------------------------------*
           else      move  zero           to   w-cnt-cor-nrp-vis      .
      *                      *-----------------------------------------*
      *                      * Rimozione record dalla catena           *
      *                      *-----------------------------------------*
           move      "RM"                 to   w-cat-rig-ope          .
           move      w-cnt-cor-nrg-dac    to   w-cat-rig-num          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
      *                      *-----------------------------------------*
      *                      * Segnale di almeno una modifica          *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-acc-flg-aum      .
           go to     acc-cor-reg-000.
       acc-cor-reg-965.
      *                  *---------------------------------------------*
      *                  * Se Tab da primo campo di impostazione riga  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se record New                           *
      *                      *-----------------------------------------*
           if        w-cat-rig-new        =    spaces
                     go to acc-cor-reg-966.
      *                          *-------------------------------------*
      *                          * Visualizzazione del numero riga a   *
      *                          * spaces                              *
      *                          *-------------------------------------*
           move      "S"                  to   w-cnt-cor-tvi-rig      .
           move      w-cnt-cor-nrg-dac    to   w-cnt-cor-nrg-dav      .
           perform   vis-lin-cor-000      thru vis-lin-cor-999        .
      *                          *-------------------------------------*
      *                          * Numero pagina attualmente visualiz- *
      *                          * zata a zero per forzare la rivisua- *
      *                          * lizzazione a meno che non si sia in *
      *                          * Append                              *
      *                          *-------------------------------------*
           if        w-cat-rig-new        =    spaces or
                     w-cat-rig-lst        =    spaces
                     move  zero           to   w-cnt-cor-nrp-vis      .
      *                          *-------------------------------------*
      *                          * Rimozione record dalla catena       *
      *                          *-------------------------------------*
           move      "RM"                 to   w-cat-rig-ope          .
           move      w-cnt-cor-nrg-dac    to   w-cat-rig-num          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
           go to     acc-cor-reg-967.
       acc-cor-reg-966.
      *                      *-----------------------------------------*
      *                      * Se record non New                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Visualizzazione del numero riga     *
      *                          *-------------------------------------*
           move      "N"                  to   w-cnt-cor-tvi-rig      .
           move      w-cnt-cor-nrg-dac    to   w-cnt-cor-nrg-dav      .
           perform   vis-lin-cor-000      thru vis-lin-cor-999        .
       acc-cor-reg-967.
      *                          *-------------------------------------*
      *                          * Ad accettazione max + 1             *
      *                          *-------------------------------------*
           move      w-cat-rig-max        to   w-cnt-cor-nrg-dac      .
           if        w-cat-rig-app        =    spaces
                     add   1              to   w-cnt-cor-nrg-dac      .
           go to     acc-cor-reg-000.
       acc-cor-reg-970.
      *                  *---------------------------------------------*
      *                  * Se Back da primo campo di impostazione riga *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se record New                           *
      *                      *-----------------------------------------*
           if        w-cat-rig-new        =    spaces
                     go to acc-cor-reg-971.
      *                          *-------------------------------------*
      *                          * Visualizzazione del numero riga a   *
      *                          * spaces                              *
      *                          *-------------------------------------*
           move      "S"                  to   w-cnt-cor-tvi-rig      .
           move      w-cnt-cor-nrg-dac    to   w-cnt-cor-nrg-dav      .
           perform   vis-lin-cor-000      thru vis-lin-cor-999        .
      *                          *-------------------------------------*
      *                          * Numero pagina attualmente visualiz- *
      *                          * zata a zero per forzare la rivisua- *
      *                          * lizzazione a meno che non si sia in *
      *                          * Append                              *
      *                          *-------------------------------------*
           if        w-cat-rig-new        =    spaces or
                     w-cat-rig-lst        =    spaces
                     move  zero           to   w-cnt-cor-nrp-vis      .
      *                          *-------------------------------------*
      *                          * Rimozione record dalla catena       *
      *                          *-------------------------------------*
           move      "RM"                 to   w-cat-rig-ope          .
           move      w-cnt-cor-nrg-dac    to   w-cat-rig-num          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
           move      1                    to   w-cnt-cor-nrg-dac      .
           go to     acc-cor-reg-000.
       acc-cor-reg-971.
      *                      *-----------------------------------------*
      *                      * Se record non New                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Visualizzazione del numero riga     *
      *                          *-------------------------------------*
           move      "N"                  to   w-cnt-cor-tvi-rig      .
           move      w-cnt-cor-nrg-dac    to   w-cnt-cor-nrg-dav      .
           perform   vis-lin-cor-000      thru vis-lin-cor-999        .
           move      1                    to   w-cnt-cor-nrg-dac      .
           go to     acc-cor-reg-000.
       acc-cor-reg-975.
      *                  *---------------------------------------------*
      *                  * Se Nxsc da primo campo di impostazione riga *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se record New                           *
      *                      *-----------------------------------------*
           if        w-cat-rig-new        =    spaces
                     go to acc-cor-reg-976.
      *                          *-------------------------------------*
      *                          * Visualizzazione del numero riga a   *
      *                          * spaces                              *
      *                          *-------------------------------------*
           move      "S"                  to   w-cnt-cor-tvi-rig      .
           move      w-cnt-cor-nrg-dac    to   w-cnt-cor-nrg-dav      .
           perform   vis-lin-cor-000      thru vis-lin-cor-999        .
      *                          *-------------------------------------*
      *                          * Numero pagina attualmente visualiz- *
      *                          * zata a zero per forzare la rivisua- *
      *                          * lizzazione a meno che non si sia in *
      *                          * Append                              *
      *                          *-------------------------------------*
           if        w-cat-rig-new        =    spaces or
                     w-cat-rig-lst        =    spaces
                     move  zero           to   w-cnt-cor-nrp-vis      .
      *                          *-------------------------------------*
      *                          * Rimozione record dalla catena       *
      *                          *-------------------------------------*
           move      "RM"                 to   w-cat-rig-ope          .
           move      w-cnt-cor-nrg-dac    to   w-cat-rig-num          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
           go to     acc-cor-reg-977.
       acc-cor-reg-976.
      *                      *-----------------------------------------*
      *                      * Se record non New                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Visualizzazione del numero riga     *
      *                          *-------------------------------------*
           move      "N"                  to   w-cnt-cor-tvi-rig      .
           move      w-cnt-cor-nrg-dac    to   w-cnt-cor-nrg-dav      .
           perform   vis-lin-cor-000      thru vis-lin-cor-999        .
       acc-cor-reg-977.
      *                          *-------------------------------------*
      *                          * Se si era nell'ultima riga, ed essa *
      *                          * era in Append : tipo uscita "+" e   *
      *                          * si esce                             *
      *                          *-------------------------------------*
           if        w-cnt-cor-nrg-dac    >    w-cat-rig-max
                     move    "+"          to   w-cnt-tus-acc-cor
                     go to acc-cor-reg-990.
      *                          *-------------------------------------*
      *                          * Determinazione numero d'ordine del- *
      *                          * la prima riga appartenente alla pa- *
      *                          * gina (max + 1)                      *
      *                          *-------------------------------------*
           move      w-cat-rig-max        to   w-cnt-wrk-ctr-001      .
           add       w-lin-num-lin-vis    to   w-cnt-wrk-ctr-001      .
           subtract  1                    from w-cnt-wrk-ctr-001      .
           divide    w-lin-num-lin-vis    into w-cnt-wrk-ctr-001      .
           multiply  w-lin-num-lin-vis    by   w-cnt-wrk-ctr-001      .
           add       1                    to   w-cnt-wrk-ctr-001      .
      *                          *-------------------------------------*
      *                          * Determinazione numero d'ordine del- *
      *                          * la prima riga appartenente alla pa- *
      *                          * gina (attuale + 1)                  *
      *                          *-------------------------------------*
           move      w-cnt-cor-nrg-dac    to   w-cnt-wrk-ctr-002      .
           add       w-lin-num-lin-vis    to   w-cnt-wrk-ctr-002      .
           subtract  1                    from w-cnt-wrk-ctr-002      .
           divide    w-lin-num-lin-vis    into w-cnt-wrk-ctr-002      .
           multiply  w-lin-num-lin-vis    by   w-cnt-wrk-ctr-002      .
           add       1                    to   w-cnt-wrk-ctr-002      .
      *                          *-------------------------------------*
      *                          * Se la prossima pagina e' sicuramen- *
      *                          * te prima dell'ultima                *
      *                          *-------------------------------------*
           if        w-cnt-wrk-ctr-002    <    w-cnt-wrk-ctr-001
                     move  w-cnt-wrk-ctr-002
                                          to   w-cnt-cor-nrg-dac
                     go to acc-cor-reg-000.
      *                          *-------------------------------------*
      *                          * Se si e' sull'ultima pagina la qua- *
      *                          * le e' pero' completamente piena     *
      *                          *-------------------------------------*
           subtract  1                    from w-cnt-wrk-ctr-001      .
           if        w-cat-rig-max        =    w-cnt-wrk-ctr-001
                     move  w-cnt-wrk-ctr-002
                                          to   w-cnt-cor-nrg-dac
                     go to acc-cor-reg-000.
      *                          *-------------------------------------*
      *                          * Se si supera l'ultima pagina        *
      *                          *-------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-cor      .
           go to     acc-cor-reg-990.
       acc-cor-reg-980.
      *                  *---------------------------------------------*
      *                  * Se Prsc da primo campo di impostazione riga *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se record New                           *
      *                      *-----------------------------------------*
           if        w-cat-rig-new        =    spaces
                     go to acc-cor-reg-981.
      *                          *-------------------------------------*
      *                          * Visualizzazione del numero riga a   *
      *                          * spaces                              *
      *                          *-------------------------------------*
           move      "S"                  to   w-cnt-cor-tvi-rig      .
           move      w-cnt-cor-nrg-dac    to   w-cnt-cor-nrg-dav      .
           perform   vis-lin-cor-000      thru vis-lin-cor-999        .
      *                          *-------------------------------------*
      *                          * Numero pagina attualmente visualiz- *
      *                          * zata a zero per forzare la rivisua- *
      *                          * lizzazione a meno che non si sia in *
      *                          * Append                              *
      *                          *-------------------------------------*
           if        w-cat-rig-new        =    spaces or
                     w-cat-rig-lst        =    spaces
                     move  zero           to   w-cnt-cor-nrp-vis      .
      *                          *-------------------------------------*
      *                          * Rimozione record dalla catena       *
      *                          *-------------------------------------*
           move      "RM"                 to   w-cat-rig-ope          .
           move      w-cnt-cor-nrg-dac    to   w-cat-rig-num          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
           go to     acc-cor-reg-982.
       acc-cor-reg-981.
      *                      *-----------------------------------------*
      *                      * Se record non New                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Visualizzazione del numero riga     *
      *                          *-------------------------------------*
           move      "N"                  to   w-cnt-cor-tvi-rig      .
           move      w-cnt-cor-nrg-dac    to   w-cnt-cor-nrg-dav      .
           perform   vis-lin-cor-000      thru vis-lin-cor-999        .
       acc-cor-reg-982.
      *                          *-------------------------------------*
      *                          * Se si e' sulla prima pagina         *
      *                          *-------------------------------------*
           if        w-cnt-cor-nrg-dac    not  > w-lin-num-lin-vis
                     move  "-"            to   w-cnt-tus-acc-cor
                     go to acc-cor-reg-990.
      *                          *-------------------------------------*
      *                          * Se non si e' sulla prima pagina     *
      *                          *-------------------------------------*
           subtract  w-lin-num-lin-vis    from w-cnt-cor-nrg-dac      .
           subtract  1                    from w-cnt-cor-nrg-dac      .
           divide    w-lin-num-lin-vis    into w-cnt-cor-nrg-dac      .
           multiply  w-lin-num-lin-vis    by   w-cnt-cor-nrg-dac      .
           add       1                    to   w-cnt-cor-nrg-dac      .
           go to     acc-cor-reg-000.
       acc-cor-reg-985.
      *                  *---------------------------------------------*
      *                  * Se Slct da primo campo di impostazione riga *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se record New                           *
      *                      *-----------------------------------------*
           if        w-cat-rig-new        =    spaces
                     go to acc-cor-reg-986.
      *                          *-------------------------------------*
      *                          * Visualizzazione del numero riga a   *
      *                          * spaces                              *
      *                          *-------------------------------------*
           move      "S"                  to   w-cnt-cor-tvi-rig      .
           move      w-cnt-cor-nrg-dac    to   w-cnt-cor-nrg-dav      .
           perform   vis-lin-cor-000      thru vis-lin-cor-999        .
      *                          *-------------------------------------*
      *                          * Numero pagina attualmente visualiz- *
      *                          * zata a zero per forzare la rivisua- *
      *                          * lizzazione a meno che non si sia in *
      *                          * Append                              *
      *                          *-------------------------------------*
           if        w-cat-rig-new        =    spaces or
                     w-cat-rig-lst        =    spaces
                     move  zero           to   w-cnt-cor-nrp-vis      .
      *                          *-------------------------------------*
      *                          * Rimozione record dalla catena       *
      *                          *-------------------------------------*
           move      "RM"                 to   w-cat-rig-ope          .
           move      w-cnt-cor-nrg-dac    to   w-cat-rig-num          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
      *                          *-------------------------------------*
      *                          * Se il numero riga di destinazione   *
      *                          * risulta oltre il numero d'ordine    *
      *                          * attuale si sottrae 1 al numero ri-  *
      *                          * ga di destinazione                  *
      *                          *-------------------------------------*
           if        w-cnt-slc-num-rig    >    w-cnt-cor-nrg-dac
                     subtract  1          from w-cnt-slc-num-rig      .
           go to     acc-cor-reg-987.
       acc-cor-reg-986.
      *                      *-----------------------------------------*
      *                      * Se record non New                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Visualizzazione del numero riga     *
      *                          *-------------------------------------*
           move      "N"                  to   w-cnt-cor-tvi-rig      .
           move      w-cnt-cor-nrg-dac    to   w-cnt-cor-nrg-dav      .
           perform   vis-lin-cor-000      thru vis-lin-cor-999        .
       acc-cor-reg-987.
      *                          *-------------------------------------*
      *                          * Ad accettazione nr riga selezionato *
      *                          *-------------------------------------*
           move      w-cnt-slc-num-rig    to   w-cnt-cor-nrg-dac      .
           go to     acc-cor-reg-000.
       acc-cor-reg-990.
      *              *-------------------------------------------------*
      *              * Assestamento status di impostazione corpo       *
      *              *                                                 *
      *              * N.B. : si accettano anche zero righe corpo      *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-cor      .
       acc-cor-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione pagina corpo numero : w-cnt-cor-nrp-dav   *
      *    *-----------------------------------------------------------*
       vis-cor-reg-000.
      *              *-------------------------------------------------*
      *              * Erase linee impegnate, escluse fincature        *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      08                   to   v-lin                  .
           move      18                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-cor-reg-150.
      *              *-------------------------------------------------*
      *              * Determinazione numero d'ordine del primo numero *
      *              * riga da visualizzare                            *
      *              *-------------------------------------------------*
           move      w-cnt-cor-nrp-dav    to   w-cnt-wrk-ctr-008      .
           multiply  w-lin-num-lin-vis    by   w-cnt-wrk-ctr-008      .
           add       1                    to   w-cnt-wrk-ctr-008      .
           subtract  w-lin-num-lin-vis    from w-cnt-wrk-ctr-008      .
      *              *-------------------------------------------------*
      *              * Determinazione numero d'ordine dell'ultimo nu-  *
      *              * mero riga da visualizzare                       *
      *              *-------------------------------------------------*
           move      w-cnt-wrk-ctr-008    to   w-cnt-wrk-ctr-009      .
           add       w-lin-num-lin-vis    to   w-cnt-wrk-ctr-009      .
           subtract  1                    from w-cnt-wrk-ctr-009      .
      *              *-------------------------------------------------*
      *              * Operazione di Start su catena movimenti         *
      *              *-------------------------------------------------*
           move      "ST"                 to   w-cat-rig-ope          .
           move      w-cnt-wrk-ctr-008    to   w-cat-rig-num          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
      *                  *---------------------------------------------*
      *                  * Se start errata : fine visualizzazione      *
      *                  *---------------------------------------------*
           if        w-cat-rig-exs        not  = spaces
                     go to  vis-cor-reg-999.
       vis-cor-reg-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale catena movimenti            *
      *              *-------------------------------------------------*
           move      "RN"                 to   w-cat-rig-ope          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
      *                  *---------------------------------------------*
      *                  * Se fine lettura : fine visualizzazione      *
      *                  *---------------------------------------------*
           if        w-cat-rig-exs        not  = spaces
                     go to  vis-cor-reg-999.
      *              *-------------------------------------------------*
      *              * Movimento da buffer catena movimenti a work di  *
      *              * lavoro                                          *
      *              *-------------------------------------------------*
           move      w-cat-rig-buf        to   w-rig                  .
      *              *-------------------------------------------------*
      *              * Riga singolo movimento                          *
      *              *-------------------------------------------------*
           move      "R"                  to   w-cnt-cor-tvi-rig      .
           move      w-cnt-wrk-ctr-008    to   w-cnt-cor-nrg-dav      .
           perform   vis-lin-cor-000      thru vis-lin-cor-999        .
      *              *-------------------------------------------------*
      *              * Incremento numero riga da visualizzare          *
      *              *-------------------------------------------------*
           add       1                    to   w-cnt-wrk-ctr-008      .
      *              *-------------------------------------------------*
      *              * Test se oltre ultima riga da visualizzare       *
      *              *-------------------------------------------------*
           if        w-cnt-wrk-ctr-008    not  > w-cnt-wrk-ctr-009
                     go to vis-cor-reg-200.
       vis-cor-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione linea corpo numero : w-cnt-cor-nrg-dav    *
      *    *-----------------------------------------------------------*
       vis-lin-cor-000.
      *              *-------------------------------------------------*
      *              * Abblencamento immagine linea di corpo           *
      *              *-------------------------------------------------*
           move      spaces               to   w-lin-imm              .
      *              *-------------------------------------------------*
      *              * Determinazione numero linea assoluta a video    *
      *              *-------------------------------------------------*
           move      w-cnt-cor-nrg-dav    to   w-cnt-wrk-ctr-001      .
           subtract  1                    from w-cnt-wrk-ctr-001      .
           divide    w-lin-num-lin-vis    into w-cnt-wrk-ctr-001
                                        giving w-cnt-wrk-ctr-003
                                     remainder w-cnt-wrk-ctr-002      .
           multiply  w-lin-num-lin-prc    by   w-cnt-wrk-ctr-002      .
           add       w-lin-pri-lin-vid    to   w-cnt-wrk-ctr-002      .
      *              *-------------------------------------------------*
      *              * Test su tipo visualizzazione riga               *
      *              *-------------------------------------------------*
           if        w-cnt-cor-tvi-rig    =    "I"
                     go to vis-lin-cor-100
           else if   w-cnt-cor-tvi-rig    =    "S"
                     go to vis-lin-cor-200
           else if   w-cnt-cor-tvi-rig    =    "N"
                     go to vis-lin-cor-300
           else      go to vis-lin-cor-400.
       vis-lin-cor-100.
      *                  *---------------------------------------------*
      *                  * Se visualizzazione indicatore linea         *
      *                  *---------------------------------------------*
           move      "==>"                to   w-lin-imm-num-lin      .
           move      03                   to   v-car                  .
           go to     vis-lin-cor-900.
       vis-lin-cor-200.
      *                  *---------------------------------------------*
      *                  * Se visualizzazione numero linea a spaces    *
      *                  *---------------------------------------------*
           move      spaces               to   w-lin-imm-num-lin      .
           move      03                   to   v-car                  .
           go to     vis-lin-cor-900.
       vis-lin-cor-300.
      *                  *---------------------------------------------*
      *                  * Se visualizzazione numero d'ordine riga     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      spaces               to   v-edm                  .
           move      w-cat-rig-cur        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-lin-imm-num-lin      .
           go to     vis-lin-cor-900.
       vis-lin-cor-400.
      *                  *---------------------------------------------*
      *                  * Se visualizzazione riga intera              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se riga in append : uscita              *
      *                      *-----------------------------------------*
           if        w-cat-rig-new        not  = spaces and
                     w-cat-rig-lst        not  = spaces
                     go to  vis-lin-cor-999.
      *                      *-----------------------------------------*
      *                      * Editing Numero d'ordine riga            *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      spaces               to   v-edm                  .
           move      w-cat-rig-cur        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-lin-imm-num-lin      .
      *                      *-----------------------------------------*
      *                      * Se riga di tipo New : fine editing      *
      *                      *-----------------------------------------*
           if        w-cat-rig-new        not  = spaces
                     go to vis-lin-cor-800.
      *                      *-----------------------------------------*
      *                      * Editing codice dipendenza               *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      spaces               to   v-edm                  .
           move      w-rig-cod-dpz (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-lin-imm-cod-dpz      .
      *                      *-----------------------------------------*
      *                      * Editing descrizione dipendenza          *
      *                      *-----------------------------------------*
           move      w-rig-cod-dpz-des (1)
                                          to   w-lin-imm-des-dpz      .
      *                      *-----------------------------------------*
      *                      * Editing codice ubicazione               *
      *                      *-----------------------------------------*
           move      w-rig-cod-dsl (1)    to   w-lin-imm-cod-dsl      .
      *                      *-----------------------------------------*
      *                      * Editing descrizione ubicazione          *
      *                      *-----------------------------------------*
           move      w-rig-cod-dsl-des (1)
                                          to   w-lin-imm-des-dsl      .
       vis-lin-cor-800.
      *                      *-----------------------------------------*
      *                      * Parametri per visualizzazione           *
      *                      *-----------------------------------------*
           move      80                   to   v-car                  .
       vis-lin-cor-900.
      *              *-------------------------------------------------*
      *              * Esecuzione visualizzazione prima linea          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      w-cnt-wrk-ctr-002    to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-lin-imm-dsp-lin (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se una sola linea : uscita                      *
      *              *-------------------------------------------------*
           if        w-lin-num-lin-prc    =    1
                     go to vis-lin-cor-999.
      *              *-------------------------------------------------*
      *              * Esecuzione visualizzazione altre linee          *
      *              *-------------------------------------------------*
           move      1                    to   w-cnt-wrk-ctr-003      .
       vis-lin-cor-925.
           add       1                    to   w-cnt-wrk-ctr-003      .
           if        w-cnt-wrk-ctr-003    >    w-lin-num-lin-prc
                     go to vis-lin-cor-999.
           add       1                    to   w-cnt-wrk-ctr-002      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      w-cnt-wrk-ctr-002    to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-lin-imm-dsp-lin
                    (w-cnt-wrk-ctr-003)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     vis-lin-cor-925.
       vis-lin-cor-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts area linee di corpo               *
      *    *-----------------------------------------------------------*
       pmt-cor-reg-000.
      *              *-------------------------------------------------*
      *              * Erase linee impegnate sia dall'area per la fin- *
      *              * catura che dall'area delle righe effettive      *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      19                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Riporto tipo movimento                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Literal                                     *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice tipo movimento      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Codice                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-tmb        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Descrizione                                 *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-tes-des-tmb (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Trattini a linea 05                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all  "-"             to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Fincatura                                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "N.R          Dipendenza                  Ubicazion
      -              "e di origine "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Sottolinetura fincatura                         *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "---   -----------------------   ------------------
      -              "--------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Trattini a linea 19                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all  "-"             to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cor-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione riga corpo di accettazione in w-rig       *
      *    *-----------------------------------------------------------*
       vis-rig-cor-000.
      *              *-------------------------------------------------*
      *              * Se riga di tipo New : nessuna visualizzazione   *
      *              *-------------------------------------------------*
           if        w-cat-rig-new        not  = spaces
                     go to vis-rig-cor-999.
      *              *-------------------------------------------------*
      *              * Campo di accettazione corpo : Dipendenza        *
      *              *-------------------------------------------------*
           perform   vis-cod-dpz-000      thru vis-cod-dpz-999        .
           perform   vis-cod-dpz-des-000  thru vis-cod-dpz-des-999    .
      *              *-------------------------------------------------*
      *              * Campo di accettazione corpo : Ubicazione        *
      *              *-------------------------------------------------*
           perform   vis-cod-dsl-000      thru vis-cod-dsl-999        .
           perform   vis-cod-dsl-des-000  thru vis-cod-dsl-des-999    .
       vis-rig-cor-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per riga corpo di accettazione    *
      *    *-----------------------------------------------------------*
       pmt-rig-cor-000.
      *              *-------------------------------------------------*
      *              * Erase linee impegnate dall'area di impostazione *
      *              * della singola riga corpo espansa a piede        *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      20                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts per riga corpo espansa  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Dipendenza                                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Dipendenza        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ubicazione                                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Ubicazione        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-rig-cor-999.
           exit.

      *    *===========================================================*
      *    * Accettazione del primo campo riga corpo espansa : Codice  *
      *    * dipendenza                                                *
      *    *-----------------------------------------------------------*
       acc-cod-dpz-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-rig-cod-dpz (1)    to   w-sav-cod-dpz          .
       acc-cod-dpz-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripristino valore precedente                *
      *                  *---------------------------------------------*
           move      w-sav-cod-dpz        to   w-rig-cod-dpz (1)      .
      *                  *---------------------------------------------*
      *                  * Parametri generici                          *
      *                  *---------------------------------------------*
           move      "AC"                 to   w-cod-cod-dpz-ope      .
           move      20                   to   w-cod-cod-dpz-lin      .
           move      21                   to   w-cod-cod-dpz-pos      .
           move      20                   to   w-cod-cod-dpz-dln      .
           move      30                   to   w-cod-cod-dpz-dps      .
           move      "<B"                 to   v-edm                  .
      *                  *---------------------------------------------*
      *                  * Tasti funzione                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Up   : sempre ammesso                   *
      *                      *-----------------------------------------*
           move      "UP  "               to   v-pfk (01)             .
      *                      *-----------------------------------------*
      *                      * Down : sempre ammesso a meno che non ci *
      *                      *        siano zero righe                 *
      *                      *-----------------------------------------*
           if        w-cnt-sts-imp-cor    not  = spaces
                     move  "DOWN"         to   v-pfk (02)             .
      *                      *-----------------------------------------*
      *                      * Find : ammesso solo se record new e se  *
      *                      * il primo campo ammette il Find          *
      *                      *-----------------------------------------*
           if        w-cat-rig-new        not  = spaces
                     move  "FIND"         to   v-pfk (03)             .
      *                      *-----------------------------------------*
      *                      * Insr : sempre ammesso, a meno che non   *
      *                      *        si sia in un record New oppure   *
      *                      *        manchi spazio per l'inserimento  *
      *                      *-----------------------------------------*
           if        w-cat-rig-new        =    spaces and
                     w-cat-rig-ins        =    spaces
                     move  "INSR"         to   v-pfk (04)             .
      *                      *-----------------------------------------*
      *                      * Do   : sempre ammesso, purche' sia con- *
      *                      *        cesso dallo status delle impo-   *
      *                      *        stazioni; eventualmente disat-   *
      *                      *        tivato dal richiamo della sub-   *
      *                      *        routine di accettazione          *
      *                      *-----------------------------------------*
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
      *                      *-----------------------------------------*
      *                      * Remv : sempre ammesso                   *
      *                      *-----------------------------------------*
           move      "REMV"               to   v-pfk (06)             .
      *                      *-----------------------------------------*
      *                      * Prsc : sempre ammesso                   *
      *                      *-----------------------------------------*
           move      "PRSC"               to   v-pfk (07)             .
      *                      *-----------------------------------------*
      *                      * Nxsc : sempre ammesso a meno che non ci *
      *                      *        siano zero righe                 *
      *                      *-----------------------------------------*
           if        w-cnt-sts-imp-cor    not  = spaces
                     move  "NXSC"         to   v-pfk (08)             .
      *                      *-----------------------------------------*
      *                      * Back : sempre ammesso, a meno che non   *
      *                      *        si sia gia' sulla prima riga     *
      *                      *-----------------------------------------*
           if        w-cnt-cor-nrg-dac    not  = 1
                     move  "BACK"         to   v-pfk (09)             .
      *                      *-----------------------------------------*
      *                      * Tab  : sempre ammesso, a meno che non   *
      *                      *        si sia gia' sull'ultima riga e   *
      *                      *        che questa sia in Append         *
      *                      *-----------------------------------------*
           if        w-cat-rig-new        =    spaces or
                     w-cat-rig-lst        =    spaces
                     move  "TAB "         to   v-pfk (10)             .
      *                      *-----------------------------------------*
      *                      * Slct : sempre ammesso, a meno che non   *
      *                      *        si sia gia' sull'unica riga      *
      *                      *-----------------------------------------*
           if        w-cat-rig-max        not  = 1
                     move  "SLCT"         to   v-pfk (11)             .
      *                      *-----------------------------------------*
      *                      * Valore di accettazione                  *
      *                      *-----------------------------------------*
           move      w-rig-cod-dpz (1)    to   w-cod-cod-dpz-cod      .
      *                      *-----------------------------------------*
      *                      * Richiamo subroutine di accettazione     *
      *                      *-----------------------------------------*
           perform   cod-cod-dpz-cll-000  thru cod-cod-dpz-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-dpz-foi-000  thru cod-cod-dpz-foi-999    .
       acc-cod-dpz-110.
           perform   cod-cod-dpz-cll-000  thru cod-cod-dpz-cll-999    .
           if        w-cod-cod-dpz-ope    =    "F+"
                     go to acc-cod-dpz-115.
           if        w-cod-cod-dpz-ope    =    "AC"
                     go to acc-cod-dpz-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-dpz-115.
           perform   cod-cod-dpz-foi-000  thru cod-cod-dpz-foi-999    .
           go to     acc-cod-dpz-110.
       acc-cod-dpz-120.
           move      w-cod-cod-dpz-cod    to   v-num                  .
       acc-cod-dpz-200.
      *              *-------------------------------------------------*
      *              * Se Return                                       *
      *              *-------------------------------------------------*
           if        v-key                =    spaces
                     go to acc-cod-dpz-400.
       acc-cod-dpz-250.
      *              *-------------------------------------------------*
      *              * Se Slct                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "SLCT"
                     go to acc-cod-dpz-300
           else      go to acc-cod-dpz-350.
       acc-cod-dpz-300.
      *                  *---------------------------------------------*
      *                  * Numero riga impostato in work di comodo     *
      *                  *---------------------------------------------*
           move      v-num                to   w-cnt-slc-rap-num      .
      *                  *---------------------------------------------*
      *                  * Se numero riga impostato minore di 1 o mag- *
      *                  * maggiore del massimo : reimpostazione       *
      *                  *---------------------------------------------*
           if        w-cnt-slc-rap-num    =    zero       or
                     w-cnt-slc-rap-num    >    w-cat-rig-max
                     go to acc-cod-dpz-100.
      *                  *---------------------------------------------*
      *                  * Se numero riga impostato pari a numero riga *
      *                  * attuale : reimpostazione                    *
      *                  *---------------------------------------------*
           if        w-cnt-slc-rap-num    =    w-cnt-cor-nrg-dac
                     go to acc-cod-dpz-100.
      *                  *---------------------------------------------*
      *                  * Preparazione parametri e uscita             *
      *                  *---------------------------------------------*
           move      w-cnt-slc-rap-num    to   w-cnt-slc-num-rig      .
           move      "."                  to   w-cnt-tus-acc-rig      .
           go to     acc-cod-dpz-999.
       acc-cod-dpz-350.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-rig
                     go to acc-cod-dpz-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-rig
                     go to acc-cod-dpz-999.
      *              *-------------------------------------------------*
      *              * Se premuto un altro tasto funzione non deve es- *
      *              * sere avvenuta variazione del campo d'impostaz.  *
      *              *-------------------------------------------------*
           if        v-mod                not  = spaces
                     go to acc-cod-dpz-100.
      *              *-------------------------------------------------*
      *              * Se Up                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "UP  "
                     move  "U"            to   w-cnt-tus-acc-rig
                     go to acc-cod-dpz-999.
      *              *-------------------------------------------------*
      *              * Se Down                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DOWN"
                     move  "D"            to   w-cnt-tus-acc-rig
                     go to acc-cod-dpz-999.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-rig
                             go to acc-cod-dpz-999
                     else    go to acc-cod-dpz-100.
      *              *-------------------------------------------------*
      *              * Se Insr                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "INSR"
                     move  "I"            to   w-cnt-tus-acc-rig
                     go to acc-cod-dpz-999.
      *              *-------------------------------------------------*
      *              * Se Remv                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "REMV"
                     move  "R"            to   w-cnt-tus-acc-rig
                     go to acc-cod-dpz-999.
      *              *-------------------------------------------------*
      *              * Se Tab                                          *
      *              *-------------------------------------------------*
           if        v-key                =    "TAB "
                     move  "T"            to   w-cnt-tus-acc-rig
                     go to acc-cod-dpz-999.
      *              *-------------------------------------------------*
      *              * Se Back                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "BACK"
                     move  "B"            to   w-cnt-tus-acc-rig
                     go to acc-cod-dpz-999.
      *              *-------------------------------------------------*
      *              * Se Nxsc                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "NXSC"
                     move  "N"            to   w-cnt-tus-acc-rig
                     go to acc-cod-dpz-999.
      *              *-------------------------------------------------*
      *              * Se Prsc                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "PRSC"
                     move  "P"            to   w-cnt-tus-acc-rig
                     go to acc-cod-dpz-999.
       acc-cod-dpz-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore in campo di destinazione             *
      *                  *---------------------------------------------*
           move      v-num                to   w-rig-cod-dpz (1)      .
       acc-cod-dpz-425.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [ada]                      *
      *                  *---------------------------------------------*
           move      w-rig-cod-dpz (1)    to   w-let-arc-ada-cod      .
           perform   let-ana-dpz-000      thru let-ana-dpz-999        .
      *                  *---------------------------------------------*
      *                  * Se dipendenza non esistente : messaggio     *
      *                  * d'errore e reimpostazione                   *
      *                  *---------------------------------------------*
           if        w-let-arc-ada-flg    =    spaces
                     go to acc-cod-dpz-430.
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-rig-cod-dpz (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      spaces               to   w-err-box-err-msg      .
           string    "Non esiste una dipendenza con codice '"
                                delimited by   size
                     v-edt
                                delimited by   spaces
                     "' !"      delimited by   size
                                          into w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     acc-cod-dpz-100.
       acc-cod-dpz-430.
      *                  *---------------------------------------------*
      *                  * Controllo che non esista gia' una riga con  *
      *                  * questo codice dipendenza                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Solo se riga New                        *
      *                      *-----------------------------------------*
           if        w-cat-rig-new        =    spaces
                     go to acc-cod-dpz-450.
      *                      *-----------------------------------------*
      *                      * Controllo                               *
      *                      *-----------------------------------------*
           move      w-rig-cod-dpz (1)    to   w-ctl-cod-dpz-dpz      .
           perform   ctl-cod-dpz-000      thru ctl-cod-dpz-999        .
      *                      *-----------------------------------------*
      *                      * Se non superato : messaggio e reimposta-*
      *                      * zione                                   *
      *                      *-----------------------------------------*
           if        w-ctl-cod-dpz-flg    =    spaces
                     go to acc-cod-dpz-450.
           move      "Esiste gia' una riga con questo codice dipendenza 
      -              "!"
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     acc-cod-dpz-100.
       acc-cod-dpz-450.
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-ada-des    to   w-rig-cod-dpz-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-cod-dpz-des-000  thru vis-cod-dpz-des-999    .
      *                  *---------------------------------------------*
      *                  * Se impostazione a vuoto                     *
      *                  *    - se in Append : come Down purche' non   *
      *                  *                     ci siano zero righe     *
      *                  *    - altrimenti   : reimpostazione          *
      *                  *---------------------------------------------*
           if        w-rig-cod-dpz (1)    =    zero
                     if    w-cat-rig-new  not  = spaces and
                           w-cat-rig-lst  not  = spaces and
                           w-cnt-sts-imp-cor
                                          not  = spaces
                           move  "D"      to   w-cnt-tus-acc-rig
                           go to acc-cod-dpz-999
                     else  go to acc-cod-dpz-100.
      *                  *---------------------------------------------*
      *                  * Se record non New non si ammette la modifi- *
      *                  * ca del valore                               *
      *                  *---------------------------------------------*
           if        w-cat-rig-new        =    spaces and
                     v-mod                not  = spaces
                     go to acc-cod-dpz-100.
       acc-cod-dpz-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-dpz-800.
       acc-cod-dpz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice dipendenza                 *
      *    *-----------------------------------------------------------*
       vis-cod-dpz-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      20                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      w-rig-cod-dpz (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-dpz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Denominazione dipendenza          *
      *    *-----------------------------------------------------------*
       vis-cod-dpz-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-rig-cod-dpz-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-dpz-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice ubicazione                          *
      *    *-----------------------------------------------------------*
       acc-cod-dsl-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-dsl-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-zub-ope      .
           move      w-rig-cod-dpz (1)    to   w-cod-des-zub-dpz      .
           move      w-rig-cod-dsl (1)    to   w-cod-des-zub-cod      .
           move      21                   to   w-cod-des-zub-lin      .
           move      21                   to   w-cod-des-zub-pos      .
           move      21                   to   w-cod-des-zub-dln      .
           move      30                   to   w-cod-des-zub-dps      .
           move      spaces               to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           if        w-cnt-sts-imp-rig    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      "REMV"               to   v-pfk (06)             .
           perform   cod-des-zub-cll-000  thru cod-des-zub-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-zub-foi-000  thru cod-des-zub-foi-999    .
       acc-cod-dsl-110.
           perform   cod-des-zub-cll-000  thru cod-des-zub-cll-999    .
           if        w-cod-des-zub-ope    =    "F+"
                     go to acc-cod-dsl-115.
           if        w-cod-des-zub-ope    =    "AC"
                     go to acc-cod-dsl-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-dsl-115.
           perform   cod-des-zub-foi-000  thru cod-des-zub-foi-999    .
           go to     acc-cod-dsl-110.
       acc-cod-dsl-120.
           move      w-cod-des-zub-cod    to   v-alf                  .
       acc-cod-dsl-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-rig
                     go to acc-cod-dsl-999.
      *              *-------------------------------------------------*
      *              * Se Remv                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "REMV"
                     move  "R"            to   w-cnt-tus-acc-rig
                     go to acc-cod-dsl-999.
      *              *-------------------------------------------------*
      *              * Valore impostato                                *
      *              *-------------------------------------------------*
           move      v-alf                to   w-rig-cod-dsl (1)      .
       acc-cod-dsl-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella [zub]                       *
      *                  *---------------------------------------------*
           move      w-rig-cod-dpz (1)    to   w-let-arc-zub-dpz      .
           move      w-rig-cod-dsl (1)    to   w-let-arc-zub-cod      .
           perform   let-arc-zub-000      thru let-arc-zub-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zub-des    to   w-rig-cod-dsl-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-cod-dsl-des-000  thru vis-cod-dsl-des-999    .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : reimpostazione          *
      *                  *---------------------------------------------*
           if        w-let-arc-zub-flg    not  = spaces
                     go to acc-cod-dsl-100.
       acc-cod-dsl-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-dsl-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-rig-000
                                          thru cnt-tdo-rig-999
                     if      w-cnt-tdo-rig-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-rig
                             go to acc-cod-dsl-999
                     else    move  spaces to   w-cnt-tdo-rig-flg
                             go to acc-cod-dsl-100.
       acc-cod-dsl-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice dislocazione               *
      *    *-----------------------------------------------------------*
       vis-cod-dsl-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      w-rig-cod-dsl (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-dsl-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Descrizione codice dislocazione   *
      *    *-----------------------------------------------------------*
       vis-cod-dsl-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-rig-cod-dsl-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-dsl-des-999.
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
      *              * Test su Codice tipo movimento                   *
      *              *-------------------------------------------------*
           if        w-tes-cod-tmb        =    spaces
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
      *              * Test su : Interessa la fatturazione             *
      *              *-------------------------------------------------*
           if        w-tes-int-ftr (1)    not  = zero
                     go to cnt-tdo-nok-100.
           move      "Mancano le modalita' di fatturazione !"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-100.
      *              *-------------------------------------------------*
      *              * Test su : tipo movimento per fatturazione       *
      *              *-------------------------------------------------*
           if        w-tes-int-ftr (1)    =    01
                     go to cnt-tdo-nok-200.
           if        w-tes-tmo-ftr (1)    not  = spaces
                     go to cnt-tdo-nok-200.
           move      "Manca il tipo movimento per fatturazione !"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-200.
      *              *-------------------------------------------------*
      *              * Test su : Si/No accompagnamento merce           *
      *              *-------------------------------------------------*
           if        w-tes-snx-acm (1)    not  = spaces
                     go to cnt-tdo-nok-300.
           move      "Manca da definire se il documento accompagna merce
      -              " !"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-300.
      *              *-------------------------------------------------*
      *              * Test su : codice c/merce                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cau-mag (1)    =    zero
                     go to cnt-tdo-nok-400.
           if        w-tes-cau-mag-ttc (1)
                                          =    01 or
                     w-tes-cau-mag-ttc (1)
                                          =    02
                     go to cnt-tdo-nok-400.
      *                  *---------------------------------------------*
      *                  * Controllo                                   *
      *                  *---------------------------------------------*
           if        w-tes-cod-mic (1)    not  = spaces
                     go to cnt-tdo-nok-400.
           move      "Manca il codice c/merce !"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-400.
      *              *-------------------------------------------------*
      *              * Test su : tipo archivio                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo movimento valido per fatturazione   *
      *                  *---------------------------------------------*
           if        w-tes-int-ftr (1)    =    01
                     go to cnt-tdo-nok-500.
           if        w-tes-def-tar (1)    =    "C" and
                     w-tes-snv-tar (1)    =    "N" and
                     w-tes-lst-tar (1)    =    "C"
                     go to cnt-tdo-nok-500.
           move      "Dati relativi all'archivio errati per la fatturazi
      -              "one !"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-500.
      *              *-------------------------------------------------*
      *              * Test su : origine del documento                 *
      *              *-------------------------------------------------*
           if        w-tes-org-doc (1)    not  = zero
                     go to cnt-tdo-nok-600.
           move      "Manca il tipo origine del documento"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-600.
      *              *-------------------------------------------------*
      *              * Test su : Descrizione dicitura                  *
      *              *-------------------------------------------------*
           if        w-tes-snx-dct (1)    not  = 01
                     go to cnt-tdo-nok-700.
           if        w-tes-des-dct (1)    not  = spaces
                     go to cnt-tdo-nok-700.
           move      "Manca la descrizione dicitura da stampare"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-700.
      *              *-------------------------------------------------*
      *              * Compatibilita' fra tipo movimento a fronte e    *
      *              * tipo archivio                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione tipo movimento a fronte     *
      *                  *---------------------------------------------*
           if        w-tes-mov-afd (1)    =    zero
                     move 01              to   w-tes-mov-afd (1)      .
      *                  *---------------------------------------------*
      *                  * Se non riguarda ordini clienti o ordini di  *
      *                  * spedizione : oltre                          *
      *                  *---------------------------------------------*
           if        w-tes-mov-afd (1)    =    01 or
                     w-tes-mov-afd (1)    =    04
                     go to cnt-tdo-nok-750
           else if   w-tes-mov-afd (1)    =    02
                     go to cnt-tdo-nok-720
           else if   w-tes-mov-afd (1)    =    03
                     go to cnt-tdo-nok-730.
           go to     cnt-tdo-nok-750.
       cnt-tdo-nok-720.
      *                  *---------------------------------------------*
      *                  * Se riguarda ordini clienti : controllo che  *
      *                  * nella lista archivi sia presente tipo 'C' o *
      *                  * 'D'                                         *
      *                  *---------------------------------------------*
           move      zero                 to   w-wrk-lta-inx-001      .
           inspect   w-tes-lst-tar (1)
                                      tallying w-wrk-lta-inx-001
                     for                  all  "C"                    .
           if        w-wrk-lta-inx-001    >    zero
                     go to cnt-tdo-nok-750.
           move      zero                 to   w-wrk-lta-inx-001      .
           inspect   w-tes-lst-tar (1)
                                      tallying w-wrk-lta-inx-001
                     for                  all  "D"                    .
           if        w-wrk-lta-inx-001    >    zero
                     go to cnt-tdo-nok-750.
           move      "Movimento a fronte incompatibile con tipo archivio
      -              " !"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-730.
      *                  *---------------------------------------------*
      *                  * Se riguarda ordini di spedizione : si       *
      *                  * controlla che nella lista archivi siano     *
      *                  * presenti i tipo archivio previsti dal tipo  *
      *                  * movimento a fronte                          *
      *                  *---------------------------------------------*
           move      zero                 to   w-wrk-lta-inx-001      .
           inspect   w-tes-lst-tar (1)
                                      tallying w-wrk-lta-inx-001
                     for                  all  w-let-arc-zsc-tar      .
           if        w-wrk-lta-inx-001    >    zero
                     go to cnt-tdo-nok-750.
           move      "Movimento a fronte incompatibile con tipo archivio
      -              " !"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-750.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Validita' per le dipendenze                 *
      *                  *---------------------------------------------*
           if        w-dpz-ctr-dpz        >    1
                     go to cnt-tdo-nok-760.
      *                  *---------------------------------------------*
      *                  * Valore forzato                              *
      *                  *---------------------------------------------*
           move      spaces               to   w-tes-vld-dpz (1)      .
       cnt-tdo-nok-760.
      *                  *---------------------------------------------*
      *                  * Numero copie da stampare                    *
      *                  *---------------------------------------------*
           if        w-tes-num-cps (1)    =    zero
                     move  01             to   w-tes-num-cps (1)      .
       cnt-tdo-nok-770.
      *                  *---------------------------------------------*
      *                  * Si/no stampa Agente                         *
      *                  *---------------------------------------------*
           if        w-tes-snx-age (1)    =    spaces
                     move  "N"            to   w-tes-snx-age (1)      .
       cnt-tdo-nok-780.
      *                  *---------------------------------------------*
      *                  * Si/no controllo stampa gia' effettuata      *
      *                  *---------------------------------------------*
           if        w-tes-snx-lib (1)    not  = "S"
                     move  spaces         to   w-tes-snx-lib (1)      .
       cnt-tdo-nok-800.
      *              *-------------------------------------------------*
      *              * Uscita per controlli superati                   *
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
       cnt-tdo-nok-999.
           exit.
           
      *    *===========================================================*
      *    * Controllo su impostazione tasto Do riga corpo             *
      *    *-----------------------------------------------------------*
       cnt-tdo-rig-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-rig-flg      .
       cnt-tdo-rig-999.
           exit.

      *    *===========================================================*
      *    * Preparazioni pre-accettazione riga corpo                  *
      *    *-----------------------------------------------------------*
       pre-rig-cor-000.
       pre-rig-cor-999.
           exit.

      *    *===========================================================*
      *    * Attribuzione del numero progressivo per la riga New       *
      *    *-----------------------------------------------------------*
       prg-rig-cor-000.
           move      w-cat-rig-prg        to   w-rig-num-prg (1)      .
       prg-rig-cor-999.
           exit.

      *    *===========================================================*
      *    * Azioni post-accettazione riga corpo                       *
      *    *-----------------------------------------------------------*
       pos-rig-cor-000.
       pos-rig-cor-999.
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
      *              *-------------------------------------------------*
      *              * Normalizzazione dati non chiave riga corpo      *
      *              *-------------------------------------------------*
           perform   nor-nok-rig-000      thru nor-nok-rig-999        .
      *              *-------------------------------------------------*
      *              * Normalizzazione dati non chiave piede           *
      *              *-------------------------------------------------*
           perform   nor-nok-pie-000      thru nor-nok-pie-999        .
       nor-key-nok-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati chiave                               *
      *    *-----------------------------------------------------------*
       nor-key-reg-000.
           move      spaces               to   w-tes-cod-tmb          .
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *-----------------------------------------------------------*
       nor-nok-tes-000.
           move      spaces               to   w-tes-des-tmb (1)      .
           move      spaces               to   w-tes-des-key (1)      .
           move      spaces               to   w-tes-des-stp (1)      .
           move      spaces               to   w-tes-pwd-tmb (1)      .
           move      zero                 to   w-tes-int-ftr (1)      .
           move      spaces               to   w-tes-tmo-ftr (1)      .
           move      spaces               to   w-tes-tmo-ftr-des (1)  .
           move      zero                 to   w-tes-tmo-ftr-tdo (1)  .
           move      spaces               to   w-tes-tmo-ft2 (1)      .
           move      spaces               to   w-tes-snx-acm (1)      .
           move      zero                 to   w-tes-def-tac (1)      .
           move      spaces               to   w-tes-def-ctr (1)      .
           move      spaces               to   w-tes-def-ctr-des (1)  .
           move      zero                 to   w-tes-cau-mag (1)      .
           move      spaces               to   w-tes-cau-mag-des (1)  .
           move      zero                 to   w-tes-cau-mag-ttc (1)  .
           move      spaces               to   w-tes-cau-mag-tpc (1)  .
           move      spaces               to   w-tes-cau-mag-cdc (1)  .
           move      spaces               to   w-tes-cau-mag-vac (1)  .
           move      spaces               to   w-tes-cau-mag-dfa (1)  .
           move      spaces               to   w-tes-cau-mag-vaa (1)  .
           move      spaces               to   w-tes-cau-mag-lsa (1)  .
           move      spaces               to   w-tes-cod-mic (1)      .
           move      spaces               to   w-tes-cod-mic-des (1)  .
           move      zero                 to   w-tes-cam-agg (1)      .
           move      spaces               to   w-tes-cam-agg-des (1)  .
           move      spaces               to   w-tes-cam-agg-dfa (1)  .
           move      spaces               to   w-tes-cam-agg-vaa (1)  .
           move      spaces               to   w-tes-cam-agg-lsa (1)  .
           move      spaces               to   w-tes-def-tar (1)      .
           move      spaces               to   w-tes-snv-tar (1)      .
           move      spaces               to   w-tes-lst-tar (1)      .
           move      zero                 to   w-tes-org-doc (1)      .
           move      zero                 to   w-tes-prv-doc (1)      .
           move      spaces               to   w-tes-sgl-num (1)      .
           move      zero                 to   w-tes-mov-afd (1)      .
           move      spaces               to   w-tes-def-tmf (1)      .
           move      zero                 to   w-tes-snx-prz (1)      .
           move      zero                 to   w-tes-snx-sco (1)      .
           move      zero                 to   w-tes-snx-imp (1)      .
           move      zero                 to   w-tes-snx-civ (1)      .
           move      zero                 to   w-tes-snx-ttd (1)      .
           move      zero                 to   w-tes-snx-dct (1)      .
           move      spaces               to   w-tes-des-dct (1)      .
           move      zero                 to   w-tes-num-cps (1)      .
           move      zero                 to   w-tes-def-tva (1)      .
           move      spaces               to   w-tes-snx-par (1)      .
           move      spaces               to   w-tes-vld-dpz (1)      .
           move      spaces               to   w-tes-snx-age (1)      .
           move      spaces               to   w-tes-snx-ndp (1)      .
           move      spaces               to   w-tes-snx-fop (1)      .
           move      spaces               to   w-tes-snx-lib (1)      .
           move      spaces               to   w-tes-cod-dsl (1)      .
           move      spaces               to   w-tes-def-tpr (1)      .
           move      spaces               to   w-tes-snx-nmm (1)      .
           move      spaces               to   w-tes-snx-ncv (1)      .
           move      spaces               to   w-tes-cod-lst (1)      .
           move      spaces               to   w-tes-cod-lst-des (1)  .
           move      spaces               to   w-tes-snx-nmc (1)      .
           move      spaces               to   w-tes-alx-gen (1)      .
       nor-nok-tes-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave riga corpo                *
      *    *-----------------------------------------------------------*
       nor-nok-rig-000.
           move      zero                 to   w-rig-cod-dpz (1)      .
           move      spaces               to   w-rig-cod-dpz-des (1)  .
           move      spaces               to   w-rig-cod-dsl (1)      .
           move      spaces               to   w-rig-cod-dsl-des (1)  .
           move      spaces               to   w-rig-alx-dpz (1)      .
       nor-nok-rig-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave piede                     *
      *    *-----------------------------------------------------------*
       nor-nok-pie-000.
       nor-nok-pie-999.
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
      *              * Lettura archivio [zbi]                          *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODTMB    "         to   f-key                  .
           move      w-tes-cod-tmb        to   rf-zbi-cod-tmb         .
           move      zero                 to   rf-zbi-cod-dpz         .
           move      "pgm/bol/fls/ioc/obj/iofzbi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbi                 .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to rou-let-reg-100.
       rou-let-reg-050.
      *                  *---------------------------------------------*
      *                  * Se movimento non trovato                    *
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
      *                  * Se movimento trovato                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo funzionamento : Modifica           *
      *                      *-----------------------------------------*
           move      "M"                  to   w-cnt-mfu-tip-fun      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione segnale di duplicazione *
      *                      *-----------------------------------------*
           move      spaces               to   w-cnt-dup-rec-flg      .
      *                      *-----------------------------------------*
      *                      * Inizializzazione catena [rig]           *
      *                      *-----------------------------------------*
           move      "BE"                 to   w-cat-rig-ope          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
      *                      *-----------------------------------------*
      *                      * Determinazione valori attuali testata   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Valori contenuti direttamente da    *
      *                          * record [zbi]                        *
      *                          *-------------------------------------*
           move      rf-zbi-des-tmb       to   w-tes-des-tmb (1)      .
           move      rf-zbi-des-key       to   w-tes-des-key (1)      .
           move      rf-zbi-des-stp       to   w-tes-des-stp (1)      .
           move      rf-zbi-pwd-tmb       to   w-tes-pwd-tmb (1)      .
           move      rf-zbi-int-ftr       to   w-tes-int-ftr (1)      .
           move      rf-zbi-tmo-ftr       to   w-tes-tmo-ftr (1)      .
           move      rf-zbi-snx-acm       to   w-tes-snx-acm (1)      .
           move      rf-zbi-def-tac       to   w-tes-def-tac (1)      .
           move      rf-zbi-def-ctr       to   w-tes-def-ctr (1)      .
           move      rf-zbi-cau-mag       to   w-tes-cau-mag (1)      .
           move      rf-zbi-cod-mic       to   w-tes-cod-mic (1)      .
           move      rf-zbi-cam-agg       to   w-tes-cam-agg (1)      .
           move      rf-zbi-def-tar       to   w-tes-def-tar (1)      .
           move      rf-zbi-snv-tar       to   w-tes-snv-tar (1)      .
           move      rf-zbi-lst-tar       to   w-tes-lst-tar (1)      .
           move      rf-zbi-org-doc       to   w-tes-org-doc (1)      .
           move      rf-zbi-prv-doc       to   w-tes-prv-doc (1)      .
           move      rf-zbi-sgl-num       to   w-tes-sgl-num (1)      .
           move      rf-zbi-mov-afd       to   w-tes-mov-afd (1)      .
           move      rf-zbi-def-tmf       to   w-tes-def-tmf (1)      .
           move      rf-zbi-snx-prz       to   w-tes-snx-prz (1)      .
           move      rf-zbi-snx-sco       to   w-tes-snx-sco (1)      .
           move      rf-zbi-snx-imp       to   w-tes-snx-imp (1)      .
           move      rf-zbi-snx-civ       to   w-tes-snx-civ (1)      .
           move      rf-zbi-snx-ttd       to   w-tes-snx-ttd (1)      .
           move      rf-zbi-snx-dct       to   w-tes-snx-dct (1)      .
           move      rf-zbi-des-dct       to   w-tes-des-dct (1)      .
           move      rf-zbi-tip-sql       to   w-tes-num-cps (1)      .
      *
           move      rf-zbi-pos-sql       to   w-tes-def-tva (1)      .
           if        rf-zbi-pos-sql       >    02
                     move  01             to   w-tes-def-tva (1)      .
      *
           move      rf-zbi-snx-par       to   w-tes-snx-par (1)      .
           move      rf-zbi-vld-dpz       to   w-tes-vld-dpz (1)      .
           move      rf-zbi-snx-age       to   w-tes-snx-age (1)      .
           move      rf-zbi-snx-ndp       to   w-tes-snx-ndp (1)      .
           move      rf-zbi-snx-fop       to   w-tes-snx-fop (1)      .
           move      rf-zbi-snx-lib       to   w-tes-snx-lib (1)      .
           move      rf-zbi-tmo-ft2       to   w-tes-tmo-ft2 (1)      .
           move      rf-zbi-alx-gen       to   w-tes-alx-gen (1)      .
           move      rf-zbi-cod-dsl       to   w-tes-cod-dsl (1)      .
           move      rf-zbi-def-tpr       to   w-tes-def-tpr (1)      .
           move      rf-zbi-cod-lst       to   w-tes-cod-lst (1)      .
      *
           move      rf-zbi-snx-nmc       to   w-tes-snx-nmc (1)      .
           if        w-tes-snx-nmc (1)    =    spaces
                     move  "S"            to   w-tes-snx-nmc (1)      .
      *
           move      rf-zbi-snx-nmm       to   w-tes-snx-nmm (1)      .
           if        w-tes-snx-nmm (1)    =    spaces
                     move  "S"            to   w-tes-snx-nmm (1)      .
      *
           move      rf-zbi-snx-ncv       to   w-tes-snx-ncv (1)      .
           if        w-tes-snx-nmm (1)    =    spaces
                     move  "N"            to   w-tes-snx-ncv (1)      .
      *                          *-------------------------------------*
      *                          * Eventuale normalizzazione numero    *
      *                          * copie da stampare                   *
      *                          *-------------------------------------*
           if        w-tes-num-cps (1)    =    zero
                     move  01             to   w-tes-num-cps (1)      .
           if        w-tes-num-cps (1)    >    05
                     move  01             to   w-tes-num-cps (1)      .
      *                          *-------------------------------------*
      *                          * Valori contenuti indirettamente in  *
      *                          * record [zbi]                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura archivio [zfi]          *
      *                              *---------------------------------*
           move      w-tes-tmo-ftr (1)    to   w-let-arc-zfi-cod      .
           perform   let-arc-zfi-000      thru let-arc-zfi-999        .
           move      w-let-arc-zfi-des    to   w-tes-tmo-ftr-des (1)  .
           move      w-let-arc-zfi-tdo    to   w-tes-tmo-ftr-tdo (1)  .
      *                              *---------------------------------*
      *                              * Lettura tabella [zct]           *
      *                              *---------------------------------*
           move      w-tes-def-ctr (1)    to   w-let-arc-zct-cod      .
           perform   let-arc-zct-000      thru let-arc-zct-999        .
           move      w-let-arc-zct-des    to   w-tes-def-ctr-des (1)  .
           if        w-tes-def-ctr (1)    =    spaces
                     move  "Nessuna proposta"
                                          to   w-tes-def-ctr-des (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [zmc]          *
      *                              *---------------------------------*
           move      w-tes-cau-mag (1)    to   w-let-arc-zmc-cod      .
           perform   let-arc-zmc-000      thru let-arc-zmc-999        .
           move      w-let-arc-zmc-des    to   w-tes-cau-mag-des (1)  .
           move      w-let-arc-zmc-ttc    to   w-tes-cau-mag-ttc (1)  .
           move      w-let-arc-zmc-tpc    to   w-tes-cau-mag-tpc (1)  .
           move      w-let-arc-zmc-cdc    to   w-tes-cau-mag-cdc (1)  .
           move      w-let-arc-zmc-vac    to   w-tes-cau-mag-vac (1)  .
           move      w-let-arc-zmc-dfa    to   w-tes-cau-mag-dfa (1)  .
           move      w-let-arc-zmc-vaa    to   w-tes-cau-mag-vaa (1)  .
           move      w-let-arc-zmc-lsa    to   w-tes-cau-mag-lsa (1)  .
      *                              *---------------------------------*
      *                              * Lettura tabella [zmm]           *
      *                              *---------------------------------*
           move      w-tes-cod-mic (1)    to   w-let-arc-zmm-cod      .
           perform   let-arc-zmm-000      thru let-arc-zmm-999        .
           move      w-let-arc-zmm-des    to   w-tes-cod-mic-des (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [zmc]          *
      *                              *---------------------------------*
           move      w-tes-cam-agg (1)    to   w-let-arc-zmc-cod      .
           perform   let-arc-zmc-000      thru let-arc-zmc-999        .
           move      w-let-arc-zmc-des    to   w-tes-cam-agg-des (1)  .
           move      w-let-arc-zmc-dfa    to   w-tes-cam-agg-dfa (1)  .
           move      w-let-arc-zmc-vaa    to   w-tes-cam-agg-vaa (1)  .
           move      w-let-arc-zmc-lsa    to   w-tes-cam-agg-lsa (1)  .
      *                              *---------------------------------*
      *                              * Anagrafica listini              *
      *                              *---------------------------------*
           move      w-tes-cod-lst (1)    to   w-let-arc-zls-cod      .
           perform   let-arc-zls-000      thru let-arc-zls-999        .
      *
           if        w-tes-cod-lst (1)    =    spaces
                     move  "(nessun default)"
                                          to   w-tes-cod-lst-des (1)
           else      move  w-let-arc-zls-des
                                          to   w-tes-cod-lst-des (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivi in funzione del *
      *                              * tipo movimento a fronte         *
      *                              *---------------------------------*
           if        w-tes-mov-afd (1)    =    01 or
                     w-tes-mov-afd (1)    =    04
                     go to rou-let-reg-400
           else if   w-tes-mov-afd (1)    =    02
                     go to rou-let-reg-320
           else if   w-tes-mov-afd (1)    =    03
                     go to rou-let-reg-330.
           go to     rou-let-reg-400.
       rou-let-reg-320.
      *                              *---------------------------------*
      *                              * Lettura tabella [zoc]           *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Test se a spazi             *
      *                                  *-----------------------------*
           if        w-tes-def-tmf (1)    =    spaces
                     go to rou-let-reg-400.
      *                                  *-----------------------------*
      *                                  * Lettura                     *
      *                                  *-----------------------------*
           move      w-tes-def-tmf (1)    to   w-let-arc-zoc-cod      .
           perform   let-arc-zoc-000      thru let-arc-zoc-999        .
      *                                  *-----------------------------*
      *                                  * Oltre                       *
      *                                  *-----------------------------*
           go to     rou-let-reg-400.
       rou-let-reg-330.
      *                              *---------------------------------*
      *                              * Lettura tabella [zsc]           *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Test se a spazi             *
      *                                  *-----------------------------*
           if        w-tes-def-tmf (1)    =    spaces
                     go to rou-let-reg-400.
      *                                  *-----------------------------*
      *                                  * Lettura                     *
      *                                  *-----------------------------*
           move      w-tes-def-tmf (1)    to   w-let-arc-zsc-cod      .
           perform   let-arc-zsc-000      thru let-arc-zsc-999        .
      *                                  *-----------------------------*
      *                                  * Oltre                       *
      *                                  *-----------------------------*
           go to     rou-let-reg-400.
       rou-let-reg-400.
      *                      *-----------------------------------------*
      *                      * Valori precedenti testata               *
      *                      *-----------------------------------------*
           move      w-tes-val-aep (1)    to   w-tes-val-aep (2)      .
       rou-let-reg-500.
      *              *-------------------------------------------------*
      *              * Lettura righe                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start                                       *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CODTMB    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-tes-cod-tmb        to   rf-zbi-cod-tmb         .
           move      zero                 to   rf-zbi-cod-dpz         .
           move      "pgm/bol/fls/ioc/obj/iofzbi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbi                 .
      *                      *-----------------------------------------*
      *                      * Se errore di start : fine lettura       *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to  rou-let-reg-850.
       rou-let-reg-600.
      *                  *---------------------------------------------*
      *                  * Lettura sequenziale file [zbi]              *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofzbi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbi                 .
      *                      *-----------------------------------------*
      *                      * Se at end : fine lettura                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to  rou-let-reg-850.
      *                  *---------------------------------------------*
      *                  * Se oltre il massimo : fine lettura          *
      *                  *---------------------------------------------*
           if        rf-zbi-cod-tmb       not  = w-tes-cod-tmb
                     go to  rou-let-reg-850.
      *                  *---------------------------------------------*
      *                  * Se record con codice dipendenza a zero : ri-*
      *                  * ciclo in lettura                            *
      *                  *---------------------------------------------*
           if        rf-zbi-cod-dpz       =    zero
                     go to rou-let-reg-600.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione riga                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Valori attuali                          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Valori contenuti direttamente in    *
      *                          * record [zbi]                        *
      *                          *-------------------------------------*
           move      rf-zbi-cod-dpz       to   w-rig-cod-dpz (1)      .
           move      rf-zbi-cod-dsl       to   w-rig-cod-dsl (1)      .
           move      rf-zbi-alx-dpz       to   w-rig-alx-dpz (1)      .
      *                          *-------------------------------------*
      *                          * Valori contenuti indirettamente in  *
      *                          * record [zbi]                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura archivio [ada]          *
      *                              *---------------------------------*
           move      w-rig-cod-dpz (1)    to   w-let-arc-ada-cod      .
           perform   let-ana-dpz-000      thru let-ana-dpz-999        .
           move      w-let-arc-ada-des    to   w-rig-cod-dpz-des (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [zub]          *
      *                              *---------------------------------*
           move      w-rig-cod-dpz (1)    to   w-let-arc-zub-dpz      .
           move      w-rig-cod-dsl (1)    to   w-let-arc-zub-cod      .
           perform   let-arc-zub-000      thru let-arc-zub-999        .
           move      w-let-arc-zub-des    to   w-rig-cod-dsl-des (1)  .
       rou-let-reg-800.
      *                      *-----------------------------------------*
      *                      * Append di un record vuoto a fine catena *
      *                      *-----------------------------------------*
           move      "AP"                 to   w-cat-rig-ope          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
      *                      *-----------------------------------------*
      *                      * Aggiornamento del numero progressivo    *
      *                      *-----------------------------------------*
           perform   prg-rig-cor-000      thru prg-rig-cor-999        .
      *                      *-----------------------------------------*
      *                      * Valori precedenti                       *
      *                      *-----------------------------------------*
           move      w-rig-val-aep (1)    to   w-rig-val-aep (2)      .
      *                      *-----------------------------------------*
      *                      * Update del record vuoto di fine catena  *
      *                      *-----------------------------------------*
           move      "UP"                 to   w-cat-rig-ope          .
           move      w-cat-rig-max        to   w-cat-rig-num          .
           move      w-rig                to   w-cat-rig-buf          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
      *                  *---------------------------------------------*
      *                  * Riciclo a lettura [zbi] successivo          *
      *                  *---------------------------------------------*
           go to     rou-let-reg-600.
       rou-let-reg-850.
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
      *              * Duplicazione record precedente se richiesto     *
      *              *-------------------------------------------------*
           if        w-cnt-dup-rec-flg    =    spaces
                     go to pre-acc-ins-500.
       pre-acc-ins-200.
      *                  *---------------------------------------------*
      *                  * Se richiesta la duplicazione                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Da buffer a 'w-tes'                     *
      *                      *-----------------------------------------*
           move      w-tes-val-aep (2)    to   w-tes-val-aep (1)      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione flags di controllo      *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-tes      .
           move      all "#"              to   w-cnt-sts-imp-pte      .
           move      all "#"              to   w-cnt-sts-ing-pte      .
           move      "#"                  to   w-cnt-sts-imp-cor      .
           move      "#"                  to   w-cnt-sts-imp-pie      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione flags di visualizzazio- *
      *                      * ne per le pagine                        *
      *                      *-----------------------------------------*
           move      spaces               to   w-cnt-sts-vis-ptx (2)  .
           move      spaces               to   w-cnt-sts-vis-ptx (3)  .
           move      spaces               to   w-cnt-sts-vis-ptx (4)  .
           move      spaces               to   w-cnt-sts-vis-ptx (5)  .
           move      spaces               to   w-cnt-sts-vis-ptx (6)  .
           move      spaces               to   w-cnt-sts-vis-ptx (7)  .
           move      spaces               to   w-cnt-sts-vis-ptx (8)  .
           move      spaces               to   w-cnt-sts-vis-ptx (9)  .
      *                  *---------------------------------------------*
      *                  * Normalizzazione segnale di duplicazione     *
      *                  *---------------------------------------------*
           move      spaces               to   w-cnt-dup-rec-flg      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione pagine di testata           *
      *                  *---------------------------------------------*
           perform   vis-tes-reg-000      thru vis-tes-reg-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pre-acc-ins-999.
       pre-acc-ins-500.
      *                  *---------------------------------------------*
      *                  * Se non richiesta la duplicazione            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione segnale di duplicazione *
      *                      *-----------------------------------------*
           move      spaces               to   w-cnt-dup-rec-flg      .
      *                      *-----------------------------------------*
      *                      * Inizializzazione catena [rig]           *
      *                      *-----------------------------------------*
           move      "BE"                 to   w-cat-rig-ope          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
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
      *              * Se inserimento non si esaminano le righe dele-  *
      *              * tate                                            *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "I"
                     go to scr-mov-fil-200.
       scr-mov-fil-100.
      *              *-------------------------------------------------*
      *              * Lettura riga "last removed"                     *
      *              *-------------------------------------------------*
           move      "LR"                 to   w-cat-rig-ope          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
      *              *-------------------------------------------------*
      *              * Se fine : a trattamento righe "in vita"         *
      *              *-------------------------------------------------*
           if        w-cat-rig-exs        not  = spaces
                     go to scr-mov-fil-200.
      *              *-------------------------------------------------*
      *              * Se riga precedente non esistente : riciclo      *
      *              *-------------------------------------------------*
           move      w-cat-rig-buf        to   w-rig                  .
           if        w-rig-num-prg (2)    =    zero
                     go to scr-mov-fil-100.
      *              *-------------------------------------------------*
      *              * Delete [zbi] relativo alla dipendenza           *
      *              *-------------------------------------------------*
           perform   del-rec-zbd-000      thru del-rec-zbd-999        .
      *              *-------------------------------------------------*
      *              * Riciclo a prossima "last removed"               *
      *              *-------------------------------------------------*
           go to     scr-mov-fil-100.
       scr-mov-fil-200.
      *              *-------------------------------------------------*
      *              * Determinazione del flag di variazioni in testa- *
      *              * ta che si ripercuotono anche sulle righe        *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-var-tes-cor      .
      *              *-------------------------------------------------*
      *              * Start su righe corpo                            *
      *              *-------------------------------------------------*
           move      "ST"                 to   w-cat-rig-ope          .
           move      1                    to   w-cat-rig-num          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
      *              *-------------------------------------------------*
      *              * Se start non valida : uscita                    *
      *              *-------------------------------------------------*
           if        w-cat-rig-exs        not  = spaces
                     go to scr-mov-fil-700.
       scr-mov-fil-300.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale righe corpo                 *
      *              *-------------------------------------------------*
           move      "RN"                 to   w-cat-rig-ope          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
      *              *-------------------------------------------------*
      *              * Se at end : uscita                              *
      *              *-------------------------------------------------*
           if        w-cat-rig-exs        not  = spaces
                     go to scr-mov-fil-700.
      *              *-------------------------------------------------*
      *              * Movimento da buffer catena movimenti a work     *
      *              *-------------------------------------------------*
           move      w-cat-rig-buf        to   w-rig                  .
      *              *-------------------------------------------------*
      *              * Trattamento file [zbi] dipendenza               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-500.
       scr-mov-fil-400.
      *                      *-----------------------------------------*
      *                      * Write record [zbi] relativo alla dipen- *
      *                      * denza                                   *
      *                      *-----------------------------------------*
           perform   wrt-rec-zbd-000      thru wrt-rec-zbd-999        .
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura riga corpo successiva *
      *                      *-----------------------------------------*
           go to     scr-mov-fil-300.
       scr-mov-fil-500.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se riga nuova : come inserimento        *
      *                      *-----------------------------------------*
           if        w-rig-cod-dpz (2)    =    zero
                     go to scr-mov-fil-400.
      *                      *-----------------------------------------*
      *                      * Controllo se riga corpo variata o se    *
      *                      * variazioni in testata che si riper-     *
      *                      * cuotono sulle righe                     *
      *                      *-----------------------------------------*
           if        w-rig-val-aep (1)    not  = w-rig-val-aep (2) or
                     w-cnt-var-tes-cor    not  = spaces
                     go to scr-mov-fil-600.
      *                      *-----------------------------------------*
      *                      * Se riga corpo non variata               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Riciclo a lettura riga corpo suc-   *
      *                          * cessiva                             *
      *                          *-------------------------------------*
           go to     scr-mov-fil-300.
       scr-mov-fil-600.
      *                      *-----------------------------------------*
      *                      * Se riga corpo variata                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Rewrite [zbi] relativo alla dipen-  *
      *                          * denza                               *
      *                          *-------------------------------------*
           perform   rew-rec-zbd-000      thru rew-rec-zbd-999        .
      *                          *-------------------------------------*
      *                          * Riciclo a lettura riga corpo suc-   *
      *                          * cessiva                             *
      *                          *-------------------------------------*
           go to     scr-mov-fil-300.
       scr-mov-fil-700.
      *              *-------------------------------------------------*
      *              * Trattamento file [zbi] generale                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-800.
      *                      *-----------------------------------------*
      *                      * Write record [zbi] generale             *
      *                      *-----------------------------------------*
           perform   wrt-rec-zbg-000      thru wrt-rec-zbg-999        .
           go to     scr-mov-fil-999.
       scr-mov-fil-800.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rewrite record [zbi] generale           *
      *                      *-----------------------------------------*
           perform   rew-rec-zbg-000      thru rew-rec-zbg-999        .
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *-----------------------------------------------------------*
       del-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Cancellazione righe                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su righe corpo                        *
      *                  *---------------------------------------------*
           move      "ST"                 to   w-cat-rig-ope          .
           move      1                    to   w-cat-rig-num          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
      *                  *---------------------------------------------*
      *                  * Se start non valida : a testata             *
      *                  *---------------------------------------------*
           if        w-cat-rig-exs        not  = spaces
                     go to del-mov-fil-500.
       del-mov-fil-100.
      *                  *---------------------------------------------*
      *                  * Lettura sequenziale righe corpo             *
      *                  *---------------------------------------------*
           move      "RN"                 to   w-cat-rig-ope          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
      *                  *---------------------------------------------*
      *                  * Se at end : a testata                       *
      *                  *---------------------------------------------*
           if        w-cat-rig-exs        not  = spaces
                     go to del-mov-fil-500.
      *                  *---------------------------------------------*
      *                  * Movimento da buffer catena movimenti a work *
      *                  *---------------------------------------------*
           move      w-cat-rig-buf        to   w-rig                  .
      *                  *---------------------------------------------*
      *                  * Delete [zbi] relativo alla dipendenza       *
      *                  *---------------------------------------------*
           perform   del-rec-zbd-000      thru del-rec-zbd-999        .
      *                  *---------------------------------------------*
      *                  * Riciclo a lettura riga corpo successiva     *
      *                  *---------------------------------------------*
           go to     del-mov-fil-100.
       del-mov-fil-500.
      *              *-------------------------------------------------*
      *              * Cancellazione file [zbi] generale               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Delete record [zbi] generale                *
      *                  *---------------------------------------------*
           perform   del-rec-zbg-000      thru del-rec-zbg-999        .
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [zbi] relativo alla dipendenza        *
      *    *-----------------------------------------------------------*
       cmp-rec-zbd-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofzbi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbi                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           move      w-tes-cod-tmb        to   rf-zbi-cod-tmb         .
           move      w-rig-cod-dpz (1)    to   rf-zbi-cod-dpz         .
           move      w-rig-cod-dsl (1)    to   rf-zbi-cod-dsl         .
           move      w-rig-alx-dpz (1)    to   rf-zbi-alx-dpz         .
       cmp-rec-zbd-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [zbi] relativo alla dipendenza           *
      *    *-----------------------------------------------------------*
       wrt-rec-zbd-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-zbd-000      thru cmp-rec-zbd-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofzbi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbi                 .
       wrt-rec-zbd-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record righe [zbi] relativo alla dipendenza   *
      *    *-----------------------------------------------------------*
       rew-rec-zbd-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-zbd-000      thru cmp-rec-zbd-999        .
      *              *-------------------------------------------------*
      *              * Force Put record                                *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofzbi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbi                 .
       rew-rec-zbd-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [zbi] relativo alla dipendenza       *
      *    *-----------------------------------------------------------*
       del-rec-zbd-000.
      *              *-------------------------------------------------*
      *              * Composizione chiave primaria                    *
      *              *-------------------------------------------------*
           move      w-tes-cod-tmb        to   rf-zbi-cod-tmb         .
           move      w-rig-cod-dpz (2)    to   rf-zbi-cod-dpz         .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofzbi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbi                 .
       del-rec-zbd-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [zbi] generale                        *
      *    *-----------------------------------------------------------*
       cmp-rec-zbg-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofzbi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbi                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           move      w-tes-cod-tmb        to   rf-zbi-cod-tmb         .
           move      zero                 to   rf-zbi-cod-dpz         .
           move      w-tes-des-tmb (1)    to   rf-zbi-des-tmb         .
           move      w-tes-des-key (1)    to   rf-zbi-des-key         .
           move      w-tes-des-stp (1)    to   rf-zbi-des-stp         .
           move      w-tes-pwd-tmb (1)    to   rf-zbi-pwd-tmb         .
           move      w-tes-int-ftr (1)    to   rf-zbi-int-ftr         .
           move      w-tes-tmo-ftr (1)    to   rf-zbi-tmo-ftr         .
           move      w-tes-snx-acm (1)    to   rf-zbi-snx-acm         .
           move      w-tes-def-tac (1)    to   rf-zbi-def-tac         .
           move      w-tes-def-ctr (1)    to   rf-zbi-def-ctr         .
           move      w-tes-cau-mag (1)    to   rf-zbi-cau-mag         .
           move      w-tes-cod-mic (1)    to   rf-zbi-cod-mic         .
           move      w-tes-cam-agg (1)    to   rf-zbi-cam-agg         .
           move      w-tes-def-tar (1)    to   rf-zbi-def-tar         .
           move      w-tes-snv-tar (1)    to   rf-zbi-snv-tar         .
           move      w-tes-lst-tar (1)    to   rf-zbi-lst-tar         .
           move      w-tes-org-doc (1)    to   rf-zbi-org-doc         .
           move      w-tes-prv-doc (1)    to   rf-zbi-prv-doc         .
           move      w-tes-sgl-num (1)    to   rf-zbi-sgl-num         .
           move      w-tes-mov-afd (1)    to   rf-zbi-mov-afd         .
           move      w-tes-def-tmf (1)    to   rf-zbi-def-tmf         .
           move      w-tes-snx-prz (1)    to   rf-zbi-snx-prz         .
           move      w-tes-snx-sco (1)    to   rf-zbi-snx-sco         .
           move      w-tes-snx-imp (1)    to   rf-zbi-snx-imp         .
           move      w-tes-snx-civ (1)    to   rf-zbi-snx-civ         .
           move      w-tes-snx-ttd (1)    to   rf-zbi-snx-ttd         .
           move      w-tes-snx-dct (1)    to   rf-zbi-snx-dct         .
           move      w-tes-des-dct (1)    to   rf-zbi-des-dct         .
           move      w-tes-num-cps (1)    to   rf-zbi-tip-sql         .
           move      w-tes-def-tva (1)    to   rf-zbi-pos-sql         .
           move      w-tes-snx-par (1)    to   rf-zbi-snx-par         .
           move      w-tes-vld-dpz (1)    to   rf-zbi-vld-dpz         .
           move      w-tes-snx-age (1)    to   rf-zbi-snx-age         .
           move      w-tes-snx-ndp (1)    to   rf-zbi-snx-ndp         .
           move      w-tes-snx-fop (1)    to   rf-zbi-snx-fop         .
           move      w-tes-snx-lib (1)    to   rf-zbi-snx-lib         .
           move      w-tes-snx-nmm (1)    to   rf-zbi-snx-nmm         .
           move      w-tes-snx-ncv (1)    to   rf-zbi-snx-ncv         .
           move      w-tes-tmo-ft2 (1)    to   rf-zbi-tmo-ft2         .
           move      w-tes-cod-lst (1)    to   rf-zbi-cod-lst         .
           move      w-tes-snx-nmc (1)    to   rf-zbi-snx-nmc         .
           move      w-tes-alx-gen (1)    to   rf-zbi-alx-gen         .
           move      w-tes-cod-dsl (1)    to   rf-zbi-cod-dsl         .
           move      w-tes-def-tpr (1)    to   rf-zbi-def-tpr         .
       cmp-rec-zbg-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [zbi] generale                           *
      *    *-----------------------------------------------------------*
       wrt-rec-zbg-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-zbg-000      thru cmp-rec-zbg-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofzbi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbi                 .
       wrt-rec-zbg-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [zbi] generale                         *
      *    *-----------------------------------------------------------*
       rew-rec-zbg-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-zbg-000      thru cmp-rec-zbg-999        .
      *              *-------------------------------------------------*
      *              * Force Put record                                *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofzbi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbi                 .
       rew-rec-zbg-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [zbi] generale                       *
      *    *-----------------------------------------------------------*
       del-rec-zbg-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-zbg-000      thru cmp-rec-zbg-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofzbi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbi                 .
       del-rec-zbg-999.
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
      *    * Routine di lettura archivio [ada]                         *
      *    *-----------------------------------------------------------*
       let-ana-dpz-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-ada-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-ada-cod    =    zero
                     go to let-ana-dpz-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODDPZ    "         to   f-key                  .
           move      w-let-arc-ada-cod    to   rf-ada-cod-dpz         .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-ana-dpz-400.
       let-ana-dpz-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-ada-cod-mne       to   w-let-arc-ada-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-ana-dpz-999.
       let-ana-dpz-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-ada-flg      .
           move      all   "."            to   w-let-arc-ada-des      .
           go to     let-ana-dpz-999.
       let-ana-dpz-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-ada-des      .
       let-ana-dpz-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zfi]                         *
      *    *-----------------------------------------------------------*
       let-arc-zfi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zfi-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spazi                          *
      *              *-------------------------------------------------*
           if        w-let-arc-zfi-cod    =    spaces
                     go to let-arc-zfi-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODTMO"             to   f-key                  .
           move      w-let-arc-zfi-cod    to   rf-zfi-cod-tmo         .
           move      "pgm/fat/fls/ioc/obj/iofzfi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfi                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zfi-400.
       let-arc-zfi-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zfi-des-tmo       to   w-let-arc-zfi-des      .
           move      rf-zfi-vld-dpz       to   w-let-arc-zfi-vld      .
           move      rf-zfi-cod-dpz       to   w-let-arc-zfi-dpz      .
           move      rf-zfi-tip-doc       to   w-let-arc-zfi-tdo      .
           move      rf-zfi-org-doc       to   w-let-arc-zfi-ord      .
           move      rf-zfi-prv-doc       to   w-let-arc-zfi-prd      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zfi-999.
       let-arc-zfi-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      all  "."             to   w-let-arc-zfi-des      .
           move      "#"                  to   w-let-arc-zfi-flg      .
           go to     let-arc-zfi-600.
       let-arc-zfi-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zfi-des      .
       let-arc-zfi-600.
           move      zero                 to   w-let-arc-zfi-vld      .
           move      zero                 to   w-let-arc-zfi-dpz      .
           move      zero                 to   w-let-arc-zfi-tdo      .
           move      zero                 to   w-let-arc-zfi-ord      .
           move      zero                 to   w-let-arc-zfi-prd      .
       let-arc-zfi-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zct]                         *
      *    *-----------------------------------------------------------*
       let-arc-zct-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zct-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spazi                          *
      *              *-------------------------------------------------*
           if        w-let-arc-zct-cod    =    spaces
                     go to let-arc-zct-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCTR"             to   f-key                  .
           move      w-let-arc-zct-cod    to   rf-zct-cod-ctr         .
           move      "pgm/bol/fls/ioc/obj/iofzct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zct                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zct-400.
       let-arc-zct-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zct-des-ctr       to   w-let-arc-zct-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zct-999.
       let-arc-zct-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zct-flg      .
           move      all   "."            to   w-let-arc-zct-des      .
           go to     let-arc-zct-999.
       let-arc-zct-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zct-des      .
       let-arc-zct-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zmc]                             *
      *    *-----------------------------------------------------------*
       let-arc-zmc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zmc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice causale a zero                   *
      *              *-------------------------------------------------*
           if        w-let-arc-zmc-cod    =    zero
                     go to let-arc-zmc-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCAU    "         to   f-key                  .
           move      w-let-arc-zmc-cod    to   rf-zmc-cod-cau         .
           move      "pgm/mag/fls/ioc/obj/iofzmc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zmc                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zmc-400.
       let-arc-zmc-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zmc-des-cau       to   w-let-arc-zmc-des      .
           move      rf-zmc-trt-val       to   w-let-arc-zmc-trv      .
           move      rf-zmc-tip-mdm       to   w-let-arc-zmc-mdm      .
           move      rf-zmc-trt-mic       to   w-let-arc-zmc-ttc      .
           move      rf-zmc-tip-mic       to   w-let-arc-zmc-tpc      .
           move      rf-zmc-cod-mic       to   w-let-arc-zmc-cdc      .
           move      rf-zmc-snv-mic       to   w-let-arc-zmc-vac      .
           move      rf-zmc-def-tar       to   w-let-arc-zmc-dfa      .
           move      rf-zmc-snv-tar       to   w-let-arc-zmc-vaa      .
           move      rf-zmc-lst-tar       to   w-let-arc-zmc-lsa      .
           move      rf-zmc-def-tco       to   w-let-arc-zmc-dfm      .
           move      rf-zmc-snv-tco       to   w-let-arc-zmc-vam      .
           move      rf-zmc-lst-tco       to   w-let-arc-zmc-lsm      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zmc-999.
       let-arc-zmc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zmc-flg      .
           move      all   "."            to   w-let-arc-zmc-des      .
           go to     let-arc-zmc-600.
       let-arc-zmc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zmc-des      .
       let-arc-zmc-600.
           move      spaces               to   w-let-arc-zmc-trv      .
           move      zero                 to   w-let-arc-zmc-mdm      .
           move      zero                 to   w-let-arc-zmc-ttc      .
           move      spaces               to   w-let-arc-zmc-tpc      .
           move      spaces               to   w-let-arc-zmc-cdc      .
           move      spaces               to   w-let-arc-zmc-vac      .
           move      spaces               to   w-let-arc-zmc-dfa      .
           move      spaces               to   w-let-arc-zmc-vaa      .
           move      spaces               to   w-let-arc-zmc-lsa      .
           move      spaces               to   w-let-arc-zmc-dfm      .
           move      spaces               to   w-let-arc-zmc-vam      .
           move      spaces               to   w-let-arc-zmc-lsm      .
       let-arc-zmc-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zmm]                             *
      *    *-----------------------------------------------------------*
       let-arc-zmm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zmm-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice conto merce a Spaces             *
      *              *-------------------------------------------------*
           if        w-let-arc-zmm-cod    =    spaces
                     go to let-arc-zmm-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCTM    "         to   f-key                  .
           move      w-let-arc-zmm-cod    to   rf-zmm-cod-ctm         .
           move      "pgm/mag/fls/ioc/obj/iofzmm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zmm                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zmm-400.
       let-arc-zmm-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zmm-des-ctm       to   w-let-arc-zmm-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zmm-999.
       let-arc-zmm-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zmm-flg      .
           move      all   "."            to   w-let-arc-zmm-des      .
           go to     let-arc-zmm-999.
       let-arc-zmm-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zmm-des      .
       let-arc-zmm-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zub]                         *
      *    *-----------------------------------------------------------*
       let-arc-zub-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zub-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice ubicazione a spaces              *
      *              *-------------------------------------------------*
           if        w-let-arc-zub-cod    =    spaces
                     go to let-arc-zub-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODUBI    "         to   f-key                  .
           move      w-let-arc-zub-dpz    to   rf-zub-cod-dpz         .
           move      w-let-arc-zub-cod    to   rf-zub-cod-ubi         .
           move      "pgm/mag/fls/ioc/obj/iofzub"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zub                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zub-400.
       let-arc-zub-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zub-des-ubi       to   w-let-arc-zub-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zub-999.
       let-arc-zub-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zub-flg      .
           move      all   "."            to   w-let-arc-zub-des      .
           go to     let-arc-zub-600.
       let-arc-zub-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zub-des      .
       let-arc-zub-600.
       let-arc-zub-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zoc]                         *
      *    *-----------------------------------------------------------*
       let-arc-zoc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zoc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spazi                          *
      *              *-------------------------------------------------*
           if        w-let-arc-zoc-cod    =    spaces
                     go to let-arc-zoc-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODTOC"             to   f-key                  .
           move      w-let-arc-zoc-cod    to   rf-zoc-cod-toc         .
           move      "pgm/orc/fls/ioc/obj/iofzoc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zoc                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zoc-400.
       let-arc-zoc-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zoc-des-toc       to   w-let-arc-zoc-des      .
           move      rf-zoc-vld-dpz       to   w-let-arc-zoc-vld      .
           move      rf-zoc-cod-dpz       to   w-let-arc-zoc-dpz      .
           move      rf-zoc-org-doc       to   w-let-arc-zoc-ord      .
           move      rf-zoc-prv-doc       to   w-let-arc-zoc-prd      .
           move      rf-zoc-sgl-num       to   w-let-arc-zoc-sgl      .
           move      rf-zoc-def-tpr       to   w-let-arc-zoc-dtr      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zoc-999.
       let-arc-zoc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zoc-flg      .
           move      all   "."            to   w-let-arc-zoc-des      .
           go to     let-arc-zoc-600.
       let-arc-zoc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zoc-des      .
       let-arc-zoc-600.
           move      spaces               to   w-let-arc-zoc-des      .
           move      zero                 to   w-let-arc-zoc-vld      .
           move      zero                 to   w-let-arc-zoc-dpz      .
           move      zero                 to   w-let-arc-zoc-ord      .
           move      zero                 to   w-let-arc-zoc-prd      .
           move      spaces               to   w-let-arc-zoc-sgl      .
           move      spaces               to   w-let-arc-zoc-dtr      .
       let-arc-zoc-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zsc]                         *
      *    *-----------------------------------------------------------*
       let-arc-zsc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zsc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spazi                          *
      *              *-------------------------------------------------*
           if        w-let-arc-zsc-cod    =    spaces
                     go to let-arc-zsc-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODTMS"             to   f-key                  .
           move      w-let-arc-zsc-cod    to   rf-zsc-cod-tos         .
           move      "pgm/ods/fls/ioc/obj/iofzsc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsc                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zsc-400.
       let-arc-zsc-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zsc-des-tos       to   w-let-arc-zsc-des      .
           move      rf-zsc-vld-dpz       to   w-let-arc-zsc-vld      .
           move      rf-zsc-cod-dpz       to   w-let-arc-zsc-dpz      .
           move      rf-zsc-mov-afd       to   w-let-arc-zsc-maf      .
           move      rf-zsc-def-tmf       to   w-let-arc-zsc-dmf      .
           move      rf-zsc-tip-arc       to   w-let-arc-zsc-tar      .
           move      rf-zsc-def-tpr       to   w-let-arc-zsc-dtr      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zsc-999.
       let-arc-zsc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zsc-flg      .
           move      all   "."            to   w-let-arc-zsc-des      .
           go to     let-arc-zsc-600.
       let-arc-zsc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zsc-des      .
       let-arc-zsc-600.
           move      zero                 to   w-let-arc-zsc-vld      .
           move      zero                 to   w-let-arc-zsc-dpz      .
           move      zero                 to   w-let-arc-zsc-maf      .
           move      spaces               to   w-let-arc-zsc-dmf      .
           move      spaces               to   w-let-arc-zsc-tar      .
           move      spaces               to   w-let-arc-zsc-dtr      .
       let-arc-zsc-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zls]                             *
      *    *-----------------------------------------------------------*
       let-arc-zls-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zls-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice listino a Spaces                 *
      *              *-------------------------------------------------*
           if        w-let-arc-zls-cod    not  = spaces
                     go to let-arc-zls-400.
       let-arc-zls-200.
      *              *-------------------------------------------------*
      *              * Se codice listino a Spaces                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Descrizione listino                         *
      *                  *---------------------------------------------*
           move      "Listino Base        "
                                          to   w-let-arc-zls-des      .
      *                  *---------------------------------------------*
      *                  * Valuta per il listino, valuta base          *
      *                  *---------------------------------------------*
           move      c-sgl                to   w-let-arc-zls-vlt      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-zls-999.
       let-arc-zls-400.
      *              *-------------------------------------------------*
      *              * Se codice listino a non Spaces                  *
      *              *-------------------------------------------------*
       let-arc-zls-450.
      *                  *---------------------------------------------*
      *                  * Lettura per codice                          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODLST    "         to   f-key                  .
           move      w-let-arc-zls-cod    to   rf-zls-cod-lst         .
           move      "pgm/dcp/fls/ioc/obj/iofzls"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zls                 .
      *                  *---------------------------------------------*
      *                  * Deviazione secondo l'esito della lettura    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-arc-zls-550.
       let-arc-zls-500.
      *                  *---------------------------------------------*
      *                  * Se record esistente                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione listino                     *
      *                      *-----------------------------------------*
           move      rf-zls-des-lst       to   w-let-arc-zls-des      .
      *                      *-----------------------------------------*
      *                      * Valuta per il listino                   *
      *                      *-----------------------------------------*
           move      rf-zls-cod-vlt       to   w-let-arc-zls-vlt      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-arc-zls-999.
       let-arc-zls-550.
      *                  *---------------------------------------------*
      *                  * Se record non esistente                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita ad errore                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-let-arc-zls-flg      .
      *                      *-----------------------------------------*
      *                      * Descrizione listino                     *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-zls-des      .
      *                      *-----------------------------------------*
      *                      * Valuta per il listino                   *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-zls-vlt      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-arc-zls-999.
       let-arc-zls-999.
           exit.

      *    *===========================================================*
      *    * Routine di controllo codice dipendenza                    *
      *    *-----------------------------------------------------------*
       ctl-cod-dpz-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-ctl-cod-dpz-flg      .
      *              *-------------------------------------------------*
      *              * Salvataggio area catena                         *
      *              *-------------------------------------------------*
           move      w-cat-rig            to   w-sav-cat-rig          .
      *              *-------------------------------------------------*
      *              * Salvataggio area w-rig                          *
      *              *-------------------------------------------------*
           move      w-rig                to   w-sav-rig              .
      *              *-------------------------------------------------*
      *              * Start su righe corpo                            *
      *              *-------------------------------------------------*
           move      "ST"                 to   w-cat-rig-ope          .
           move      1                    to   w-cat-rig-num          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
      *              *-------------------------------------------------*
      *              * Se start non valida : uscita                    *
      *              *-------------------------------------------------*
           if        w-cat-rig-exs        not  = spaces
                     go to ctl-cod-dpz-900.
       ctl-cod-dpz-100.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale righe corpo                 *
      *              *-------------------------------------------------*
           move      "RN"                 to   w-cat-rig-ope          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
      *              *-------------------------------------------------*
      *              * Se at end : uscita                              *
      *              *-------------------------------------------------*
           if        w-cat-rig-exs        not  = spaces
                     go to ctl-cod-dpz-900.
      *              *-------------------------------------------------*
      *              * Movimento da buffer catena movimenti a work     *
      *              *-------------------------------------------------*
           move      w-cat-rig-buf        to   w-rig                  .
      *              *-------------------------------------------------*
      *              * Controllo codice dipendenza con quello passato  *
      *              *-------------------------------------------------*
           if        w-rig-cod-dpz (1)    =    w-ctl-cod-dpz-dpz
                     move  "#"            to   w-ctl-cod-dpz-flg
                     go to ctl-cod-dpz-900.
      *              *-------------------------------------------------*
      *              * Riciclo su lettura sequenziale righe corpo      *
      *              *-------------------------------------------------*
           go to     ctl-cod-dpz-100.
       ctl-cod-dpz-900.
      *              *-------------------------------------------------*
      *              * Ripristino area catena                          *
      *              *-------------------------------------------------*
           move      w-sav-cat-rig        to   w-cat-rig              .
      *              *-------------------------------------------------*
      *              * Ripristino area w-rig                           *
      *              *-------------------------------------------------*
           move      w-sav-rig            to   w-rig                  .
       ctl-cod-dpz-999.
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
           copy      "pgm/bol/prg/cpy/acdezbi0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione tipo movimento per la fat- *
      *    * turazione                                                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/acdezfi0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione causale del trasporto      *
      *    *-----------------------------------------------------------*
           copy      "pgm/bol/prg/cpy/acdezct0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice causale di ge-  *
      *    * stione magazzino                                          *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/acmnzmc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice conto merce     *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/acdezmm0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice dislocazione    *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/acdezub0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per accettazione codice dipendenza dell'a-    *
      *    * zienda                                                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/acoddpz0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione tipo movimento per ordini  *
      *    * clienti                                                   *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/acdezoc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione tipo movimento per spedi-  *
      *    * zioni clienti                                             *
      *    *-----------------------------------------------------------*
           copy      "pgm/ods/prg/cpy/acdezsc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice listino             *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acodzls0.acs"                   .

