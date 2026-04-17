       Identification Division.
       Program-Id.                                 porc9400           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    orc                 *
      *                                Settore:    uti                 *
      *                                   Fase:    orc940              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 01/03/96    *
      *                       Ultima revisione:    NdK del 07/06/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Forzatura date consegna ordine cliente      *
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
                     "orc"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "uti"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "orc940"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "porc9400"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "     FORZATURA DATE CONSEGNA ORDINE     "       .

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
      *    * Area di definizione della valuta base                     *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/c"                                  .

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
      *        * [oct]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfoct"                          .
      *        *-------------------------------------------------------*
      *        * [ocr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfocr"                          .
      *        *-------------------------------------------------------*
      *        * [ocx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfocx"                          .
      *        *-------------------------------------------------------*
      *        * [ocp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfocp"                          .
      *        *-------------------------------------------------------*
      *        * [zoc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfzoc"                          .
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .

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
      *            * Numero protocollo                                 *
      *            *---------------------------------------------------*
               10  w-tes-num-prt          pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Tipo movimento ordini clienti                     *
      *            *---------------------------------------------------*
               10  w-tes-tmo-orc          pic  x(05)                  .
               10  w-tes-tmo-orc-des      pic  x(30)                  .
               10  w-tes-tmo-orc-vld      pic  9(02)                  .
               10  w-tes-tmo-orc-dpz      pic  9(02)                  .
               10  w-tes-tmo-orc-ord      pic  9(02)                  .
               10  w-tes-tmo-orc-prd      pic  9(02)                  .
               10  w-tes-tmo-orc-sgl      pic  x(03)                  .
               10  w-tes-tmo-orc-dtr      pic  x(05)                  .
      *            *---------------------------------------------------*
      *            * Data documento                                    *
      *            *---------------------------------------------------*
               10  w-tes-dat-doc          pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Numero documento                                  *
      *            *---------------------------------------------------*
               10  w-tes-num-doc          pic  9(11)                  .
               10  w-tes-num-doc-r redefines
                   w-tes-num-doc.
                   15  w-tes-num-doc-saa  pic  9(03)                  .
                   15  w-tes-num-doc-dpz  pic  9(02)                  .
                   15  w-tes-num-doc-prg  pic  9(06)                  .
      *            *---------------------------------------------------*
      *            * Sigla numerazione                                 *
      *            *---------------------------------------------------*
               10  w-tes-sgl-num          pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
      *            *---------------------------------------------------*
      *            * Codice cliente                                    *
      *            *---------------------------------------------------*
               10  w-tes-cli-org          pic  9(07)                  .
               10  w-tes-cli-org-rag      pic  x(40)                  .
               10  w-tes-cli-org-via      pic  x(40)                  .
               10  w-tes-cli-org-loc      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Data consegna                                     *
      *            *---------------------------------------------------*
               10  w-tes-dat-cns          pic  9(07)                  .

      *    *===========================================================*
      *    * Work-area generica per tutto il programma                 *
      *    *-----------------------------------------------------------*
       01  w-gen.
           05  w-gen-dat-att              pic  9(07)                  .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
           05  filler                     pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area referenze                                       *
      *    *-----------------------------------------------------------*
       01  w-ref.
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
      *    * Work per valori di default                                *
      *    *-----------------------------------------------------------*
       01  w-def.
      *        *-------------------------------------------------------*
      *        * Data documento                                        *
      *        *-------------------------------------------------------*
           05  w-def-dat-doc              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Tipo movimento per ordine cliente                     *
      *        *-------------------------------------------------------*
           05  w-def-tmo-orc              pic  x(05)                  .
           05  w-def-tmo-orc-flg          pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Select                            *
      *    *-----------------------------------------------------------*
       01  w-slc.
      *        *-------------------------------------------------------*
      *        * Work per Select numero documento                      *
      *        *-------------------------------------------------------*
           05  w-slc-num-oct.
      *            *---------------------------------------------------*
      *            * Valori in entrata                                 *
      *            *---------------------------------------------------*
               10  w-slc-num-oct-dds      pic  9(07)                  .
               10  w-slc-num-oct-nds      pic  9(11)                  .
               10  w-slc-num-oct-nds-r redefines
                   w-slc-num-oct-nds.
                   15  w-slc-num-oct-nsa  pic  9(03)                  .
                   15  w-slc-num-oct-ndp  pic  9(02)                  .
                   15  w-slc-num-oct-npg  pic  9(06)                  .
               10  w-slc-num-oct-dpz      pic  9(02)                  .
               10  w-slc-num-oct-sgl      pic  x(03)                  .
               10  w-slc-num-oct-saa      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Valori in uscita                                  *
      *            *---------------------------------------------------*
               10  w-slc-num-oct-sel      pic  x(01)                  .
               10  w-slc-num-oct-toc      pic  x(05)                  .
               10  w-slc-num-oct-dat      pic  9(07)                  .
               10  w-slc-num-oct-num      pic  9(11)                  .
               10  w-slc-num-oct-prt      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Area di comodo                                    *
      *            *---------------------------------------------------*
               10  w-slc-num-oct-c01      pic  9(02)                  .
               10  w-slc-num-oct-c02      pic  9(02)                  .
               10  w-slc-num-oct-c03      pic  9(02)                  .
               10  w-slc-num-oct-c04      pic  9(02)                  .
               10  w-slc-num-oct-c05      pic  9(02)                  .
               10  w-slc-num-oct-nli      pic  9(02)                  .
               10  w-slc-num-oct-crb      pic  9(02)                  .
               10  w-slc-num-oct-cpb      pic  9(02)                  .
               10  w-slc-num-oct-cpa      pic  9(02)                  .
               10  w-slc-num-oct-buf
                               occurs 30.
                   15  w-slc-num-oct-bpt  pic  9(11)                  .
               10  w-slc-num-oct-ltp.
                   15  filler             pic  x(07) value "Pagina "  .
                   15  w-slc-num-oct-lt1  pic  9(01)                  .
                   15  filler             pic  x(04) value " di "     .
                   15  w-slc-num-oct-lt2  pic  9(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Find                              *
      *    *-----------------------------------------------------------*
       01  w-fnd.
      *        *-------------------------------------------------------*
      *        * Work per Find su archivio [oct]                       *
      *        *-------------------------------------------------------*
           05  w-fnd-arc-oct.
               10  w-fnd-arc-oct-sel      pic  x(01)                  .
               10  w-fnd-arc-oct-toc      pic  x(05)                  .
               10  w-fnd-arc-oct-dat      pic  9(07)                  .
               10  w-fnd-arc-oct-num      pic  9(11)                  .
               
      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
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
      *        * Work per Let su archivio [cli]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-cli.
               10  w-let-arc-cli-flg      pic  x(01)                  .
               10  w-let-arc-cli-cod      pic  9(07)                  .
               10  w-let-arc-cli-rag      pic  x(40)                  .
               10  w-let-arc-cli-via      pic  x(40)                  .
               10  w-let-arc-cli-loc      pic  x(40)                  .
               10  w-let-arc-cli-piv      pic  9(11)                  .
               10  w-let-arc-cli-stc      pic  9(07)                  .
               10  w-let-arc-cli-ass      pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcc.
               10  w-let-arc-dcc-flg      pic  x(01)                  .
               10  w-let-arc-dcc-cli      pic  9(07)                  .
               10  w-let-arc-dcc-dpz      pic  x(04)                  .
               10  w-let-arc-dcc-rag      pic  x(40)                  .
               10  w-let-arc-dcc-via      pic  x(40)                  .
               10  w-let-arc-dcc-loc      pic  x(40)                  .
               10  w-let-arc-dcc-vlt      pic  x(03)                  .
               10  w-let-arc-dcc-lng      pic  x(03)                  .
               10  w-let-arc-dcc-tai      pic  9(02)                  .
               10  w-let-arc-dcc-cbl      pic  9(02)                  .
               10  w-let-arc-dcc-obl      pic  9(02)                  .
               10  w-let-arc-dcc-tpf      pic  9(02)                  .
               10  w-let-arc-dcc-apf      pic  9(07)                  .
               10  w-let-arc-dcc-dpf      pic  x(04)                  .
               10  w-let-arc-dcc-tfz      pic  9(02)                  .
               10  w-let-arc-dcc-tdp      pic  9(02)                  .
               10  w-let-arc-dcc-tus      pic  9(02)                  .
               10  w-let-arc-dcc-tud      pic  9(07)                  .
               10  w-let-arc-dcc-tuc      pic  9(07)                  .

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
      *    * Link-area per accettazione tipo movimento ordini clienti  *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/acdezoc0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione stato ordine     *
      *    * cliente                                                   *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/dstsorc0.dtl"                   .

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
      *              * Test se programma eseguibile                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su flag di eventuale visualizzazione   *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-vis-sgr    not  = "V"
                     go to pre-exe-pgm-100.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Programma non eseguibile dall'utente !            
      -              "               "    to   w-err-box-err-msg      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione messaggio di errore         *
      *                  *---------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-pre-exe-pgm      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pre-exe-pgm-999.
       pre-exe-pgm-100.
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
           move      w-gen-dat-att        to   w-def-dat-doc          .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
      *              *-------------------------------------------------*
      *              * Lettura referenze                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Referenza per default tipo movimento        *
      *                  *---------------------------------------------*
           perform   ref-cod-tmo-000      thru ref-cod-tmo-999        .
      *              *-------------------------------------------------*
      *              * Normalizzazione default per tipo movimento      *
      *              *-------------------------------------------------*
           move      w-ref-cod-tmo-cod    to   w-def-tmo-orc          .
      *              *-------------------------------------------------*
      *              * Open moduli accettazione                        *
      *              *-------------------------------------------------*
           perform   opn-mdl-acc-000      thru opn-mdl-acc-999        .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura delle referenze relative al default per il tipo   *
      *    * movimento                                                 *
      *    *-----------------------------------------------------------*
       ref-cod-tmo-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione work-area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-ref-cod-tmo-cod      .
           move      spaces               to   w-ref-cod-tmo-trt      .
       ref-cod-tmo-200.
      *              *-------------------------------------------------*
      *              * Lettura referenza                               *
      *              *-------------------------------------------------*
           move      "R:"                 to   s-ope                  .
           move      "pgm/orc/mov/orc300[def-tmo]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-ref-cod-tmo
           else      move  spaces         to   w-ref-cod-tmo          .
       ref-cod-tmo-400.
      *              *-------------------------------------------------*
      *              * Normalizzazione valori letti                    *
      *              *-------------------------------------------------*
           if        w-ref-cod-tmo-trt    not  = spaces and
                     w-ref-cod-tmo-trt    not  = "I"    and
                     w-ref-cod-tmo-trt    not  = "C"    and
                     w-ref-cod-tmo-trt    not  = "O"
                     move  spaces         to   w-ref-cod-tmo-trt      .
       ref-cod-tmo-420.
           if        w-ref-cod-tmo-trt    =    spaces
                     move  spaces         to   w-ref-cod-tmo-cod      .
       ref-cod-tmo-999.
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
      *              * Open modulo accettazione tipo movimento per gli *
      *              * ordini clienti                                  *
      *              *-------------------------------------------------*
           perform   cod-des-zoc-opn-000  thru cod-des-zoc-opn-999    .
       opn-mdl-acc-999.
           exit.

      *    *===========================================================*
      *    * Close moduli di accettazione                              *
      *    *-----------------------------------------------------------*
       cls-mdl-acc-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione tipo movimento per    *
      *              * ordini clienti                                  *
      *              *-------------------------------------------------*
           perform   cod-des-zoc-cls-000  thru cod-des-zoc-cls-999    .
       cls-mdl-acc-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * [oct]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *              *-------------------------------------------------*
      *              * [ocr]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *              *-------------------------------------------------*
      *              * [ocx]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocx                 .
      *              *-------------------------------------------------*
      *              * [ocp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocp                 .
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
      *              * [cli]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *              *-------------------------------------------------*
      *              * [dcc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione status ordine     *
      *              *-------------------------------------------------*
           perform   det-sts-orc-opn-000  thru det-sts-orc-opn-999    .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [oct]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *              *-------------------------------------------------*
      *              * [ocr]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *              *-------------------------------------------------*
      *              * [ocx]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocx                 .
      *              *-------------------------------------------------*
      *              * [ocp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocp                 .
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
      *              * [cli]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *              *-------------------------------------------------*
      *              * [dcc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione status ordine    *
      *              *-------------------------------------------------*
           perform   det-sts-orc-cls-000  thru det-sts-orc-cls-999    .
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
      *                  * Tipo movimento ordini clienti               *
      *                  *---------------------------------------------*
           perform   acc-tmo-orc-000      thru acc-tmo-orc-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
       acc-key-reg-200.
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           perform   acc-dat-doc-000      thru acc-dat-doc-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
           if        v-key                =    "UP  "
                     go to acc-key-reg-100.
       acc-key-reg-300.
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           perform   acc-num-doc-000      thru acc-num-doc-999        .
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
      *              * Tipo movimento                                  *
      *              *-------------------------------------------------*
           perform   vis-tmo-orc-000      thru vis-tmo-orc-999        .
           perform   vis-tmo-orc-des-000  thru vis-tmo-orc-des-999    .
      *              *-------------------------------------------------*
      *              * Data documento                                  *
      *              *-------------------------------------------------*
           perform   vis-dat-doc-000      thru vis-dat-doc-999        .
      *              *-------------------------------------------------*
      *              * Numero documento                                *
      *              *-------------------------------------------------*
           perform   vis-num-doc-000      thru vis-num-doc-999        .
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
      *                  * Tipo movimento                              *
      *                  *---------------------------------------------*
           perform   pmt-tmo-orc-000      thru pmt-tmo-orc-999        .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           perform   pmt-dat-doc-000      thru pmt-dat-doc-999        .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           perform   pmt-num-doc-000      thru pmt-num-doc-999        .
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
      *    * Visualizzazione prompts per tipo movimento                *
      *    *-----------------------------------------------------------*
       pmt-tmo-orc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo movimento :"   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-tmo-orc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per data documento                *
      *    *-----------------------------------------------------------*
       pmt-dat-doc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      56                   to   v-pos                  .
           move      "Data documento :"   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-dat-doc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per numero documento              *
      *    *-----------------------------------------------------------*
       pmt-num-doc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      56                   to   v-pos                  .
           move      "Nr.  documento :"   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-num-doc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Tipo movimento per ordini     *
      *    * clienti                                                   *
      *    *-----------------------------------------------------------*
       acc-tmo-orc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione valore di default              *
      *                  *---------------------------------------------*
           perform   acc-tmo-orc-def-000  thru acc-tmo-orc-def-999    .
           if        w-def-tmo-orc-flg    not  = spaces
                     go to acc-tmo-orc-400.
       acc-tmo-orc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-zoc-ope      .
           move      w-tes-tmo-orc        to   w-cod-des-zoc-cod      .
           move      04                   to   w-cod-des-zoc-lin      .
           move      18                   to   w-cod-des-zoc-pos      .
           move      04                   to   w-cod-des-zoc-dln      .
           move      24                   to   w-cod-des-zoc-dps      .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-des-zoc-cll-000  thru cod-des-zoc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-zoc-foi-000  thru cod-des-zoc-foi-999    .
       acc-tmo-orc-110.
           perform   cod-des-zoc-cll-000  thru cod-des-zoc-cll-999    .
           if        w-cod-des-zoc-ope    =    "F+"
                     go to acc-tmo-orc-115.
           if        w-cod-des-zoc-ope    =    "AC"
                     go to acc-tmo-orc-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-tmo-orc-115.
           perform   cod-des-zoc-foi-000  thru cod-des-zoc-foi-999    .
           go to     acc-tmo-orc-110.
       acc-tmo-orc-120.
           move      w-cod-des-zoc-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-tmo-orc-999.
       acc-tmo-orc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-tmo-orc          .
       acc-tmo-orc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      w-tes-tmo-orc        to   w-all-str-alf          .
           move      05                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-tmo-orc-100.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zoc]                      *
      *                  *---------------------------------------------*
           move      w-tes-tmo-orc        to   w-let-arc-zoc-cod      .
           perform   let-arc-zoc-000      thru let-arc-zoc-999        .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori associati al tipo    *
      *                  * movimento ordini clienti                    *
      *                  *---------------------------------------------*
           move      w-let-arc-zoc-des    to   w-tes-tmo-orc-des      .
           move      w-let-arc-zoc-vld    to   w-tes-tmo-orc-vld      .
           move      w-let-arc-zoc-dpz    to   w-tes-tmo-orc-dpz      .
           move      w-let-arc-zoc-ord    to   w-tes-tmo-orc-ord      .
           move      w-let-arc-zoc-prd    to   w-tes-tmo-orc-prd      .
           move      w-let-arc-zoc-sgl    to   w-tes-tmo-orc-sgl      .
           move      w-let-arc-zoc-dtr    to   w-tes-tmo-orc-dtr      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-tmo-orc-des-000  thru vis-tmo-orc-des-999    .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zoc-flg    not  = spaces
                     go to acc-tmo-orc-100.
      *                  *---------------------------------------------*
      *                  * Se a spaces : oltre                         *
      *                  *---------------------------------------------*
           if        w-tes-tmo-orc        =    spaces
                     go to acc-tmo-orc-600.
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza                   *
      *                  *---------------------------------------------*
           if        w-tes-tmo-orc-vld    not  = 02
                     go to acc-tmo-orc-420.
           if        w-tes-cod-dpz        =    w-tes-tmo-orc-dpz
                     go to acc-tmo-orc-420.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Tipo movimento incompatibile con il codice dipende
      -              "nza            "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-tmo-orc-100.
       acc-tmo-orc-420.
      *                  *---------------------------------------------*
      *                  * Controllo tipo origine del documento        *
      *                  *---------------------------------------------*
           if        w-tes-tmo-orc-ord    =   01
                     go to acc-tmo-orc-600.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Tipo movimento non gestibile da questo programma !
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-tmo-orc-100.
       acc-tmo-orc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tmo-orc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-tmo-orc-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-tmo-orc-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-tmo-orc-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-tmo-orc-999.
       acc-tmo-orc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Tipo movimento                *
      *    *                                                           *
      *    * Subroutine di trattamento del default                     *
      *    *-----------------------------------------------------------*
       acc-tmo-orc-def-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di obbligatorieta'         *
      *              *-------------------------------------------------*
           move      spaces               to   w-def-tmo-orc-flg      .
       acc-tmo-orc-def-100.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di trattamento  *
      *              * da referenza                                    *
      *              *-------------------------------------------------*
           if        w-ref-cod-tmo-trt    =    spaces
                     go to acc-tmo-orc-def-200
           else if   w-ref-cod-tmo-trt    =    "I"
                     go to acc-tmo-orc-def-300
           else if   w-ref-cod-tmo-trt    =    "C"
                     go to acc-tmo-orc-def-400
           else if   w-ref-cod-tmo-trt    =    "O"
                     go to acc-tmo-orc-def-500
           else      go to acc-tmo-orc-def-900.
       acc-tmo-orc-def-200.
      *              *-------------------------------------------------*
      *              * Se nessun default                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se default da utilizzare               *
      *                  *---------------------------------------------*
           if        w-tes-tmo-orc        not  = spaces
                     go to acc-tmo-orc-def-900.
      *                  *---------------------------------------------*
      *                  * Utilizzo del default                        *
      *                  *---------------------------------------------*
           move      w-def-tmo-orc        to   w-tes-tmo-orc          .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     acc-tmo-orc-def-900.
       acc-tmo-orc-def-300.
      *              *-------------------------------------------------*
      *              * Se default iniziale                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se default da utilizzare               *
      *                  *---------------------------------------------*
           if        w-tes-tmo-orc        not  = spaces
                     go to acc-tmo-orc-def-900.
      *                  *---------------------------------------------*
      *                  * Utilizzo del default                        *
      *                  *---------------------------------------------*
           move      w-def-tmo-orc        to   w-tes-tmo-orc          .
      *                  *---------------------------------------------*
      *                  * A lettura e visualizzazione                 *
      *                  *---------------------------------------------*
           go to     acc-tmo-orc-def-600.
       acc-tmo-orc-def-400.
      *              *-------------------------------------------------*
      *              * Se default continuo                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Utilizzo del default                        *
      *                  *---------------------------------------------*
           move      w-ref-cod-tmo-cod    to   w-tes-tmo-orc          .
      *                  *---------------------------------------------*
      *                  * A lettura e visualizzazione                 *
      *                  *---------------------------------------------*
           go to     acc-tmo-orc-def-600.
       acc-tmo-orc-def-500.
      *              *-------------------------------------------------*
      *              * Se default obbligatorio                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Utilizzo del default                        *
      *                  *---------------------------------------------*
           move      w-def-tmo-orc        to   w-tes-tmo-orc          .
      *                  *---------------------------------------------*
      *                  * Forzatura del flag di obbligatorieta'       *
      *                  *---------------------------------------------*
           move      "#"                  to   w-def-tmo-orc-flg      .
      *                  *---------------------------------------------*
      *                  * A lettura e visualizzazione                 *
      *                  *---------------------------------------------*
           go to     acc-tmo-orc-def-600.
       acc-tmo-orc-def-600.
      *              *-------------------------------------------------*
      *              * Lettura descrizione tipo movimento              *
      *              *-------------------------------------------------*
           move      w-tes-tmo-orc        to   w-let-arc-zoc-cod      .
           perform   let-arc-zoc-000      thru let-arc-zoc-999        .
      *              *-------------------------------------------------*
      *              * Bufferizzazione descrizione tipo movimento      *
      *              *-------------------------------------------------*
           move      w-let-arc-zoc-des    to   w-tes-tmo-orc-des      .
      *              *-------------------------------------------------*
      *              * Visualizzazione tipo movimento                  *
      *              *-------------------------------------------------*
           perform   vis-tmo-orc-000      thru vis-tmo-orc-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione descrizione tipo movimento      *
      *              *-------------------------------------------------*
           perform   vis-tmo-orc-des-000  thru vis-tmo-orc-des-999    .
       acc-tmo-orc-def-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-tmo-orc-def-999.
       acc-tmo-orc-def-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : tipo movimento per ordini  *
      *    * clienti                                                   *
      *    *-----------------------------------------------------------*
       vis-tmo-orc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      18                   to   v-pos                  .
           move      w-tes-tmo-orc        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tmo-orc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : descrizione tipo movimento *
      *    * per ordini clienti                                        *
      *    *-----------------------------------------------------------*
       vis-tmo-orc-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      w-tes-tmo-orc-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tmo-orc-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Data documento                *
      *    *-----------------------------------------------------------*
       acc-dat-doc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuale preparazione del valore di de-    *
      *                  * fault                                       *
      *                  *---------------------------------------------*
           if        w-tes-dat-doc        =    zero
                     move  w-def-dat-doc  to   w-tes-dat-doc          .
       acc-dat-doc-100.
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
           move      w-tes-dat-doc        to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-dat-doc-999.
       acc-dat-doc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   w-tes-dat-doc          .
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-dat-doc-400.
      *                  *---------------------------------------------*
      *                  * Find su archivio [oct]                      *
      *                  *---------------------------------------------*
           perform   fnd-arc-oct-000      thru fnd-arc-oct-999        .
           if        w-fnd-arc-oct-sel    not  = spaces
                     go to acc-dat-doc-100.
      *                  *---------------------------------------------*
      *                  * Forzatura function-key Do                   *
      *                  *---------------------------------------------*
           move      "DO  "               to   v-key                  .
       acc-dat-doc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a zero : reimpostazione, a meno   *
      *                  * che non si sia su Up                        *
      *                  *---------------------------------------------*
           if        w-tes-dat-doc        not  = zero
                     go to acc-dat-doc-600.
           if        v-key                =    "UP  "
                     go to acc-dat-doc-600
           else      go to acc-dat-doc-100.
       acc-dat-doc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-doc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-dat-doc-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-dat-doc-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-dat-doc-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-dat-doc-999.
       acc-dat-doc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Data documento             *
      *    *-----------------------------------------------------------*
       vis-dat-doc-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      04                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      w-tes-dat-doc        to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-doc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Numero documento              *
      *    *-----------------------------------------------------------*
       acc-num-doc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-num-doc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      05                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "SLCT"               to   v-pfk (11)             .
           move      w-tes-num-doc-prg    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-num-doc-999.
       acc-num-doc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-num-doc-prg      .
       acc-num-doc-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-num-doc-350.
      *                  *---------------------------------------------*
      *                  * Find su archivio [oct]                      *
      *                  *---------------------------------------------*
           perform   fnd-arc-oct-000      thru fnd-arc-oct-999        .
           if        w-fnd-arc-oct-sel    not  = spaces
                     go to acc-num-doc-100.
      *                  *---------------------------------------------*
      *                  * Forzatura function-key Do                   *
      *                  *---------------------------------------------*
           move      "DO  "               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * A controllo valore impostato                *
      *                  *---------------------------------------------*
           go to     acc-num-doc-400.
       acc-num-doc-350.
      *              *-------------------------------------------------*
      *              * Se Select                                       *
      *              *-------------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to acc-num-doc-400.
      *                  *---------------------------------------------*
      *                  * Test preliminari                            *
      *                  *---------------------------------------------*
           if        w-tes-num-doc-prg    =    zero
                     go to acc-num-doc-100.
           if        w-tes-dat-doc        =    zero
                     go to acc-num-doc-100.
           if        w-tes-tmo-orc        =    spaces
                     go to acc-num-doc-100.
      *                  *---------------------------------------------*
      *                  * Preparazione dei parametri per la selezione *
      *                  *---------------------------------------------*
           move      w-tes-num-doc-prg    to   w-slc-num-oct-npg      .
           move      w-tes-cod-dpz        to   w-slc-num-oct-dpz      .
           move      w-tes-tmo-orc-sgl    to   w-slc-num-oct-sgl      .
           move      w-tes-dat-doc        to   w-slc-num-oct-dds      .
      *                  *---------------------------------------------*
      *                  * Routine di ricerca                          *
      *                  *---------------------------------------------*
           perform   slc-num-oct-000      thru slc-num-oct-999        .
      *                  *---------------------------------------------*
      *                  * Se non selezionato alcun elemento : a       *
      *                  * reimpostazione                              *
      *                  *---------------------------------------------*
           if        w-slc-num-oct-sel    not  = spaces
                     go to acc-num-doc-100.
      *                  *---------------------------------------------*
      *                  * Forzatura function-key Do                   *
      *                  *---------------------------------------------*
           move      "DO  "               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * A controllo valore impostato                *
      *                  *---------------------------------------------*
           go to     acc-num-doc-400.
       acc-num-doc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-num-doc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-num-doc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-num-doc-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-num-doc-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-num-doc-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-num-doc-999.
       acc-num-doc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Numero documento           *
      *    *-----------------------------------------------------------*
       vis-num-doc-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      05                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      w-tes-num-doc-prg    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-doc-999.
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
      *                  * Data consegna                               *
      *                  *---------------------------------------------*
           perform   acc-dat-cns-000      thru acc-dat-cns-999        .
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
      *              * Codice cliente                                  *
      *              *-------------------------------------------------*
           perform   vis-cli-org-000      thru vis-cli-org-999        .
           perform   vis-cli-org-des-000  thru vis-cli-org-des-999    .
      *              *-------------------------------------------------*
      *              * Data consegna                                   *
      *              *-------------------------------------------------*
           perform   vis-dat-cns-000      thru vis-dat-cns-999        .
      *              *-------------------------------------------------*
      *              * Status ordine                                   *
      *              *-------------------------------------------------*
           perform   vis-sts-orc-000      thru vis-sts-orc-999        .
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
      *              * Codice cliente                                  *
      *              *-------------------------------------------------*
           perform   pmt-cli-org-000      thru pmt-cli-org-999        .
      *              *-------------------------------------------------*
      *              * Data consegna                                   *
      *              *-------------------------------------------------*
           perform   pmt-dat-cns-000      thru pmt-dat-cns-999        .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice cliente                   *
      *    *-----------------------------------------------------------*
       pmt-cli-org-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      25                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Cliente commerciale     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cli-org-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Data consegna                    *
      *    *-----------------------------------------------------------*
       pmt-dat-cns-000.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      25                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data consegna           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazione Note                            *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "N.B.: Il programma forza tutte le date consegna, r
      -              "ichiesta e prevista, del docu-"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "      mento, anche se evaso o parzialmente evaso. 
      -              "                              "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dat-cns-999.
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
      *    * Visualizzazione campo testata : Codice cliente            *
      *    *-----------------------------------------------------------*
       vis-cli-org-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      08                   to   v-lin                  .
           move      27                   to   v-pos                  .
           move      "<B"                 to   v-edm                  .
           move      w-tes-cli-org (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cli-org-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione cliente       *
      *    *-----------------------------------------------------------*
       vis-cli-org-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cli-org-rag (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cli-org-via (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cli-org-loc (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cli-org-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Data consegna                *
      *    *-----------------------------------------------------------*
       acc-dat-cns-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dat-cns-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      ">"                  to   v-edm                  .
           move      12                   to   v-lin                  .
           move      27                   to   v-pos                  .
           move      w-tes-dat-cns (1)    to   v-dat                  .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dat-cns-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dat-cns-999.
       acc-dat-cns-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   w-tes-dat-cns (1)      .
       acc-dat-cns-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        w-tes-dat-cns (1)    =    zero
                     go to acc-dat-cns-100.
       acc-dat-cns-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-cns-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dat-cns-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dat-cns-100.
       acc-dat-cns-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Data consegna             *
      *    *-----------------------------------------------------------*
       vis-dat-cns-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      12                   to   v-lin                  .
           move      27                   to   v-pos                  .
           move      w-tes-dat-cns (1)    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-cns-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Status ordine             *
      *    *-----------------------------------------------------------*
       vis-sts-orc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      41                   to   v-pos                  .
           if        d-sts-orc-sts-ord    =    01
                     move  "Chiuso       "
                                          to   v-alf
           else if   d-sts-orc-sts-ord    =    02
                     move  "Evaso parz.  "
                                          to   v-alf
           else if   d-sts-orc-sts-ord    =    03
                     move  "Evaso        "
                                          to   v-alf
           else if   d-sts-orc-sts-ord    =    04
                     move  "Inevaso      "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sts-orc-999.
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
           if        w-tes-tmo-orc        not  = spaces
                     go to cnt-tdo-key-100.
           move      "Manca il tipo movimento !                         
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-key-900.
       cnt-tdo-key-100.
      *              *-------------------------------------------------*
      *              * Test su data documento                          *
      *              *-------------------------------------------------*
           if        w-tes-dat-doc        not  = zero
                     go to cnt-tdo-key-200.
           move      "Manca la data documento !                         
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-key-900.
       cnt-tdo-key-200.
      *              *-------------------------------------------------*
      *              * Test su numero documento                        *
      *              *-------------------------------------------------*
           if        w-tes-num-doc-prg    not  = zero
                     go to cnt-tdo-key-300.
           move      "Manca il numero documento !                       
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-key-900.
       cnt-tdo-key-300.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Completamento numero documento              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Secolo/Anno                             *
      *                      *-----------------------------------------*
           move      w-tes-dat-doc        to   s-dat                  .
           move      s-saa                to   w-tes-num-doc-saa      .
      *                      *-----------------------------------------*
      *                      * Codice dipendenza                       *
      *                      *-----------------------------------------*
           move      w-tes-cod-dpz        to   w-tes-num-doc-dpz      .
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
      *              * Test                                            *
      *              *-------------------------------------------------*
           if        w-tes-tmo-orc        =    spaces and
                     w-tes-dat-doc        =    zero   and
                     w-tes-num-doc        =    zero
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
           move      spaces               to   w-tes-tmo-orc          .
           move      spaces               to   w-tes-tmo-orc-des      .
           move      zero                 to   w-tes-tmo-orc-vld      .
           move      zero                 to   w-tes-tmo-orc-dpz      .
           move      zero                 to   w-tes-tmo-orc-ord      .
           move      zero                 to   w-tes-tmo-orc-prd      .
           move      spaces               to   w-tes-tmo-orc-sgl      .
           move      zero                 to   w-tes-num-doc          .
           move      zero                 to   w-tes-dat-doc          .
           move      spaces               to   w-tes-sgl-num          .
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *-----------------------------------------------------------*
       nor-nok-tes-000.
           move      zero                 to   w-tes-cli-org (1)      .
           move      spaces               to   w-tes-cli-org-rag (1)  .
           move      spaces               to   w-tes-cli-org-via (1)  .
           move      spaces               to   w-tes-cli-org-loc (1)  .
           move      zero                 to   w-tes-dat-cns (1)      .
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
      *              * Determinazione se documento esistente           *
      *              *-------------------------------------------------*
           perform   det-doc-ges-000      thru det-doc-ges-999        .
      *              *-------------------------------------------------*
      *              * Se documento non esistente : A Errore           *
      *              *-------------------------------------------------*
           if        w-det-doc-ges-snx    =    "N"
                     go to rou-let-reg-050.
      *              *-------------------------------------------------*
      *              * Se documento esistente : Modifica               *
      *              *-------------------------------------------------*
           if        w-det-doc-ges-snx    =    "S"
                     go to rou-let-reg-100.
      *              *-------------------------------------------------*
      *              * Se documento esistente ma con tipo movimento    *
      *              * diverso                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      spaces               to   w-err-box-err-msg      .
           string    "Movimento esistente, ma di tipo '"
                                delimited by   size
                     w-det-doc-ges-tmt
                                delimited by   spaces
                     "' !"
                                delimited by   size
                                          into w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-rou-let-reg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rou-let-reg-999.
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
      *                      * Test se ordine chiuso                   *
      *                      *-----------------------------------------*
           if        rf-oct-flg-och       =    spaces
                     go to rou-let-reg-120.
      *                          *-------------------------------------*
      *                          * Messaggio di errore                 *
      *                          *-------------------------------------*
           move      "Ordine cliente gia' chiuso !                      
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
           move      rf-oct-num-prt       to   w-tes-num-prt          .
           move      rf-oct-sgl-num       to   w-tes-sgl-num          .
      *                      *-----------------------------------------*
      *                      * Determinazione valori attuali testata   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Valori contenuti direttamente in    *
      *                          * record [oct]                        *
      *                          *-------------------------------------*
           move      rf-oct-cod-arc       to   w-tes-cli-org (1)      .
           move      rf-oct-dat-cns       to   w-tes-dat-cns (1)      .
      *                          *-------------------------------------*
      *                          * Valori contenuti indirettamente in  *
      *                          * record [oct]                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Anagrafica archivio             *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Lettura record [dcc] prin-  *
      *                                  * cipale                      *
      *                                  *-----------------------------*
           move      w-tes-cli-org (1)    to   w-let-arc-dcc-cli      .
           move      spaces               to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
           move      w-let-arc-dcc-rag    to   w-tes-cli-org-rag (1)  .
           move      w-let-arc-dcc-via    to   w-tes-cli-org-via (1)  .
           move      w-let-arc-dcc-loc    to   w-tes-cli-org-loc (1)  .
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
      *              *-------------------------------------------------*
      *              * Determinazione status dell'ordine               *
      *              *-------------------------------------------------*
           move      "DT"                 to   d-sts-orc-tip-ope      .
           perform   det-sts-orc-cll-000  thru det-sts-orc-cll-999    .
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
           move      w-tes-dat-doc        to   w-def-dat-doc          .
      *              *-------------------------------------------------*
      *              * Aggiornamento del tipo movimento di default     *
      *              *-------------------------------------------------*
           move      w-tes-tmo-orc        to   w-def-tmo-orc          .
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
      *              * Trattamento righe ordine                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatore righe scritte    *
      *                  *---------------------------------------------*
           move      zero                 to   w-scr-mov-fil-ctr      .
      *                  *---------------------------------------------*
      *                  * Start su file [ocr]                         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-tes-num-prt        to   rf-ocr-num-prt         .
           move      zero                 to   rf-ocr-num-prg         .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                      *-----------------------------------------*
      *                      * Test su esito operazione                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to scr-mov-fil-500.
       scr-mov-fil-100.
      *                  *---------------------------------------------*
      *                  * Ottenimento sequenziale record [ocr]        *
      *                  *---------------------------------------------*
           move      "GN"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                      *-----------------------------------------*
      *                      * Test se 'At End'                        *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to scr-mov-fil-500.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
           if        rf-ocr-num-prt       not  = w-tes-num-prt
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
      *                      * Release record [ocr]                    *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
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
      *                  * Aggiornamento record [ocr]                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Data di consegna richiesta              *
      *                      *-----------------------------------------*
           move      w-tes-dat-cns (1)    to   rf-ocr-dcn-ric         .
      *                      *-----------------------------------------*
      *                      * Data di consegna prevista               *
      *                      *-----------------------------------------*
           move      w-tes-dat-cns (1)    to   rf-ocr-dcn-prv         .
      *                      *-----------------------------------------*
      *                      * Update record [ocr]                     *
      *                      *-----------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                      *-----------------------------------------*
      *                      * Release record [ocr]                    *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
       scr-mov-fil-220.
      *                  *---------------------------------------------*
      *                  * Riciclo a riga successiva                   *
      *                  *---------------------------------------------*
           go to     scr-mov-fil-100.
       scr-mov-fil-500.
      *              *-------------------------------------------------*
      *              * Release ultimo record [ocr]                     *
      *              *-------------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
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
           move      "Nessuna riga per l'ordine richiamato.             
      -              "               "    to   w-err-box-err-m02      .
           perform   box-msg-e02-000      thru box-msg-e02-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     scr-mov-fil-999.
       scr-mov-fil-600.
      *              *-------------------------------------------------*
      *              * Aggiornamento record [oct]                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ottenimento record [oct]                    *
      *                  *---------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-tes-num-prt        to   rf-oct-num-prt         .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * Flag di ordine chiuso                       *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-oct-ide-dat         .
           move      s-ute                to   rf-oct-ide-ute         .
           move      s-fas                to   rf-oct-ide-fas         .
           move      w-tes-dat-cns (1)    to   rf-oct-dat-cns         .
      *                  *---------------------------------------------*
      *                  * Update record [oct]                         *
      *                  *---------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * Rilascio record [oct]                       *
      *                  *---------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
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
      *    * Select archivio [oct] in base al numero documento         *
      *    *-----------------------------------------------------------*
       slc-num-oct-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-slc-num-oct-sel      .
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-slc-num-oct-toc      .
           move      zero                 to   w-slc-num-oct-dat      .
           move      zero                 to   w-slc-num-oct-num      .
           move      zero                 to   w-slc-num-oct-crb      .
      *              *-------------------------------------------------*
      *              * Preparazione secolo, anno                       *
      *              *-------------------------------------------------*
           move      w-slc-num-oct-dds    to   s-dat                  .
           move      s-saa                to   w-slc-num-oct-saa      .
       slc-num-oct-080.
      *              *-------------------------------------------------*
      *              * Completamento numero documento                  *
      *              *-------------------------------------------------*
           move      w-slc-num-oct-saa    to   w-slc-num-oct-nsa      .
           move      w-slc-num-oct-dpz    to   w-slc-num-oct-ndp      .
       slc-num-oct-100.
      *              *-------------------------------------------------*
      *              * Start su file [oct]                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start                                       *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CNTDEN    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-slc-num-oct-saa    to   rf-oct-scl-ann         .
           move      w-slc-num-oct-dpz    to   rf-oct-cod-dpz         .
           move      w-slc-num-oct-sgl    to   rf-oct-sgl-num         .
           move      w-slc-num-oct-nds    to   rf-oct-num-doc         .
           move      zero                 to   rf-oct-num-prt         .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * Se errore di start : a controllo contatore  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to slc-num-oct-800.
       slc-num-oct-200.
      *              *-------------------------------------------------*
      *              * Read-next su [oct]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * Se at end : a controllo contatore           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to slc-num-oct-500.
       slc-num-oct-300.
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : a controllo contatore     *
      *              *-------------------------------------------------*
           if        rf-oct-scl-ann       not  = w-slc-num-oct-saa or
                     rf-oct-cod-dpz       not  = w-slc-num-oct-dpz or
                     rf-oct-sgl-num       not  = w-slc-num-oct-sgl or
                     rf-oct-num-doc       not  = w-slc-num-oct-nds
                     go to slc-num-oct-500.
       slc-num-oct-400.
      *              *-------------------------------------------------*
      *              * Incremento numero records nel buffer            *
      *              *-------------------------------------------------*
           add       1                    to   w-slc-num-oct-crb      .
      *              *-------------------------------------------------*
      *              * Test se buffer oltre il numero previsto         *
      *              *-------------------------------------------------*
           if        w-slc-num-oct-crb    >    30
                     go to slc-num-oct-520.
       slc-num-oct-420.
      *              *-------------------------------------------------*
      *              * Bufferizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Protocollo ordine                           *
      *                  *---------------------------------------------*
           move      rf-oct-num-prt       to   w-slc-num-oct-bpt
                                              (w-slc-num-oct-crb)     .
      *                  *---------------------------------------------*
      *                  * Riciclo a lettura                           *
      *                  *---------------------------------------------*
           go to     slc-num-oct-200.
       slc-num-oct-500.
      *                  *---------------------------------------------*
      *                  * Controllo numero records letti con lo stes- *
      *                  * so valore                                   *
      *                  *---------------------------------------------*
           if        w-slc-num-oct-crb    =    zero
                     go to slc-num-oct-800.
      *                  *---------------------------------------------*
      *                  * Se trovato un solo elemento uscita con      *
      *                  * quello                                      *
      *                  *---------------------------------------------*
           if        w-slc-num-oct-crb    >    1
                     go to slc-num-oct-520.
      *                      *-----------------------------------------*
      *                      * Lettura record [oct]                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-slc-num-oct-bpt (1)
                                          to   rf-oct-num-prt         .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-oct-dat-doc
                     move  zero           to   rf-oct-num-doc
                     move  zero           to   rf-oct-cod-arc
                     move  spaces         to   rf-oct-dpz-arc         .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione dati letti              *
      *                      *-----------------------------------------*
           move      rf-oct-dat-doc       to   w-slc-num-oct-dat      .
           move      rf-oct-num-doc       to   w-slc-num-oct-num      .
           move      rf-oct-tmo-orc       to   w-slc-num-oct-toc      .
      *                      *-----------------------------------------*
      *                      * Ad operazioni prima dell'uscita         *
      *                      *-----------------------------------------*
           go to     slc-num-oct-800.
       slc-num-oct-520.
      *                  *---------------------------------------------*
      *                  * Box di espansione                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione numero pagine nel buffer *
      *                      *-----------------------------------------*
           move      w-slc-num-oct-crb    to   w-slc-num-oct-cpb      .
           subtract  1                    from w-slc-num-oct-cpb      .
           divide    6                    into w-slc-num-oct-cpb      .
           add       1                    to   w-slc-num-oct-cpb      .
      *                      *-----------------------------------------*
      *                      * Inizializzazione numero record nel buf- *
      *                      * fer attualmente trattato                *
      *                      *-----------------------------------------*
           move      1                    to   w-slc-num-oct-c01      .
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
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      18                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Fincatura                               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "                       Selezionare l'ordine deside
      -              "rato                      "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura di chiusura              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione pagina video contenente *
      *                      * il record attualmente trattato          *
      *                      *-----------------------------------------*
           perform   slc-num-oct-950      thru slc-num-oct-989        .
      *                      *-----------------------------------------*
      *                      * Video in On                             *
      *                      *-----------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       slc-num-oct-550.
      *                      *-----------------------------------------*
      *                      * Determinazione numero linea a video     *
      *                      *-----------------------------------------*
           move      w-slc-num-oct-c01    to   w-slc-num-oct-nli      .
       slc-num-oct-555.
           if        w-slc-num-oct-nli    >    6
                     subtract  6          from w-slc-num-oct-nli
                     go to slc-num-oct-555.
      *                          *-------------------------------------*
      *                          * Incremento numero linea a video     *
      *                          * per posizionamento verticale        *
      *                          *-------------------------------------*
           add       09                   to   w-slc-num-oct-nli      .
       slc-num-oct-560.
      *                      *-----------------------------------------*
      *                      * Espansione record attualmente trattato  *
      *                      *-----------------------------------------*
       slc-num-oct-575.
      *                      *-----------------------------------------*
      *                      * Accettazione del mark-point             *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-slc-num-oct-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-slc-num-oct-c01    <    w-slc-num-oct-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-slc-num-oct-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-slc-num-oct-cpa    <    w-slc-num-oct-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-slc-num-oct-nli    to   v-lin                  .
           move      09                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       slc-num-oct-580.
           if        v-key                =    spaces or
                     v-key                =    "DO  "
                     go to slc-num-oct-582
           else if   v-key                =    "UP  "
                     go to slc-num-oct-584
           else if   v-key                =    "DOWN"
                     go to slc-num-oct-586
           else if   v-key                =    "EXIT"
                     go to slc-num-oct-598
           else if   v-key                =    "NXSC"
                     go to slc-num-oct-592
           else if   v-key                =    "PRSC"
                     go to slc-num-oct-594
           else      go to slc-num-oct-575.
       slc-num-oct-582.
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
      *                  * Memorizzazione valore selezionato           *
      *                  *---------------------------------------------*
           move      w-slc-num-oct-bpt
                    (w-slc-num-oct-c01)   to   w-slc-num-oct-prt      .
      *                  *---------------------------------------------*
      *                  * Lettura record [oct]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-slc-num-oct-prt    to   rf-oct-num-prt         .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-oct-dat-doc
                     move  zero           to   rf-oct-num-doc
                     move  zero           to   rf-oct-cod-arc
                     move  spaces         to   rf-oct-dpz-arc         .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione dati letti                  *
      *                  *---------------------------------------------*
           move      rf-oct-dat-doc       to   w-slc-num-oct-dat      .
           move      rf-oct-num-doc       to   w-slc-num-oct-num      .
           move      rf-oct-tmo-orc       to   w-slc-num-oct-toc      .
      *                  *---------------------------------------------*
      *                  * Ad operazioni prima dell'uscita             *
      *                  *---------------------------------------------*
           go to     slc-num-oct-800.
       slc-num-oct-584.
      *              *-------------------------------------------------*
      *              * Se Up                                           *
      *              *-------------------------------------------------*
           subtract  1                    from w-slc-num-oct-c01      .
           if        w-slc-num-oct-nli    =    10
                     go to slc-num-oct-590
           else      go to slc-num-oct-550.
       slc-num-oct-586.
      *              *-------------------------------------------------*
      *              * Se Down                                         *
      *              *-------------------------------------------------*
           if        w-slc-num-oct-c01    <    w-slc-num-oct-crb
                     add   1              to   w-slc-num-oct-c01
                     go to slc-num-oct-588
           else      go to slc-num-oct-575.
       slc-num-oct-588.
           if        w-slc-num-oct-nli    =    15
                     go to slc-num-oct-590
           else      go to slc-num-oct-550.
       slc-num-oct-590.
           perform   slc-num-oct-950      thru slc-num-oct-989        .
           go to     slc-num-oct-550.
       slc-num-oct-592.
      *              *-------------------------------------------------*
      *              * Se Next screen                                  *
      *              *-------------------------------------------------*
           add       1                    to   w-slc-num-oct-cpa      .
           go to     slc-num-oct-596.
       slc-num-oct-594.
      *              *-------------------------------------------------*
      *              * Se Previous screen                              *
      *              *-------------------------------------------------*
           subtract  1                    from w-slc-num-oct-cpa      .
       slc-num-oct-596.
           move      w-slc-num-oct-cpa    to   w-slc-num-oct-c01      .
           multiply  6                    by   w-slc-num-oct-c01      .
           subtract  5                    from w-slc-num-oct-c01      .
           go to     slc-num-oct-590.
       slc-num-oct-598.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       slc-num-oct-800.
      *              *-------------------------------------------------*
      *              * Salvataggio valori chiave                       *
      *              *-------------------------------------------------*
           move      w-tes-val-key        to   w-sav-val-key          .
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori estratti                 *
      *              *-------------------------------------------------*
           move      w-slc-num-oct-toc    to   w-tes-tmo-orc          .
           move      w-slc-num-oct-dat    to   w-tes-dat-doc          .
           move      w-slc-num-oct-num    to   w-tes-num-doc          .
      *              *-------------------------------------------------*
      *              * Test su valori estratti                         *
      *              *-------------------------------------------------*
           if        w-tes-tmo-orc        =    spaces or
                     w-tes-dat-doc        =    zero   or
                     w-tes-num-doc        =    zero
                     go to slc-num-oct-900.
       slc-num-oct-820.
      *              *-------------------------------------------------*
      *              * Determinazione campi derivati e effettuazione   *
      *              * controlli come se fossero stati impostati       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zoc]                      *
      *                  *---------------------------------------------*
           move      w-tes-tmo-orc        to   w-let-arc-zoc-cod      .
           perform   let-arc-zoc-000      thru let-arc-zoc-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zoc-flg    not  = spaces
                     go to slc-num-oct-900.
      *                  *---------------------------------------------*
      *                  * Se a spaces : reimpostazione                *
      *                  *---------------------------------------------*
           if        w-tes-tmo-orc        =    spaces
                     go to slc-num-oct-900.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori associati al tipo    *
      *                  * movimento ordini clienti                    *
      *                  *---------------------------------------------*
           move      w-let-arc-zoc-des    to   w-tes-tmo-orc-des      .
           move      w-let-arc-zoc-vld    to   w-tes-tmo-orc-vld      .
           move      w-let-arc-zoc-dpz    to   w-tes-tmo-orc-dpz      .
           move      w-let-arc-zoc-ord    to   w-tes-tmo-orc-ord      .
           move      w-let-arc-zoc-prd    to   w-tes-tmo-orc-prd      .
           move      w-let-arc-zoc-sgl    to   w-tes-tmo-orc-sgl      .
           move      w-let-arc-zoc-dtr    to   w-tes-tmo-orc-dtr      .
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza                   *
      *                  *---------------------------------------------*
           if        w-tes-tmo-orc-vld    not  = 02
                     go to slc-num-oct-840.
           if        w-tes-cod-dpz        =    w-tes-tmo-orc-dpz
                     go to slc-num-oct-840.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Tipo movimento incompatibile con il codice dipende
      -              "nza            "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A uscita con errore                     *
      *                      *-----------------------------------------*
           go to     slc-num-oct-900.
       slc-num-oct-840.
      *                  *---------------------------------------------*
      *                  * Controllo tipo origine del documento        *
      *                  *---------------------------------------------*
           if        w-tes-tmo-orc-ord    =   01
                     go to slc-num-oct-860.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Tipo movimento non gestibile da questo programma !
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A uscita con errore                     *
      *                      *-----------------------------------------*
           go to     slc-num-oct-900.
       slc-num-oct-860.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Completamento visualizzazione campi-chiave  *
      *                  *---------------------------------------------*
           perform   vis-tmo-orc-000      thru vis-tmo-orc-999        .
           perform   vis-tmo-orc-des-000  thru vis-tmo-orc-des-999    .
           perform   vis-dat-doc-000      thru vis-dat-doc-999        .
           perform   vis-num-doc-000      thru vis-num-doc-999        .
      *                  *---------------------------------------------*
      *                  * Uscita con successo                         *
      *                  *---------------------------------------------*
           go to     slc-num-oct-999.
       slc-num-oct-900.
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
           move      "#"                  to   w-slc-num-oct-sel      .
       slc-num-oct-940.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     slc-num-oct-999.
       slc-num-oct-950.
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina video contenente il re-  *
      *              * cord attualmente trattato                       *
      *              *-------------------------------------------------*
           move      w-slc-num-oct-c01    to   w-slc-num-oct-c02      .
           add       5                    to   w-slc-num-oct-c02      .
           divide    6                    into w-slc-num-oct-c02      .
           move      w-slc-num-oct-c02    to   w-slc-num-oct-cpa      .
           subtract  1                    from w-slc-num-oct-c02      .
           multiply  6                    by   w-slc-num-oct-c02      .
           add       1                    to   w-slc-num-oct-c02      .
           add       5
                     w-slc-num-oct-c02  giving w-slc-num-oct-c03      .
           move      w-slc-num-oct-c03    to   w-slc-num-oct-c04      .
           if        w-slc-num-oct-c03    >    w-slc-num-oct-crb
                     move  w-slc-num-oct-crb
                                          to   w-slc-num-oct-c03      .
           move      10                   to   w-slc-num-oct-c05      .
       slc-num-oct-951.
      *              *-------------------------------------------------*
      *              * Lettura record [oct] per visualizzazione        *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-slc-num-oct-bpt
                    (w-slc-num-oct-c02)   to   rf-oct-num-prt         .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-oct-dat-doc
                     move  zero           to   rf-oct-num-doc
                     move  zero           to   rf-oct-cod-arc
                     move  spaces         to   rf-oct-dpz-arc         .
       slc-num-oct-960.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice tipo movimento                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-slc-num-oct-c05    to   v-lin                  .
           move      03                   to   v-pos                  .
           move      rf-oct-tmo-orc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      w-slc-num-oct-c05    to   v-lin                  .
           move      09                   to   v-pos                  .
           move      rf-oct-dat-doc       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      w-slc-num-oct-c05    to   v-lin                  .
           move      18                   to   v-pos                  .
           move      rf-oct-num-doc (6:6) to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Codice archivio                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      w-slc-num-oct-c05    to   v-lin                  .
           move      26                   to   v-pos                  .
           move      rf-oct-cod-arc       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza archivio                  *
      *                  *---------------------------------------------*
           if        rf-oct-dpz-arc       =    spaces
                     go to slc-num-oct-965.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      w-slc-num-oct-c05    to   v-lin                  .
           move      33                   to   v-pos                  .
           move      "-"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      w-slc-num-oct-c05    to   v-lin                  .
           move      34                   to   v-pos                  .
           move      rf-oct-dpz-arc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       slc-num-oct-965.
      *                  *---------------------------------------------*
      *                  * Lettura record [dcc]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
           move      rf-oct-cod-arc       to   rf-dcc-cod-cli         .
           move      rf-oct-dpz-arc       to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-dcc-rag-soc         .
      *                  *---------------------------------------------*
      *                  * Ragione sociale archivio                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-slc-num-oct-c05    to   v-lin                  .
           move      39                   to   v-pos                  .
           move      rf-dcc-rag-soc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Incremento contatori                        *
      *                  *---------------------------------------------*
           add       1                    to   w-slc-num-oct-c02      .
           add       1                    to   w-slc-num-oct-c05      .
           if        w-slc-num-oct-c02    not  > w-slc-num-oct-c03
                     go to slc-num-oct-951.
       slc-num-oct-970.
           if        w-slc-num-oct-c02    >    w-slc-num-oct-c04
                     go to slc-num-oct-980.
           if        w-slc-num-oct-crb    not  > 6
                     go to slc-num-oct-980.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      w-slc-num-oct-c05    to   v-lin                  .
           move      11                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-slc-num-oct-c02      .
           add       1                    to   w-slc-num-oct-c05      .
           go to     slc-num-oct-970.
       slc-num-oct-980.
      *                  *---------------------------------------------*
      *                  * Literal 'pagina'                            *
      *                  *---------------------------------------------*
           move      w-slc-num-oct-cpa    to   w-slc-num-oct-lt1      .
           move      w-slc-num-oct-cpb    to   w-slc-num-oct-lt2      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-slc-num-oct-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       slc-num-oct-989.
           exit.
       slc-num-oct-999.
           exit.

      *    *===========================================================*
      *    * Find su archivio [oct]                                    *
      *    *-----------------------------------------------------------*
       fnd-arc-oct-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-fnd-arc-oct-sel      .
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "porc3010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     move  "#"            to   w-fnd-arc-oct-sel
                     go to  fnd-arc-oct-999.
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
           move      "pgm/orc/prg/obj/porc3010"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
      *              *-------------------------------------------------*
      *              * Estrazione di eventuali variabili di i.p.c. de- *
      *              * terminate da function-key "SLCT" durante l'in-  *
      *              * terrogazione                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo movimento                              *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "tmo-orc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     move  "#"            to   w-fnd-arc-oct-sel
                     go to fnd-arc-oct-999.
           move      s-alf                to   w-fnd-arc-oct-toc      .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "dat-doc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     move  "#"            to   w-fnd-arc-oct-sel
                     go to fnd-arc-oct-999.
           move      s-dat                to   w-fnd-arc-oct-dat      .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "num-doc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     move  "#"            to   w-fnd-arc-oct-sel
                     go to fnd-arc-oct-999.
           move      s-num                to   w-fnd-arc-oct-num      .
      *              *-------------------------------------------------*
      *              * Salvataggio valori chiave                       *
      *              *-------------------------------------------------*
           move      w-tes-val-key        to   w-sav-val-key          .
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori estratti                 *
      *              *-------------------------------------------------*
           move      w-fnd-arc-oct-toc    to   w-tes-tmo-orc          .
           move      w-fnd-arc-oct-dat    to   w-tes-dat-doc          .
           move      w-fnd-arc-oct-num    to   w-tes-num-doc          .
       fnd-arc-oct-400.
      *              *-------------------------------------------------*
      *              * Determinazione campi derivati e effettuazione   *
      *              * controlli come se fossero stati impostati       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zoc]                      *
      *                  *---------------------------------------------*
           move      w-tes-tmo-orc        to   w-let-arc-zoc-cod      .
           perform   let-arc-zoc-000      thru let-arc-zoc-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zoc-flg    not  = spaces
                     go to fnd-arc-oct-900.
      *                  *---------------------------------------------*
      *                  * Se a spaces : reimpostazione                *
      *                  *---------------------------------------------*
           if        w-tes-tmo-orc        =    spaces
                     go to fnd-arc-oct-900.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori associati al tipo    *
      *                  * movimento ordini clienti                    *
      *                  *---------------------------------------------*
           move      w-let-arc-zoc-des    to   w-tes-tmo-orc-des      .
           move      w-let-arc-zoc-vld    to   w-tes-tmo-orc-vld      .
           move      w-let-arc-zoc-dpz    to   w-tes-tmo-orc-dpz      .
           move      w-let-arc-zoc-ord    to   w-tes-tmo-orc-ord      .
           move      w-let-arc-zoc-prd    to   w-tes-tmo-orc-prd      .
           move      w-let-arc-zoc-sgl    to   w-tes-tmo-orc-sgl      .
           move      w-let-arc-zoc-dtr    to   w-tes-tmo-orc-dtr      .
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza                   *
      *                  *---------------------------------------------*
           if        w-tes-tmo-orc-vld    not  = 02
                     go to fnd-arc-oct-420.
           if        w-tes-cod-dpz        =    w-tes-tmo-orc-dpz
                     go to fnd-arc-oct-420.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Tipo movimento incompatibile con il codice dipende
      -              "nza            "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A uscita con errore                     *
      *                      *-----------------------------------------*
           go to     fnd-arc-oct-900.
       fnd-arc-oct-420.
      *                  *---------------------------------------------*
      *                  * Controllo tipo origine del documento        *
      *                  *---------------------------------------------*
           if        w-tes-tmo-orc-ord    =   01
                     go to fnd-arc-oct-600.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Tipo movimento non gestibile da questo programma !
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A uscita con errore                     *
      *                      *-----------------------------------------*
           go to     fnd-arc-oct-900.
       fnd-arc-oct-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Completamento visualizzazione campi-chiave  *
      *                  *---------------------------------------------*
           perform   vis-tmo-orc-000      thru vis-tmo-orc-999        .
           perform   vis-tmo-orc-des-000  thru vis-tmo-orc-des-999    .
           perform   vis-dat-doc-000      thru vis-dat-doc-999        .
           perform   vis-num-doc-000      thru vis-num-doc-999        .
      *                  *---------------------------------------------*
      *                  * Uscita con successo                         *
      *                  *---------------------------------------------*
           go to     fnd-arc-oct-999.
       fnd-arc-oct-900.
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
           move      "#"                  to   w-fnd-arc-oct-sel      .
       fnd-arc-oct-999.
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
      *    * Routine di lettura archivio [cli]                         *
      *    *-----------------------------------------------------------*
       let-arc-cli-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cli-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-cli-cod    =    zero
                     go to let-arc-cli-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      w-let-arc-cli-cod    to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-cli-400.
       let-arc-cli-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-cli-rag-soc       to   w-let-arc-cli-rag      .
           move      rf-cli-via-cli       to   w-let-arc-cli-via      .
           move      rf-cli-loc-cli       to   w-let-arc-cli-loc      .
           move      rf-cli-prt-iva       to   w-let-arc-cli-piv      .
           move      rf-cli-cod-cge       to   w-let-arc-cli-stc      .
           move      rf-cli-cod-iva       to   w-let-arc-cli-ass      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-cli-999.
       let-arc-cli-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-cli-flg      .
           move      all   "."            to   w-let-arc-cli-rag      .
           go to     let-arc-cli-520.
       let-arc-cli-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cli-rag      .
       let-arc-cli-520.
           move      spaces               to   w-let-arc-cli-via      .
           move      spaces               to   w-let-arc-cli-loc      .
           move      zero                 to   w-let-arc-cli-piv      .
           move      zero                 to   w-let-arc-cli-stc      .
           move      zero                 to   w-let-arc-cli-ass      .
       let-arc-cli-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [dcc]                         *
      *    *-----------------------------------------------------------*
       let-arc-dcc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codici a zero o spazi                   *
      *              *-------------------------------------------------*
           if        w-let-arc-dcc-cli    =    zero
                     go to let-arc-dcc-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      w-let-arc-dcc-cli    to   rf-dcc-cod-cli         .
           move      w-let-arc-dcc-dpz    to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-dcc-400.
       let-arc-dcc-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-dcc-rag-soc       to   w-let-arc-dcc-rag      .
           move      rf-dcc-via-dcc       to   w-let-arc-dcc-via      .
           move      rf-dcc-loc-dcc       to   w-let-arc-dcc-loc      .
           move      rf-dcc-cod-vlt       to   w-let-arc-dcc-vlt      .
           move      rf-dcc-cod-lng       to   w-let-arc-dcc-lng      .
           move      rf-dcc-tas-ivc       to   w-let-arc-dcc-tai      .
           move      rf-dcc-fco-blo       to   w-let-arc-dcc-cbl      .
           move      rf-dcc-for-blo       to   w-let-arc-dcc-obl      .
           move      rf-dcc-tip-frn       to   w-let-arc-dcc-tpf      .
           move      rf-dcc-arc-plf       to   w-let-arc-dcc-apf      .
           move      rf-dcc-dpz-plf       to   w-let-arc-dcc-dpf      .
           move      rf-dcc-tip-ftz       to   w-let-arc-dcc-tfz      .
           move      rf-dcc-tdp-plc       to   w-let-arc-dcc-tdp      .
           move      rf-dcc-sta-tus       to   w-let-arc-dcc-tus      .
           move      rf-dcc-sta-tud       to   w-let-arc-dcc-tud      .
           move      rf-dcc-sta-tuc       to   w-let-arc-dcc-tuc      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dcc-999.
       let-arc-dcc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dcc-flg      .
           move      all   "."            to   w-let-arc-dcc-rag      .
           go to     let-arc-dcc-520.
       let-arc-dcc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcc-rag      .
       let-arc-dcc-520.
           move      spaces               to   w-let-arc-dcc-via      .
           move      spaces               to   w-let-arc-dcc-loc      .
           move      spaces               to   w-let-arc-dcc-vlt      .
           move      spaces               to   w-let-arc-dcc-lng      .
           move      zero                 to   w-let-arc-dcc-tai      .
           move      zero                 to   w-let-arc-dcc-cbl      .
           move      zero                 to   w-let-arc-dcc-obl      .
           move      zero                 to   w-let-arc-dcc-tpf      .
           move      zero                 to   w-let-arc-dcc-apf      .
           move      spaces               to   w-let-arc-dcc-dpf      .
           move      zero                 to   w-let-arc-dcc-tfz      .
           move      zero                 to   w-let-arc-dcc-tdp      .
           move      zero                 to   w-let-arc-dcc-tus      .
           move      zero                 to   w-let-arc-dcc-tud      .
           move      zero                 to   w-let-arc-dcc-tuc      .
       let-arc-dcc-999.
           exit.

      *    *===========================================================*
      *    * Determinazione se documento gia' esistente oppure no      *
      *    *-----------------------------------------------------------*
       det-doc-ges-000.
      *              *-------------------------------------------------*
      *              * Tipo movimento trovato per il caso che la de-   *
      *              * terminazione dia esito pari a 'X' : a spaces    *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-doc-ges-tmt      .
      *              *-------------------------------------------------*
      *              * Start su file [oct]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "IDEDOC    "         to   f-key                  .
           move      w-tes-dat-doc        to   rf-oct-dat-doc         .
           move      w-tes-cod-dpz        to   rf-oct-cod-dpz         .
           move      w-tes-num-doc        to   rf-oct-num-doc         .
           move      spaces               to   rf-oct-tmo-orc         .
           move      zero                 to   rf-oct-num-prt         .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : a documento non esistente     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-doc-ges-900.
       det-doc-ges-100.
      *              *-------------------------------------------------*
      *              * Next su [oct]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *              *-------------------------------------------------*
      *              * Se 'at end' : a determinazione finale           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-doc-ges-800.
      *              *-------------------------------------------------*
      *              * Max su [oct], se non superato : a determinazio- *
      *              * ne finale                                       *
      *              *-------------------------------------------------*
           if        rf-oct-dat-doc       not  = w-tes-dat-doc or
                     rf-oct-cod-dpz       not  = w-tes-cod-dpz or
                     rf-oct-num-doc       not  = w-tes-num-doc
                     go to det-doc-ges-800.
       det-doc-ges-200.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo movimento letto   *
      *              *-------------------------------------------------*
           if        rf-oct-tmo-orc       =    w-tes-tmo-orc
                     go to det-doc-ges-300
           else      go to det-doc-ges-400.
       det-doc-ges-300.
      *              *-------------------------------------------------*
      *              * Se tipo movimento letto pari al tipo movimento  *
      *              * da cercare                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di documento esistente e trovato    *
      *                  *---------------------------------------------*
           move      "S"                  to   w-det-doc-ges-snx      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-doc-ges-999.
       det-doc-ges-400.
      *              *-------------------------------------------------*
      *              * Se tipo movimento letto diverso dal tipo movi-  *
      *              * mento da cercare                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se sigla numerazione per il tipo movimento  *
      *                  * letto diversa da sigla numerazione del ti-  *
      *                  * po movimento cercato : si ignora il docu-   *
      *                  * mento e si passa ad esaminare il documento  *
      *                  * successivo                                  *
      *                  *---------------------------------------------*
           if        rf-oct-sgl-num       not  = w-tes-tmo-orc-sgl
                     go to det-doc-ges-100.
      *                  *---------------------------------------------*
      *                  * Se invece la sigla numerazione e' la stessa *
      *                  * si memorizza il codice tipo movimento letto *
      *                  * per il caso in cui la determinazione doves- *
      *                  * se dare esito pari a 'X'                    *
      *                  *---------------------------------------------*
           move      rf-oct-tmo-orc       to   w-det-doc-ges-tmt      .
      *                  *---------------------------------------------*
      *                  * Quindi si ricicla per esaminare eventuali   *
      *                  * documenti successivi                        *
      *                  *---------------------------------------------*
           go to     det-doc-ges-100.
       det-doc-ges-800.
      *              *-------------------------------------------------*
      *              * Determinazioni finali per il caso in cui il do- *
      *              * cumento non sia stato sicuramente trovato       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se durante la ricerca e' stato trovato un   *
      *                  * documento con la data cercata, il numero    *
      *                  * cercato, e la sigla numerazione cercata :   *
      *                  * si esce con 'X', e con il codice del tipo   *
      *                  * movimento trovato gia' preparato; altri-    *
      *                  * menti si esce per documento non trovato     *
      *                  *---------------------------------------------*
           if        w-det-doc-ges-tmt    not  = spaces
                     move  "X"            to   w-det-doc-ges-snx
                     go to det-doc-ges-999
           else      go to det-doc-ges-900.
       det-doc-ges-900.
      *              *-------------------------------------------------*
      *              * Uscita per documento non trovato                *
      *              *-------------------------------------------------*
           move      "N"                  to   w-det-doc-ges-snx      .
       det-doc-ges-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per l'accettazione tipo movimento per ordini  *
      *    * clienti                                                   *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/acdezoc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione stato ordine cliente       *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/dstsorc0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .


