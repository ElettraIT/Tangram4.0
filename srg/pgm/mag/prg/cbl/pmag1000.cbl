       Identification Division.
       Program-Id.                                 pmag1000           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    mag                 *
      *                                Settore:    arc                 *
      *                                   Fase:    mag100              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 10/11/92    *
      *                       Ultima revisione:    NdK del 11/09/24    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione archivio ubicazioni di magazzino   *
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
                     "mag"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "arc"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "mag100"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pmag1000"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "         UBICAZIONI DI MAGAZZINO        "       .

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
      *        * [mau]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmau"                          .
      *        *-------------------------------------------------------*
      *        * [zmu]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfzmu"                          .
      *        *-------------------------------------------------------*
      *        * [zub]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfzub"                          .
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [dps]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dps/fls/rec/rfdps"                          .
      *        *-------------------------------------------------------*
      *        * [dpm]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dpm/fls/rec/rfdpm"                          .
      *        *-------------------------------------------------------*
      *        * [mtv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mtv/fls/rec/rfmtv"                          .

      *    *===========================================================*
      *    * Work-area per bufferizzazione testata                     *
      *    *-----------------------------------------------------------*
       01  w-tes.
      *        *-------------------------------------------------------*
      *        * Valori chiave                                         *
      *        *-------------------------------------------------------*
           05  w-tes-val-key.
               10  w-tes-cod-dpz          pic  9(02)                  .
               10  w-tes-tip-mag          pic  9(02)                  .
               10  w-tes-num-mag          pic  9(07)                  .
               10  w-tes-num-mag-alf      pic  x(14)                  .
               10  w-tes-num-mag-des      pic  x(40)                  .
               10  w-tes-var-mag          pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
               10  w-tes-tip-ubi          pic  x(03)                  .
               10  w-tes-tip-ubi-des      pic  x(15)                  .
               10  w-tes-tip-ubi-pdu      pic  9(01)                  .
               10  w-tes-tip-ubi-tbl occurs 4.
                   15  w-tes-tip-ubi-dpu  pic  x(10)                  .
               10  w-tes-prm-ubi     occurs 4
                                          pic  x(07)                  .
               10  w-tes-not-ubi          pic  x(20)                  .
               10  w-tes-inx-per          pic  9(07)                  .
               10  w-tes-alx-exp.
                   15  filler        occurs 80
                                          pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Si/No gestione prodotti di vendita attiva             *
      *        *-------------------------------------------------------*
           05  w-prs-dcp-snx              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No gestione semilavorati attiva                    *
      *        *-------------------------------------------------------*
           05  w-prs-dps-snx              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No gestione materie prime attiva                   *
      *        *-------------------------------------------------------*
           05  w-prs-dpm-snx              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No gestione materiali vari attiva                  *
      *        *-------------------------------------------------------*
           05  w-prs-mtv-snx              pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area referenze                                       *
      *    *-----------------------------------------------------------*
       01  w-ref.
      *        *-------------------------------------------------------*
      *        * Referenza relativa default per il tipo ubicazione     *
      *        *-------------------------------------------------------*
           05  w-ref-def-tub.
      *            *---------------------------------------------------*
      *            * Codice tipo movimento di default                  *
      *            *---------------------------------------------------*
               10  w-ref-def-tub-cod      pic  x(03)                  .

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
               10  w-let-arc-dcp-tpr      pic  x(01)                  .
               10  w-let-arc-dcp-umi      pic  x(03)                  .
               10  w-let-arc-dcp-deq      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dps]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dps.
               10  w-let-arc-dps-flg      pic  x(01)                  .
               10  w-let-arc-dps-num      pic  9(07)                  .
               10  w-let-arc-dps-alf      pic  x(14)                  .
               10  w-let-arc-dps-des      pic  x(40)                  .
               10  w-let-arc-dps-umi      pic  x(03)                  .
               10  w-let-arc-dps-deq      pic  9(01)                  .
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
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [mtv]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-mtv.
               10  w-let-arc-mtv-flg      pic  x(01)                  .
               10  w-let-arc-mtv-num      pic  9(07)                  .
               10  w-let-arc-mtv-alf      pic  x(14)                  .
               10  w-let-arc-mtv-des      pic  x(40)                  .
               10  w-let-arc-mtv-deq      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zmu]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zmu.
               10  w-let-arc-zmu-flg      pic  x(01)                  .
               10  w-let-arc-zmu-dpz      pic  9(02)                  .
               10  w-let-arc-zmu-cod      pic  x(03)                  .
               10  w-let-arc-zmu-des      pic  x(15)                  .
               10  w-let-arc-zmu-pdu      pic  9(01)                  .
               10  w-let-arc-zmu-tbl occurs 4.
                   15  w-let-arc-zmu-dpu  pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zub]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zub.
               10  w-let-arc-zub-flg      pic  x(01)                  .
               10  w-let-arc-zub-dpz      pic  9(02)                  .
               10  w-let-arc-zub-cod      pic  x(07)                  .
               10  w-let-arc-zub-des      pic  x(30)                  .
               10  w-let-arc-zub-inx      pic  9(07)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Tipo magazzino                                        *
      *        *-------------------------------------------------------*
           05  w-sav-tip-mag              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Codice magazzino                                      *
      *        *-------------------------------------------------------*
           05  w-sav-num-mag              pic  9(07)                  .
           05  w-sav-num-mag-alf          pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Tipo ubicazione                                       *
      *        *-------------------------------------------------------*
           05  w-sav-tip-ubi              pic  x(03)                  .

      *    *===========================================================*
      *    * Work-area per defaults di impostazione                    *
      *    *-----------------------------------------------------------*
       01  w-def.
           05  w-def-tip-mag              pic  9(02) value zero       .
           05  w-def-num-mag              pic  9(07) value zero       .

      *    *===========================================================*
      *    * Link-area per accettazione codice prodotto 'dcp'          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acoddcp0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice semilavorato            *
      *    *-----------------------------------------------------------*
           copy      "pgm/dps/prg/cpy/acoddps0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice materia prima           *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/acoddpm0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice materia varia           *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acodmtv0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice tipo ubicazione         *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/acdezmu0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice ubicazione              *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/acdezub0.acl"                   .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo magazzino                             *
      *        *-------------------------------------------------------*
           05  w-exp-tip-mag.
               10  w-exp-tip-mag-num      pic  9(02)       value 04   .
               10  w-exp-tip-mag-lun      pic  9(02)       value 25   .
               10  w-exp-tip-mag-tbl.
                   15  filler             pic  x(25) value
                            "Prodotto di vendita      "               .
                   15  filler             pic  x(25) value
                            "Semilavorato             "               .
                   15  filler             pic  x(25) value
                            "Materia prima            "               .
                   15  filler             pic  x(25) value
                            "Materiale vario          "               .
               10  w-exp-tip-mag-tbr redefines
                   w-exp-tip-mag-tbl.
                   15  w-exp-tip-mag-ele occurs 04
                                          pic  x(25)                  .
               10  w-exp-tip-mag-ast.
                   15  filler             pic  x(08) value "01020304" .
               10  w-exp-tip-mag-ass redefines
                   w-exp-tip-mag-ast.
                   15  w-exp-tip-mag-tpm occurs 04
                                          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo magazzino limitato ai soli tipi am-   *
      *        *            messi                                      *
      *        *-------------------------------------------------------*
           05  w-exp-tpm-amm.
               10  w-exp-tpm-amm-num      pic  9(02)                  .
               10  w-exp-tpm-amm-lun      pic  9(02) value 25         .
               10  w-exp-tpm-amm-tbl.
                   15  w-exp-tpm-amm-ele occurs 04
                                          pic  x(25)                  .
               10  w-exp-tpm-amm-ass.
                   15  w-exp-tpm-amm-tpm occurs 04
                                          pic  9(02)                  .
               10  w-exp-tpm-amm-i01      pic  9(02)                  .
               10  w-exp-tpm-amm-c01      pic  9(02)                  .
               10  w-exp-tpm-amm-c02      pic  9(02)                  .

      *    *===========================================================*
      *    * Area di interfaccia per sottoprogramma         "pazi000d" *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/pazi000d.pgl"                   .

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
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Si/No gestione prodotti di vendita attiva   *
      *                  *---------------------------------------------*
           perform   prs-dcp-snx-000      thru prs-dcp-snx-999        .
      *                  *---------------------------------------------*
      *                  * Si/No gestione semilavorati attiva          *
      *                  *---------------------------------------------*
           perform   prs-dps-snx-000      thru prs-dps-snx-999        .
      *                  *---------------------------------------------*
      *                  * Si/No gestione materie prime attiva         *
      *                  *---------------------------------------------*
           perform   prs-dpm-snx-000      thru prs-dpm-snx-999        .
      *                  *---------------------------------------------*
      *                  * Si/No gestione materiali vari attiva        *
      *                  *---------------------------------------------*
           perform   prs-mtv-snx-000      thru prs-mtv-snx-999        .
      *              *-------------------------------------------------*
      *              * Referenza per default tipo ubicazione           *
      *              *-------------------------------------------------*
           perform   ref-def-tub-000      thru ref-def-tub-999        .
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
                     go to pre-exe-pgm-050.
           move      "EN"                 to   w-dpz-tip-ope          .
           call      "pgm/azi/prg/obj/pazi000d"
                                         using w-dpz                  .
           cancel    "pgm/azi/prg/obj/pazi000d"                       .
           move      "#"                  to   w-cnt-pre-exe-pgm      .
           go to     pre-exe-pgm-999.
       pre-exe-pgm-050.
      *              *-------------------------------------------------*
      *              * Selezione codice dipendenza per il programma    *
      *              *-------------------------------------------------*
           move      "SD"                 to   w-dpz-tip-ope          .
           call      "pgm/azi/prg/obj/pazi000d"
                                         using w-dpz                  .
           cancel    "pgm/azi/prg/obj/pazi000d"                       .
      *              *-------------------------------------------------*
      *              * Se scelta non effettuata : uscita               *
      *              *-------------------------------------------------*
           if        w-dpz-cod-prg        =    zero
                     move  "#"            to   w-cnt-pre-exe-pgm
                     go to pre-exe-pgm-999.
      *              *-------------------------------------------------*
      *              * Determinazione tipi magazzino ammessi           *
      *              *-------------------------------------------------*
           perform   det-tpm-amm-000      thru det-tpm-amm-999        .
       pre-exe-pgm-100.
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice prodotto 'dcp'  *
      *              *-------------------------------------------------*
           perform   cod-cod-dcp-opn-000  thru cod-cod-dcp-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice 'dps'           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione semilavorati attiva        *
      *                  *---------------------------------------------*
           if        w-prs-dps-snx        not  = "S"
                     go to pre-exe-pgm-110.
      *                  *---------------------------------------------*
      *                  * Open modulo                                 *
      *                  *---------------------------------------------*
           perform   cod-cod-dps-opn-000  thru cod-cod-dps-opn-999    .
       pre-exe-pgm-110.
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice prodotto 'dpm'  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione materie prime attiva       *
      *                  *---------------------------------------------*
           if        w-prs-dpm-snx        not  = "S"
                     go to pre-exe-pgm-120.
      *                  *---------------------------------------------*
      *                  * Open modulo                                 *
      *                  *---------------------------------------------*
           perform   cod-cod-dpm-opn-000  thru cod-cod-dpm-opn-999    .
       pre-exe-pgm-120.
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice prodotto 'mtv'  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione materiali vari attiva      *
      *                  *---------------------------------------------*
           if        w-prs-mtv-snx        not  = "S"
                     go to pre-exe-pgm-130.
      *                  *---------------------------------------------*
      *                  * Open modulo                                 *
      *                  *---------------------------------------------*
           perform   cod-cod-mtv-opn-000  thru cod-cod-mtv-opn-999    .
       pre-exe-pgm-130.
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice tipo ubicazione *
      *              *-------------------------------------------------*
           perform   cod-des-zmu-opn-000  thru cod-des-zmu-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice ubicazione      *
      *              *-------------------------------------------------*
           perform   cod-des-zub-opn-000  thru cod-des-zub-opn-999    .
       pre-exe-pgm-200.
      *              *-------------------------------------------------*
      *              * Normalizzazione comodo per salvataggio valore   *
      *              * precedente                                      *
      *              *-------------------------------------------------*
           move      zero                 to   w-sav-num-mag          .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/No gestione prodotti di    *
      *    *                             vendita attiva                *
      *    *-----------------------------------------------------------*
       prs-dcp-snx-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dcp[snx]"       to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-dcp-snx
           else      move  "N"            to   w-prs-dcp-snx          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-dcp-snx        not  = "S" and
                     w-prs-dcp-snx        not  = "N"
                     move  "N"            to   w-prs-dcp-snx          .
       prs-dcp-snx-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/No gestione semilavorati   *
      *    *                             attiva                        *
      *    *-----------------------------------------------------------*
       prs-dps-snx-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dps[snx]"       to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-dps-snx
           else      move  "N"            to   w-prs-dps-snx          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-dps-snx        not  = "S" and
                     w-prs-dps-snx        not  = "N"
                     move  "N"            to   w-prs-dps-snx          .
       prs-dps-snx-999.
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
           else      move  "N"            to   w-prs-dpm-snx          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-dpm-snx        not  = "S" and
                     w-prs-dpm-snx        not  = "N"
                     move  "N"            to   w-prs-dpm-snx          .
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
      *    * Lettura delle referenze relative al default per il tipo   *
      *    * movimento                                                 *
      *    *-----------------------------------------------------------*
       ref-def-tub-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione work-area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-ref-def-tub-cod      .
       ref-def-tub-200.
      *              *-------------------------------------------------*
      *              * Lettura referenza                               *
      *              *-------------------------------------------------*
           move      "R:"                 to   s-ope                  .
           move      "pgm/mag/arc/mag100[def-tub]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-ref-def-tub
           else      move  spaces         to   w-ref-def-tub          .
       ref-def-tub-400.
      *              *-------------------------------------------------*
      *              * Normalizzazione valori letti                    *
      *              *-------------------------------------------------*
       ref-def-tub-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice prodotto 'dcp' *
      *              *-------------------------------------------------*
           perform   cod-cod-dcp-cls-000  thru cod-cod-dcp-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice prodotto 'dps' *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione semilavorati attiva        *
      *                  *---------------------------------------------*
           if        w-prs-dps-snx        not  = "S"
                     go to pos-exe-pgm-010.
      *                  *---------------------------------------------*
      *                  * Close modulo                                *
      *                  *---------------------------------------------*
           perform   cod-cod-dps-cls-000  thru cod-cod-dps-cls-999    .
       pos-exe-pgm-010.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice prodotto 'dpm' *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione materie prime attiva       *
      *                  *---------------------------------------------*
           if        w-prs-dpm-snx        not  = "S"
                     go to pos-exe-pgm-020.
      *                  *---------------------------------------------*
      *                  * Close modulo                                *
      *                  *---------------------------------------------*
           perform   cod-cod-dpm-cls-000  thru cod-cod-dpm-cls-999    .
       pos-exe-pgm-020.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice prodotto 'mtv' *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione materiali vari attiva      *
      *                  *---------------------------------------------*
           if        w-prs-mtv-snx        not  = "S"
                     go to pos-exe-pgm-030.
      *                  *---------------------------------------------*
      *                  * Close modulo                                *
      *                  *---------------------------------------------*
           perform   cod-cod-mtv-cls-000  thru cod-cod-mtv-cls-999    .
       pos-exe-pgm-030.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice tipo ubicaz.   *
      *              *-------------------------------------------------*
           perform   cod-des-zmu-cls-000  thru cod-des-zmu-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice ubicazione     *
      *              *-------------------------------------------------*
           perform   cod-des-zub-cls-000  thru cod-des-zub-cls-999    .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * [mau]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmau"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mau                 .
      *              *-------------------------------------------------*
      *              * [zmu]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzmu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zmu                 .
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
      *              * [dps]                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione semilavorati attiva        *
      *                  *---------------------------------------------*
           if        w-prs-dps-snx        not  = "S"
                     go to rou-opn-fls-020.
      *                  *---------------------------------------------*
      *                  * Apertura file                               *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
       rou-opn-fls-020.
      *              *-------------------------------------------------*
      *              * [dpm]                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione materie prime attiva       *
      *                  *---------------------------------------------*
           if        w-prs-dpm-snx        not  = "S"
                     go to rou-opn-fls-030.
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
       rou-opn-fls-030.
      *              *-------------------------------------------------*
      *              * [mtv]                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione materiali vari attiva      *
      *                  *---------------------------------------------*
           if        w-prs-mtv-snx        not  = "S"
                     go to rou-opn-fls-999.
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
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [mau]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmau"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mau                 .
      *              *-------------------------------------------------*
      *              * [zmu]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzmu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zmu                 .
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
      *              * [dps]                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione semilavorati attiva        *
      *                  *---------------------------------------------*
           if        w-prs-dps-snx        not  = "S"
                     go to rou-cls-fls-020.
      *                  *---------------------------------------------*
      *                  * Chiusura file                               *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
       rou-cls-fls-020.
      *              *-------------------------------------------------*
      *              * [dpm]                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione materie prime attiva       *
      *                  *---------------------------------------------*
           if        w-prs-dpm-snx        not  = "S"
                     go to rou-cls-fls-030.
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
       rou-cls-fls-030.
      *              *-------------------------------------------------*
      *              * [mtv]                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione materiali vari attiva      *
      *                  *---------------------------------------------*
           if        w-prs-mtv-snx        not  = "S"
                     go to rou-cls-fls-999.
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
      *              * Pre-accettazioni                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione codice dipendenza, automatico  *
      *                  * da codice dipendenza per l'intero program-  *
      *                  * ma                                          *
      *                  *---------------------------------------------*
           move      w-dpz-cod-prg        to   w-tes-cod-dpz          .
      *                  *---------------------------------------------*
      *                  * Preparazione variante di magazzino          *
      *                  *---------------------------------------------*
           move      spaces               to   w-tes-var-mag          .
       acc-key-reg-200.
      *              *-------------------------------------------------*
      *              * Accettazioni                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Tipo magazzino                              *
      *                  *---------------------------------------------*
           perform   acc-tip-mag-000      thru acc-tip-mag-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
       acc-key-reg-300.
      *                  *---------------------------------------------*
      *                  * Codice magazzino                            *
      *                  *---------------------------------------------*
           perform   acc-num-mag-000      thru acc-num-mag-999        .
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
      *              * Tipo magazzino                                  *
      *              *-------------------------------------------------*
           perform   pmt-tip-mag-000      thru pmt-tip-mag-999        .
      *              *-------------------------------------------------*
      *              * Codice magazzino                                *
      *              *-------------------------------------------------*
           perform   pmt-num-mag-000      thru pmt-num-mag-999        .
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
           move      06                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Tipo magazzino                                  *
      *              *-------------------------------------------------*
           perform   pmt-tip-mag-000      thru pmt-tip-mag-999        .
      *              *-------------------------------------------------*
      *              * Codice magazzino                                *
      *              *-------------------------------------------------*
           perform   pmt-num-mag-000      thru pmt-num-mag-999        .
      *              *-------------------------------------------------*
      *              * Linea di trattini                               *
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
      *    * Visualizzazione prompts per Tipo magazzino                *
      *    *-----------------------------------------------------------*
       pmt-tip-mag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      23                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo   magazzino      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-tip-mag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Codice magazzino              *
      *    *-----------------------------------------------------------*
       pmt-num-mag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      23                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice magazzino      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-num-mag-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Tipo prodotto                 *
      *    *-----------------------------------------------------------*
       acc-tip-mag-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
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
           move      spaces               to   v-edm                  .
           move      w-exp-tpm-amm-tbl    to   v-txt                  .
           move      04                   to   v-lin                  .
           move      25                   to   v-pos                  .
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
      *                  * Aggiornamento valore di default             *
      *                  *---------------------------------------------*
           move      w-tes-tip-mag        to   w-def-tip-mag          .
      *                  *---------------------------------------------*
      *                  * Normalizzazione codice e descrizione della  *
      *                  * voce di magazzino                           *
      *                  *---------------------------------------------*
           if        w-tes-num-mag        =    zero
                     go to acc-tip-mag-800.
           move      zero                 to   w-tes-num-mag          .
           move      spaces               to   w-tes-num-mag-alf      .
           move      spaces               to   w-tes-num-mag-des      .
           perform   vis-num-mag-alf-000  thru vis-num-mag-alf-999    .
           perform   vis-num-mag-des-000  thru vis-num-mag-des-999    .
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
      *    * Visualizzazione campo chiave : Tipo magazzino             *
      *    *-----------------------------------------------------------*
       vis-tip-mag-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-mag-lun    to   v-car                  .
           move      w-exp-tip-mag-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-mag-tbl    to   v-txt                  .
           move      04                   to   v-lin                  .
           move      25                   to   v-pos                  .
           move      w-tes-tip-mag        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-mag-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Codice magazzino              *
      *    *-----------------------------------------------------------*
       acc-num-mag-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di distinta     *
      *              *-------------------------------------------------*
           if        w-tes-tip-mag        =    01
                     go to acc-num-mag-100
           else if   w-tes-tip-mag        =    02
                     go to acc-num-mag-200
           else if   w-tes-tip-mag        =    03
                     go to acc-num-mag-300
           else if   w-tes-tip-mag        =    04
                     go to acc-num-mag-400
           else      go to acc-num-mag-999.
       acc-num-mag-100.
      *                  *---------------------------------------------*
      *                  * Se Prodotto di vendita                      *
      *                  *---------------------------------------------*
           perform   acc-cod-pro-000      thru acc-cod-pro-999        .
           go to     acc-num-mag-999.
       acc-num-mag-200.
      *                  *---------------------------------------------*
      *                  * Se Semilavorato                             *
      *                  *---------------------------------------------*
           perform   acc-cod-sem-000      thru acc-cod-sem-999        .
           go to     acc-num-mag-999.
       acc-num-mag-300.
      *                  *---------------------------------------------*
      *                  * Se Materia prima                            *
      *                  *---------------------------------------------*
           perform   acc-cod-map-000      thru acc-cod-map-999        .
           go to     acc-num-mag-999.
       acc-num-mag-400.
      *                  *---------------------------------------------*
      *                  * Se Materiale vario                          *
      *                  *---------------------------------------------*
           perform   acc-cod-mtv-000      thru acc-cod-mtv-999        .
           go to     acc-num-mag-999.
       acc-num-mag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Codice alfa magazzino      *
      *    *-----------------------------------------------------------*
       vis-num-mag-alf-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      25                   to   v-pos                  .
           move      w-tes-num-mag-alf    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-mag-alf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione magazzino     *
      *    *-----------------------------------------------------------*
       vis-num-mag-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-num-mag-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-mag-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Codice prodotto alfanumerico  *
      *    *-----------------------------------------------------------*
       acc-cod-pro-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-pro-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-dcp-ope      .
           move      "A"                  to   w-cod-cod-dcp-tac      .
           move      w-tes-num-mag        to   w-cod-cod-dcp-num      .
           move      w-tes-num-mag-alf    to   w-cod-cod-dcp-alf      .
           move      05                   to   w-cod-cod-dcp-lin      .
           move      25                   to   w-cod-cod-dcp-pos      .
           move      05                   to   w-cod-cod-dcp-dln      .
           move      41                   to   w-cod-cod-dcp-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
      *
           if        w-sav-num-mag        not  = zero
                     move "SLCT"          to   v-pfk (11)             .
      *
           perform   cod-cod-dcp-cll-000  thru cod-cod-dcp-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-dcp-foi-000  thru cod-cod-dcp-foi-999    .
       acc-cod-pro-110.
           perform   cod-cod-dcp-cll-000  thru cod-cod-dcp-cll-999    .
           if        w-cod-cod-dcp-ope    =    "F+"
                     go to acc-cod-pro-115.
           if        w-cod-cod-dcp-ope    =    "AC"
                     go to acc-cod-pro-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-pro-115.
           perform   cod-cod-dcp-foi-000  thru cod-cod-dcp-foi-999    .
           go to     acc-cod-pro-110.
       acc-cod-pro-120.
           move      w-cod-cod-dcp-num    to   v-num                  .
           move      w-cod-cod-dcp-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-pro-999.
       acc-cod-pro-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-num-mag          .
           move      v-alf                to   w-tes-num-mag-alf      .
       acc-cod-pro-300.
      *              *-------------------------------------------------*
      *              * Se Slct                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to acc-cod-pro-400.
      *                  *---------------------------------------------*
      *                  * Ultimo codice salvato                       *
      *                  *---------------------------------------------*
           move      w-sav-num-mag        to   w-tes-num-mag          .
           move      w-sav-num-mag-alf    to   w-tes-num-mag-alf      .
      *                  *---------------------------------------------*
      *                  * Ad accettazione                             *
      *                  *---------------------------------------------*
           go to     acc-cod-pro-100.
       acc-cod-pro-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [dcp]                      *
      *                  *---------------------------------------------*
           move      w-tes-num-mag        to   w-let-arc-dcp-num      .
           perform   let-arc-dcp-000      thru let-arc-dcp-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-dcp-des    to   w-tes-num-mag-des      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-num-mag-des-000  thru vis-num-mag-des-999    .
      *                  *---------------------------------------------*
      *                  * Se valore non trovato : reimpostazione      *
      *                  *---------------------------------------------*
           if        w-let-arc-dcp-flg    not  = spaces
                     go to acc-cod-pro-100.
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione a meno  *
      *                  * che non sia stato premuto 'Up'              *
      *                  *---------------------------------------------*
           if        w-tes-num-mag-alf    not  = spaces
                     go to acc-cod-pro-500.
           if        v-key                =    "UP  "
                     go to acc-cod-pro-600
           else      go to acc-cod-pro-100.
       acc-cod-pro-500.
      *                  *---------------------------------------------*
      *                  * Controllo che il tipo prodotto sia 'M'      *
      *                  *---------------------------------------------*
           if        w-let-arc-dcp-tpr    =    "M"
                     go to acc-cod-pro-600.
           move      "Il prodotto deve essere di tipo : Merce "
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     acc-cod-pro-100.
       acc-cod-pro-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-num-mag        to   w-sav-num-mag          .
           move      w-tes-num-mag-alf    to   w-sav-num-mag-alf      .
       acc-cod-pro-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-pro-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-pro-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-pro-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-pro-999.
       acc-cod-pro-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice alfa semilavorato     *
      *    *-----------------------------------------------------------*
       acc-cod-sem-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-sem-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-dps-ope      .
           move      "A"                  to   w-cod-cod-dps-tac      .
           move      w-tes-num-mag        to   w-cod-cod-dps-num      .
           move      w-tes-num-mag-alf    to   w-cod-cod-dps-alf      .
           move      05                   to   w-cod-cod-dps-lin      .
           move      25                   to   w-cod-cod-dps-pos      .
           move      05                   to   w-cod-cod-dps-dln      .
           move      41                   to   w-cod-cod-dps-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "NXSC"               to   v-pfk (08)             .
           perform   cod-cod-dps-cll-000  thru cod-cod-dps-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-dps-foi-000  thru cod-cod-dps-foi-999    .
       acc-cod-sem-110.
           perform   cod-cod-dps-cll-000  thru cod-cod-dps-cll-999    .
           if        w-cod-cod-dps-ope    =    "F+"
                     go to acc-cod-sem-115.
           if        w-cod-cod-dps-ope    =    "AC"
                     go to acc-cod-sem-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-sem-115.
           perform   cod-cod-dps-foi-000  thru cod-cod-dps-foi-999    .
           go to     acc-cod-sem-110.
       acc-cod-sem-120.
           move      w-cod-cod-dps-num    to   v-num                  .
           move      w-cod-cod-dps-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-sem-999.
       acc-cod-sem-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-num-mag          .
           move      v-alf                to   w-tes-num-mag-alf      .
       acc-cod-sem-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [dps]                      *
      *                  *---------------------------------------------*
           move      w-tes-num-mag        to   w-let-arc-dps-num      .
           perform   let-arc-dps-000      thru let-arc-dps-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-dps-des    to   w-tes-num-mag-des      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-num-mag-des-000  thru vis-num-mag-des-999    .
      *                  *---------------------------------------------*
      *                  * Se valore non trovato : reimpostazione      *
      *                  *---------------------------------------------*
           if        w-let-arc-dps-flg    not  = spaces
                     go to acc-cod-sem-100.
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione a meno  *
      *                  * che non sia stato premuto 'Up'              *
      *                  *---------------------------------------------*
           if        w-tes-num-mag-alf    not  = spaces
                     go to acc-cod-sem-600.
           if        v-key                =    "UP  "
                     go to acc-cod-sem-600
           else      go to acc-cod-sem-100.
       acc-cod-sem-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-sem-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-sem-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-sem-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-sem-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-sem-999.
       acc-cod-sem-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice materia prima         *
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
           move      "A"                  to   w-cod-cod-dpm-tac      .
           move      w-tes-num-mag        to   w-cod-cod-dpm-num      .
           move      w-tes-num-mag-alf    to   w-cod-cod-dpm-alf      .
           move      05                   to   w-cod-cod-dpm-lin      .
           move      25                   to   w-cod-cod-dpm-pos      .
           move      05                   to   w-cod-cod-dpm-dln      .
           move      41                   to   w-cod-cod-dpm-dps      .
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
           move      w-cod-cod-dpm-num    to   v-num                  .
           move      w-cod-cod-dpm-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-map-999.
       acc-cod-map-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-num-mag          .
           move      v-alf                to   w-tes-num-mag-alf      .
       acc-cod-map-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [dpm]                      *
      *                  *---------------------------------------------*
           move      w-tes-num-mag        to   w-let-arc-dpm-num      .
           perform   let-arc-dpm-000      thru let-arc-dpm-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-dpm-des    to   w-tes-num-mag-des      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-num-mag-des-000  thru vis-num-mag-des-999    .
      *                  *---------------------------------------------*
      *                  * Se valore non trovato : reimpostazione      *
      *                  *---------------------------------------------*
           if        w-let-arc-dpm-flg    not  = spaces
                     go to acc-cod-map-100.
      *                  *---------------------------------------------*
      *                  * Se valore a zero : reimpostazione a meno    *
      *                  * che non sia stato premuto 'Up'              *
      *                  *---------------------------------------------*
           if        w-tes-num-mag-alf    not  = spaces
                     go to acc-cod-map-600.
           if        v-key                =    "UP  "
                     go to acc-cod-map-600
           else      go to acc-cod-map-100.
       acc-cod-map-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-map-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-map-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-map-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-map-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-map-999.
       acc-cod-map-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice materia varia         *
      *    *-----------------------------------------------------------*
       acc-cod-mtv-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-mtv-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-mtv-ope      .
           move      "A"                  to   w-cod-cod-mtv-tac      .
           move      w-tes-num-mag        to   w-cod-cod-mtv-num      .
           move      w-tes-num-mag-alf    to   w-cod-cod-mtv-alf      .
           move      05                   to   w-cod-cod-mtv-lin      .
           move      25                   to   w-cod-cod-mtv-pos      .
           move      05                   to   w-cod-cod-mtv-dln      .
           move      41                   to   w-cod-cod-mtv-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-cod-mtv-cll-000  thru cod-cod-mtv-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-mtv-foi-000  thru cod-cod-mtv-foi-999    .
       acc-cod-mtv-110.
           perform   cod-cod-mtv-cll-000  thru cod-cod-mtv-cll-999    .
           if        w-cod-cod-mtv-ope    =    "F+"
                     go to acc-cod-mtv-115.
           if        w-cod-cod-mtv-ope    =    "AC"
                     go to acc-cod-mtv-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-mtv-115.
           perform   cod-cod-mtv-foi-000  thru cod-cod-mtv-foi-999    .
           go to     acc-cod-mtv-110.
       acc-cod-mtv-120.
           move      w-cod-cod-mtv-num    to   v-num                  .
           move      w-cod-cod-mtv-alf    to   v-alf                  .
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
           move      v-num                to   w-tes-num-mag          .
           move      v-alf                to   w-tes-num-mag-alf      .
       acc-cod-mtv-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [mtv]                      *
      *                  *---------------------------------------------*
           move      w-tes-num-mag        to   w-let-arc-mtv-num      .
           perform   let-arc-mtv-000      thru let-arc-mtv-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-mtv-des    to   w-tes-num-mag-des      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-num-mag-des-000  thru vis-num-mag-des-999    .
      *                  *---------------------------------------------*
      *                  * Se valore non trovato : reimpostazione      *
      *                  *---------------------------------------------*
           if        w-let-arc-mtv-flg    not  = spaces
                     go to acc-cod-mtv-100.
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione a meno  *
      *                  * che non sia stato premuto 'Up'              *
      *                  *---------------------------------------------*
           if        w-tes-num-mag-alf    not  = spaces
                     go to acc-cod-mtv-600.
           if        v-key                =    "UP  "
                     go to acc-cod-mtv-600
           else      go to acc-cod-mtv-100.
       acc-cod-mtv-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
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
      *              * La testata e' composta di nr. 1 pagine          *
      *              *-------------------------------------------------*
           move      1                    to   w-cnt-sts-imp-mpt      .
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
      *                  * Tipo ubicazione                             *
      *                  *---------------------------------------------*
           perform   acc-tip-ubi-000      thru acc-tip-ubi-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-110.
      *                  *---------------------------------------------*
      *                  * Parametro di ubicazione 1                   *
      *                  *---------------------------------------------*
           perform   acc-prm-ub1-000      thru acc-prm-ub1-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-100.
       acc-tes-reg-120.
      *                  *---------------------------------------------*
      *                  * Parametro di ubicazione 2                   *
      *                  *---------------------------------------------*
           perform   acc-prm-ub2-000      thru acc-prm-ub2-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-110.
       acc-tes-reg-130.
      *                  *---------------------------------------------*
      *                  * Parametro di ubicazione 3                   *
      *                  *---------------------------------------------*
           perform   acc-prm-ub3-000      thru acc-prm-ub3-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-120.
       acc-tes-reg-140.
      *                  *---------------------------------------------*
      *                  * Parametro di ubicazione 4                   *
      *                  *---------------------------------------------*
           perform   acc-prm-ub4-000      thru acc-prm-ub4-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-130.
       acc-tes-reg-160.
      *                  *---------------------------------------------*
      *                  * Note                                        *
      *                  *---------------------------------------------*
           perform   acc-not-ubi-000      thru acc-not-ubi-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-140.
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
      *              * Tipo ubicazione                                 *
      *              *-------------------------------------------------*
           perform   vis-tip-ubi-000      thru vis-tip-ubi-999        .
           perform   vis-tip-ubi-des-000  thru vis-tip-ubi-des-999    .
      *              *-------------------------------------------------*
      *              * Parametro di ubicazione 1                       *
      *              *-------------------------------------------------*
           perform   vis-prm-ub1-000      thru vis-prm-ub1-999        .
      *              *-------------------------------------------------*
      *              * Indice di percorso                              *
      *              *-------------------------------------------------*
           perform   vis-inx-per-000      thru vis-inx-per-999        .
      *              *-------------------------------------------------*
      *              * Parametro di ubicazione 2                       *
      *              *-------------------------------------------------*
           perform   vis-prm-ub2-000      thru vis-prm-ub2-999        .
      *              *-------------------------------------------------*
      *              * Parametro di ubicazione 3                       *
      *              *-------------------------------------------------*
           perform   vis-prm-ub3-000      thru vis-prm-ub3-999        .
      *              *-------------------------------------------------*
      *              * Parametro di ubicazione 4                       *
      *              *-------------------------------------------------*
           perform   vis-prm-ub4-000      thru vis-prm-ub4-999        .
      *              *-------------------------------------------------*
      *              * Note                                            *
      *              *-------------------------------------------------*
           perform   vis-not-ubi-000      thru vis-not-ubi-999        .
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
      *              * Tipo ubicazione                                 *
      *              *-------------------------------------------------*
           perform   pmt-tip-ubi-000      thru pmt-tip-ubi-999        .
      *              *-------------------------------------------------*
      *              * Parametro di ubicazione 1                       *
      *              *-------------------------------------------------*
           perform   pmt-prm-ub1-000      thru pmt-prm-ub1-999        .
      *              *-------------------------------------------------*
      *              * Parametro di ubicazione 2                       *
      *              *-------------------------------------------------*
           perform   pmt-prm-ub2-000      thru pmt-prm-ub2-999        .
      *              *-------------------------------------------------*
      *              * Parametro di ubicazione 3                       *
      *              *-------------------------------------------------*
           perform   pmt-prm-ub3-000      thru pmt-prm-ub3-999        .
      *              *-------------------------------------------------*
      *              * Parametro di ubicazione 4                       *
      *              *-------------------------------------------------*
           perform   pmt-prm-ub4-000      thru pmt-prm-ub4-999        .
      *              *-------------------------------------------------*
      *              * Indice di percorso                              *
      *              *-------------------------------------------------*
           perform   pmt-inx-per-000      thru pmt-inx-per-999        .
      *              *-------------------------------------------------*
      *              * Note                                            *
      *              *-------------------------------------------------*
           perform   pmt-not-ubi-000      thru pmt-not-ubi-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-tes-reg-999.
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tipo ubicazione                  *
      *    *-----------------------------------------------------------*
       pmt-tip-ubi-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo ubicazione            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-ubi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Parametro di ubicazione 1        *
      *    *-----------------------------------------------------------*
       pmt-prm-ub1-000.
      *              *-------------------------------------------------*
      *              * Se prompt a spaces : normalizzazione            *
      *              *-------------------------------------------------*
           if        w-tes-tip-ubi-dpu (1, 1)
                                          not  = spaces
                     go to pmt-prm-ub1-200.
      *              *-------------------------------------------------*
      *              * Prompt normalizzato                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     pmt-prm-ub1-999.
       pmt-prm-ub1-200.
      *              *-------------------------------------------------*
      *              * Prompt parametro 1                              *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-tes-tip-ubi-dpu (1, 1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * ':'                                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      28                   to   v-pos                  .
           move      ":"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-prm-ub1-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Parametro di ubicazione 2        *
      *    *-----------------------------------------------------------*
       pmt-prm-ub2-000.
      *              *-------------------------------------------------*
      *              * Se prompt a spaces : normalizzazione            *
      *              *-------------------------------------------------*
           if        w-tes-tip-ubi-dpu (1, 2)
                                          not  = spaces
                     go to pmt-prm-ub2-200.
      *              *-------------------------------------------------*
      *              * Prompt normalizzato                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     pmt-prm-ub2-999.
       pmt-prm-ub2-200.
      *              *-------------------------------------------------*
      *              * Prompt parametro 2                              *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-tes-tip-ubi-dpu (1, 2)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * ':'                                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      28                   to   v-pos                  .
           move      ":"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-prm-ub2-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Parametro di ubicazione 3        *
      *    *-----------------------------------------------------------*
       pmt-prm-ub3-000.
      *              *-------------------------------------------------*
      *              * Se prompt a spaces : normalizzazione            *
      *              *-------------------------------------------------*
           if        w-tes-tip-ubi-dpu (1, 3)
                                          not  = spaces
                     go to pmt-prm-ub3-200.
      *              *-------------------------------------------------*
      *              * Prompt normalizzato                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     pmt-prm-ub3-999.
       pmt-prm-ub3-200.
      *              *-------------------------------------------------*
      *              * Prompt parametro 3                              *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-tes-tip-ubi-dpu (1, 3)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * ':'                                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      28                   to   v-pos                  .
           move      ":"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-prm-ub3-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Parametro di ubicazione 4        *
      *    *-----------------------------------------------------------*
       pmt-prm-ub4-000.
      *              *-------------------------------------------------*
      *              * Se prompt a spaces : normalizzazione            *
      *              *-------------------------------------------------*
           if        w-tes-tip-ubi-dpu (1, 4)
                                          not  = spaces
                     go to pmt-prm-ub4-200.
      *              *-------------------------------------------------*
      *              * Prompt normalizzato                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     pmt-prm-ub4-999.
       pmt-prm-ub4-200.
      *              *-------------------------------------------------*
      *              * Prompt parametro 4                              *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-tes-tip-ubi-dpu (1, 4)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * ':'                                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      28                   to   v-pos                  .
           move      ":"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-prm-ub4-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Indice di percorso               *
      *    *-----------------------------------------------------------*
       pmt-inx-per-000.
      *              *-------------------------------------------------*
      *              * Prompt                                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Indice percorso"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Prompt parametro 1                              *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      17                   to   v-pos                  .
           move      w-tes-tip-ubi-dpu (1, 1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * ':'                                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      28                   to   v-pos                  .
           move      ":"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-inx-per-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Note per l'ubicazione            *
      *    *-----------------------------------------------------------*
       pmt-not-ubi-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Note                       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-not-ubi-999.
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
      *    * Accettazione campo testata : Tipo di ubicazione           *
      *    *-----------------------------------------------------------*
       acc-tip-ubi-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        w-tes-tip-ubi (1)    not  = spaces
                     go to acc-tip-ubi-100.
           if        w-ref-def-tub-cod    =    spaces
                     go to acc-tip-ubi-100.
           move      w-ref-def-tub-cod    to   w-tes-tip-ubi (1)      .
           perform   vis-tip-ubi-000      thru vis-tip-ubi-999        .
           go to     acc-tip-ubi-400.
       acc-tip-ubi-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-zmu-ope      .
           move      w-tes-cod-dpz        to   w-cod-des-zmu-dpz      .
           move      w-tes-tip-ubi (1)    to   w-cod-des-zmu-cod      .
           move      08                   to   w-cod-des-zmu-lin      .
           move      30                   to   w-cod-des-zmu-pos      .
           move      08                   to   w-cod-des-zmu-dln      .
           move      41                   to   w-cod-des-zmu-dps      .
           move      spaces               to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-des-zmu-cll-000  thru cod-des-zmu-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-zmu-foi-000  thru cod-des-zmu-foi-999    .
       acc-tip-ubi-110.
           perform   cod-des-zmu-cll-000  thru cod-des-zmu-cll-999    .
           if        w-cod-des-zmu-ope    =    "F+"
                     go to acc-tip-ubi-115.
           if        w-cod-des-zmu-ope    =    "AC"
                     go to acc-tip-ubi-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-tip-ubi-115.
           perform   cod-des-zmu-foi-000  thru cod-des-zmu-foi-999    .
           go to     acc-tip-ubi-110.
       acc-tip-ubi-120.
           move      w-cod-des-zmu-cod    to   v-alf                  .
       acc-tip-ubi-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-tip-ubi-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-tip-ubi-999.
       acc-tip-ubi-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-tip-ubi (1)      .
       acc-tip-ubi-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zmu]                      *
      *                  *---------------------------------------------*
           move      w-tes-cod-dpz        to   w-let-arc-zmu-dpz      .
           move      w-tes-tip-ubi (1)    to   w-let-arc-zmu-cod      .
           perform   let-arc-zmu-000      thru let-arc-zmu-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione valori                       *
      *                  *---------------------------------------------*
           move      w-let-arc-zmu-des    to   w-tes-tip-ubi-des (1)  .
           move      w-let-arc-zmu-pdu    to   w-tes-tip-ubi-pdu (1)  .
           move      w-let-arc-zmu-dpu (1)
                                          to   w-tes-tip-ubi-dpu
                                              (1, 1)                  .
           move      w-let-arc-zmu-dpu (2)
                                          to   w-tes-tip-ubi-dpu
                                              (1, 2)                  .
           move      w-let-arc-zmu-dpu (3)
                                          to   w-tes-tip-ubi-dpu
                                              (1, 3)                  .
           move      w-let-arc-zmu-dpu (4)
                                          to   w-tes-tip-ubi-dpu
                                              (1, 4)                  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-tip-ubi-des-000  thru vis-tip-ubi-des-999    .
      *                  *---------------------------------------------*
      *                  * Visualizzazione parametri di ubicazione     *
      *                  *---------------------------------------------*
           perform   pmt-prm-ub1-000      thru pmt-prm-ub1-999        .
           perform   pmt-prm-ub2-000      thru pmt-prm-ub2-999        .
           perform   pmt-prm-ub3-000      thru pmt-prm-ub3-999        .
           perform   pmt-prm-ub4-000      thru pmt-prm-ub4-999        .
           perform   pmt-inx-per-000      thru pmt-inx-per-999        .
      *                  *---------------------------------------------*
      *                  * Se valore non trovato : reimpostazione      *
      *                  *---------------------------------------------*
           if        w-let-arc-zmu-flg    not  = spaces
                     go to acc-tip-ubi-100.
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione         *
      *                  *---------------------------------------------*
           if        w-tes-tip-ubi (1)    not  = spaces
                     go to acc-tip-ubi-600.
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-tip-ubi-100.
       acc-tip-ubi-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione eventuale dei parametri di  *
      *                  * ubicazione                                  *
      *                  *---------------------------------------------*
           if        w-tes-tip-ubi-pdu (1)
                                          =    zero
                     go to acc-tip-ubi-610
           else if   w-tes-tip-ubi-pdu (1)
                                          =    1
                     go to acc-tip-ubi-620
           else if   w-tes-tip-ubi-pdu (1)
                                          =    2
                     go to acc-tip-ubi-630
           else if   w-tes-tip-ubi-pdu (1)
                                          =    3
                     go to acc-tip-ubi-640
           else      go to acc-tip-ubi-800.
       acc-tip-ubi-610.
           move      spaces               to   w-tes-prm-ubi (1, 1)   .
           perform   vis-prm-ub1-000      thru vis-prm-ub1-999        .
       acc-tip-ubi-620.
           move      spaces               to   w-tes-prm-ubi (1, 2)   .
           perform   vis-prm-ub2-000      thru vis-prm-ub2-999        .
       acc-tip-ubi-630.
           move      spaces               to   w-tes-prm-ubi (1, 3)   .
           perform   vis-prm-ub3-000      thru vis-prm-ub3-999        .
       acc-tip-ubi-640.
           move      spaces               to   w-tes-prm-ubi (1, 4)   .
           perform   vis-prm-ub4-000      thru vis-prm-ub4-999        .
       acc-tip-ubi-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-tip-ubi-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-tip-ubi-100.
       acc-tip-ubi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Tipo ubicazione           *
      *    *-----------------------------------------------------------*
       vis-tip-ubi-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-tip-ubi (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-ubi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione ubicazione    *
      *    *-----------------------------------------------------------*
       vis-tip-ubi-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-tip-ubi-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-ubi-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Parametro 1                  *
      *    *-----------------------------------------------------------*
       acc-prm-ub1-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-prm-ub1-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-zub-ope      .
           move      w-tes-cod-dpz        to   w-cod-des-zub-dpz      .
           move      w-tes-prm-ubi (1, 1) to   w-cod-des-zub-cod      .
           move      10                   to   w-cod-des-zub-lin      .
           move      30                   to   w-cod-des-zub-pos      .
           move      zero                 to   w-cod-des-zub-dln      .
           move      zero                 to   w-cod-des-zub-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-des-zub-cll-000  thru cod-des-zub-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-zub-foi-000  thru cod-des-zub-foi-999    .
       acc-prm-ub1-110.
           perform   cod-des-zub-cll-000  thru cod-des-zub-cll-999    .
           if        w-cod-des-zub-ope    =    "F+"
                     go to acc-prm-ub1-115.
           if        w-cod-des-zub-ope    =    "AC"
                     go to acc-prm-ub1-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-prm-ub1-115.
           perform   cod-des-zub-foi-000  thru cod-des-zub-foi-999    .
           go to     acc-prm-ub1-110.
       acc-prm-ub1-120.
           move      w-cod-des-zub-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-prm-ub1-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-prm-ub1-999.
       acc-prm-ub1-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-prm-ubi (1, 1)   .
       acc-prm-ub1-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      w-tes-prm-ubi (1, 1) to   w-all-str-alf          .
           move      07                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-prm-ub1-100.
       acc-prm-ub1-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se codice vuoto                        *
      *                  *---------------------------------------------*
           if        w-tes-prm-ubi (1, 1) not  = spaces
                     go to acc-prm-ub1-700.
      *                  *---------------------------------------------*
      *                  * Eventuale compattamento ubicazioni          *
      *                  *---------------------------------------------*
           move      w-tes-prm-ubi (1, 2) to   w-tes-prm-ubi (1, 1)   .
           move      w-tes-prm-ubi (1, 3) to   w-tes-prm-ubi (1, 2)   .
           move      w-tes-prm-ubi (1, 4) to   w-tes-prm-ubi (1, 3)   .
           move      spaces               to   w-tes-prm-ubi (1, 4)   .
           perform   vis-prm-ub1-000      thru vis-prm-ub1-999        .
           perform   vis-prm-ub2-000      thru vis-prm-ub2-999        .
           perform   vis-prm-ub3-000      thru vis-prm-ub3-999        .
           perform   vis-prm-ub4-000      thru vis-prm-ub4-999        .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     acc-prm-ub1-800.
       acc-prm-ub1-700.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zub]                      *
      *                  *---------------------------------------------*
           move      w-tes-cod-dpz        to   w-let-arc-zub-dpz      .
           move      w-tes-prm-ubi (1, 1) to   w-let-arc-zub-cod      .
           perform   let-arc-zub-000      thru let-arc-zub-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione indice di percorso           *
      *                  *---------------------------------------------*
           move      w-let-arc-zub-inx    to   w-tes-inx-per (1)      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione indice di percorso          *
      *                  *---------------------------------------------*
           perform   vis-inx-per-000      thru vis-inx-per-999        .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        w-let-arc-zub-flg    =    spaces
                     go to acc-prm-ub1-800.
      *                  *---------------------------------------------*
      *                  * Se ubicazione non trovata                   *
      *                  *---------------------------------------------*
           move      "Ubicazione non trovata!                        
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-prm-ub1-100.
       acc-prm-ub1-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-prm-ub1-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-prm-ub1-100.
       acc-prm-ub1-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Parametro 1               *
      *    *-----------------------------------------------------------*
       vis-prm-ub1-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-prm-ubi (1, 1) to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-prm-ub1-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Parametro 2                  *
      *    *-----------------------------------------------------------*
       acc-prm-ub2-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-tip-ubi-pdu (1)
                                          not  > 1
                     go to acc-prm-ub2-999.
       acc-prm-ub2-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-zub-ope      .
           move      w-tes-cod-dpz        to   w-cod-des-zub-dpz      .
           move      w-tes-prm-ubi (1, 2) to   w-cod-des-zub-cod      .
           move      11                   to   w-cod-des-zub-lin      .
           move      30                   to   w-cod-des-zub-pos      .
           move      zero                 to   w-cod-des-zub-dln      .
           move      zero                 to   w-cod-des-zub-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-des-zub-cll-000  thru cod-des-zub-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-zub-foi-000  thru cod-des-zub-foi-999    .
       acc-prm-ub2-110.
           perform   cod-des-zub-cll-000  thru cod-des-zub-cll-999    .
           if        w-cod-des-zub-ope    =    "F+"
                     go to acc-prm-ub2-115.
           if        w-cod-des-zub-ope    =    "AC"
                     go to acc-prm-ub2-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-prm-ub2-115.
           perform   cod-des-zub-foi-000  thru cod-des-zub-foi-999    .
           go to     acc-prm-ub2-110.
       acc-prm-ub2-120.
           move      w-cod-des-zub-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-prm-ub2-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-prm-ub2-999.
       acc-prm-ub2-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-prm-ubi (1, 2)   .
       acc-prm-ub2-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      w-tes-prm-ubi (1, 2) to   w-all-str-alf          .
           move      07                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-prm-ub2-100.
       acc-prm-ub2-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se codice vuoto                        *
      *                  *---------------------------------------------*
           if        w-tes-prm-ubi (1, 2) not  = spaces
                     go to acc-prm-ub2-700.
      *                  *---------------------------------------------*
      *                  * Eventuale compattamento ubicazioni          *
      *                  *---------------------------------------------*
           move      w-tes-prm-ubi (1, 3) to   w-tes-prm-ubi (1, 2)   .
           move      w-tes-prm-ubi (1, 4) to   w-tes-prm-ubi (1, 3)   .
           move      spaces               to   w-tes-prm-ubi (1, 4)   .
           perform   vis-prm-ub2-000      thru vis-prm-ub2-999        .
           perform   vis-prm-ub3-000      thru vis-prm-ub3-999        .
           perform   vis-prm-ub4-000      thru vis-prm-ub4-999        .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     acc-prm-ub2-800.
       acc-prm-ub2-700.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zub]                      *
      *                  *---------------------------------------------*
           move      w-tes-cod-dpz        to   w-let-arc-zub-dpz      .
           move      w-tes-prm-ubi (1, 2) to   w-let-arc-zub-cod      .
           perform   let-arc-zub-000      thru let-arc-zub-999        .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        w-let-arc-zub-flg    =    spaces
                     go to acc-prm-ub2-800.
      *                  *---------------------------------------------*
      *                  * Se ubicazione non trovata                   *
      *                  *---------------------------------------------*
           move      "Ubicazione non trovata!                        
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-prm-ub2-100.
       acc-prm-ub2-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-prm-ub2-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-prm-ub2-100.
       acc-prm-ub2-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Parametro 2               *
      *    *-----------------------------------------------------------*
       vis-prm-ub2-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-prm-ubi (1, 2) to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-prm-ub2-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Parametro 3                  *
      *    *-----------------------------------------------------------*
       acc-prm-ub3-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-tip-ubi-pdu (1)
                                          not  > 2
                     go to acc-prm-ub3-999.
       acc-prm-ub3-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-zub-ope      .
           move      w-tes-cod-dpz        to   w-cod-des-zub-dpz      .
           move      w-tes-prm-ubi (1, 3) to   w-cod-des-zub-cod      .
           move      12                   to   w-cod-des-zub-lin      .
           move      30                   to   w-cod-des-zub-pos      .
           move      zero                 to   w-cod-des-zub-dln      .
           move      zero                 to   w-cod-des-zub-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-des-zub-cll-000  thru cod-des-zub-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-zub-foi-000  thru cod-des-zub-foi-999    .
       acc-prm-ub3-110.
           perform   cod-des-zub-cll-000  thru cod-des-zub-cll-999    .
           if        w-cod-des-zub-ope    =    "F+"
                     go to acc-prm-ub3-115.
           if        w-cod-des-zub-ope    =    "AC"
                     go to acc-prm-ub3-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-prm-ub3-115.
           perform   cod-des-zub-foi-000  thru cod-des-zub-foi-999    .
           go to     acc-prm-ub3-110.
       acc-prm-ub3-120.
           move      w-cod-des-zub-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-prm-ub3-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-prm-ub3-999.
       acc-prm-ub3-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-prm-ubi (1, 3)   .
       acc-prm-ub3-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      w-tes-prm-ubi (1, 3) to   w-all-str-alf          .
           move      07                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-prm-ub3-100.
       acc-prm-ub3-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se codice vuoto                        *
      *                  *---------------------------------------------*
           if        w-tes-prm-ubi (1, 3) not  = spaces
                     go to acc-prm-ub3-700.
      *                  *---------------------------------------------*
      *                  * Eventuale compattamento ubicazioni          *
      *                  *---------------------------------------------*
           move      w-tes-prm-ubi (1, 4) to   w-tes-prm-ubi (1, 3)   .
           move      spaces               to   w-tes-prm-ubi (1, 4)   .
           perform   vis-prm-ub3-000      thru vis-prm-ub3-999        .
           perform   vis-prm-ub4-000      thru vis-prm-ub4-999        .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     acc-prm-ub3-800.
       acc-prm-ub3-700.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zub]                      *
      *                  *---------------------------------------------*
           move      w-tes-cod-dpz        to   w-let-arc-zub-dpz      .
           move      w-tes-prm-ubi (1, 3) to   w-let-arc-zub-cod      .
           perform   let-arc-zub-000      thru let-arc-zub-999        .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        w-let-arc-zub-flg    =    spaces
                     go to acc-prm-ub3-800.
      *                  *---------------------------------------------*
      *                  * Se ubicazione non trovata                   *
      *                  *---------------------------------------------*
           move      "Ubicazione non trovata!                        
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-prm-ub3-100.
       acc-prm-ub3-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-prm-ub3-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-prm-ub3-100.
       acc-prm-ub3-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Parametro 3               *
      *    *-----------------------------------------------------------*
       vis-prm-ub3-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-prm-ubi (1, 3) to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-prm-ub3-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Parametro 4                  *
      *    *-----------------------------------------------------------*
       acc-prm-ub4-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-tip-ubi-pdu (1)
                                          not  > 3
                     go to acc-prm-ub4-999.
       acc-prm-ub4-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-zub-ope      .
           move      w-tes-cod-dpz        to   w-cod-des-zub-dpz      .
           move      w-tes-prm-ubi (1, 4) to   w-cod-des-zub-cod      .
           move      13                   to   w-cod-des-zub-lin      .
           move      30                   to   w-cod-des-zub-pos      .
           move      zero                 to   w-cod-des-zub-dln      .
           move      zero                 to   w-cod-des-zub-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-des-zub-cll-000  thru cod-des-zub-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-zub-foi-000  thru cod-des-zub-foi-999    .
       acc-prm-ub4-110.
           perform   cod-des-zub-cll-000  thru cod-des-zub-cll-999    .
           if        w-cod-des-zub-ope    =    "F+"
                     go to acc-prm-ub4-115.
           if        w-cod-des-zub-ope    =    "AC"
                     go to acc-prm-ub4-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-prm-ub4-115.
           perform   cod-des-zub-foi-000  thru cod-des-zub-foi-999    .
           go to     acc-prm-ub4-110.
       acc-prm-ub4-120.
           move      w-cod-des-zub-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-prm-ub4-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-prm-ub4-999.
       acc-prm-ub4-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-prm-ubi (1, 4)   .
       acc-prm-ub4-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      w-tes-prm-ubi (1, 4) to   w-all-str-alf          .
           move      07                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-prm-ub4-100.
       acc-prm-ub4-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se codice vuoto                        *
      *                  *---------------------------------------------*
           if        w-tes-prm-ubi (1, 4) not  = spaces
                     go to acc-prm-ub4-700.
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     acc-prm-ub4-800.
       acc-prm-ub4-700.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zub]                      *
      *                  *---------------------------------------------*
           move      w-tes-cod-dpz        to   w-let-arc-zub-dpz      .
           move      w-tes-prm-ubi (1, 4) to   w-let-arc-zub-cod      .
           perform   let-arc-zub-000      thru let-arc-zub-999        .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        w-let-arc-zub-flg    =    spaces
                     go to acc-prm-ub4-800.
      *                  *---------------------------------------------*
      *                  * Se ubicazione non trovata                   *
      *                  *---------------------------------------------*
           move      "Ubicazione non trovata!                        
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-prm-ub4-100.
       acc-prm-ub4-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-prm-ub4-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-prm-ub4-100.
       acc-prm-ub4-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Parametro 4               *
      *    *-----------------------------------------------------------*
       vis-prm-ub4-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-prm-ubi (1, 4) to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-prm-ub4-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Indice di percorso                *
      *    *-----------------------------------------------------------*
       vis-inx-per-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-inx-per (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-inx-per-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Note per l'ubicazione                *
      *    *-----------------------------------------------------------*
       acc-not-ubi-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-not-ubi-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-not-ubi (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-not-ubi-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-not-ubi-999.
       acc-not-ubi-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-not-ubi (1)      .
       acc-not-ubi-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-not-ubi-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-not-ubi-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-not-ubi-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-not-ubi-100.
       acc-not-ubi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Note per l'ubicazione     *
      *    *-----------------------------------------------------------*
       vis-not-ubi-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-not-ubi (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-not-ubi-999.
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
           if        w-tes-num-mag        =    zero
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
       cnt-tdo-nok-200.
      *              *-------------------------------------------------*
      *              * Controllo su tipo ubicazione                    *
      *              *-------------------------------------------------*
           if        w-tes-tip-ubi (1)    not  = spaces
                     go to cnt-tdo-nok-300.
           move      "Manca il tipo ubicazione !                        
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-300.
      *              *-------------------------------------------------*
      *              * Controllo su parametri di ubicazione            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Controlli non piu' effettuati               *
      *                  *---------------------------------------------*
           go to     cnt-tdo-nok-500.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del numero parametri *
      *                  *---------------------------------------------*
           if        w-tes-tip-ubi-pdu (1)
                                          =    01
                     go to cnt-tdo-nok-310
           else if   w-tes-tip-ubi-pdu (1)
                                          =    02
                     go to cnt-tdo-nok-320
           else if   w-tes-tip-ubi-pdu (1)
                                          =    03
                     go to cnt-tdo-nok-330
           else      go to cnt-tdo-nok-340.
       cnt-tdo-nok-310.
           if        w-tes-prm-ubi (1, 1) not  = spaces
                     go to cnt-tdo-nok-500.
           move      "Manca il parametro 1 !                            
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-320.
           if        w-tes-prm-ubi (1, 1) not  = spaces and
                     w-tes-prm-ubi (1, 2) not  = spaces
                     go to cnt-tdo-nok-500.
           move      "Uno o piu' parametri mancanti!                    
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-330.
           if        w-tes-prm-ubi (1, 1) not  = spaces and
                     w-tes-prm-ubi (1, 2) not  = spaces and
                     w-tes-prm-ubi (1, 3) not  = spaces
                     go to cnt-tdo-nok-500.
           move      "Uno o piu' parametri mancanti!                    
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-340.
           if        w-tes-prm-ubi (1, 1) not  = spaces and
                     w-tes-prm-ubi (1, 2) not  = spaces and
                     w-tes-prm-ubi (1, 3) not  = spaces and
                     w-tes-prm-ubi (1, 4) not  = spaces
                     go to cnt-tdo-nok-500.
           move      "Uno o piu' parametri mancanti!                    
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-500.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del numero parametri *
      *                  *---------------------------------------------*
           if        w-tes-tip-ubi-pdu (1)
                                          =    01
                     go to cnt-tdo-nok-510
           else if   w-tes-tip-ubi-pdu (1)
                                          =    02
                     go to cnt-tdo-nok-520
           else if   w-tes-tip-ubi-pdu (1)
                                          =    03
                     go to cnt-tdo-nok-530
           else      go to cnt-tdo-nok-800.
       cnt-tdo-nok-510.
           move      spaces               to   w-tes-prm-ubi (1, 2)   .
       cnt-tdo-nok-520.
           move      spaces               to   w-tes-prm-ubi (1, 3)   .
       cnt-tdo-nok-530.
           move      spaces               to   w-tes-prm-ubi (1, 4)   .
       cnt-tdo-nok-800.
      *              *-------------------------------------------------*
      *              * Se controlli tutti superati                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cnt-tdo-nok-999.
       cnt-tdo-nok-900.
      *              *-------------------------------------------------*
      *              * Uscita con errore                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Segnale di errore in uscita                 *
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
           move      zero                 to   w-tes-cod-dpz          .
           move      zero                 to   w-tes-tip-mag          .
           move      zero                 to   w-tes-num-mag          .
           move      spaces               to   w-tes-num-mag-alf      .
           move      spaces               to   w-tes-num-mag-des      .
           move      spaces               to   w-tes-var-mag          .
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *-----------------------------------------------------------*
       nor-nok-tes-000.
           move      spaces               to   w-tes-tip-ubi (1)      .
           move      spaces               to   w-tes-tip-ubi-des (1)  .
           move      zero                 to   w-tes-tip-ubi-pdu (1)  .
           move      spaces               to   w-tes-tip-ubi-dpu
                                              (1, 1)                  .
           move      spaces               to   w-tes-tip-ubi-dpu
                                              (1, 2)                  .
           move      spaces               to   w-tes-tip-ubi-dpu
                                              (1, 3)                  .
           move      spaces               to   w-tes-tip-ubi-dpu
                                              (1, 4)                  .
           move      spaces               to   w-tes-prm-ubi (1, 1)   .
           move      spaces               to   w-tes-prm-ubi (1, 2)   .
           move      spaces               to   w-tes-prm-ubi (1, 3)   .
           move      spaces               to   w-tes-prm-ubi (1, 4)   .
           move      spaces               to   w-tes-not-ubi (1)      .
           move      zero                 to   w-tes-inx-per (1)      .
           move      spaces               to   w-tes-alx-exp (1)      .
       nor-nok-tes-999.
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
           move      "MAGUBI    "         to   f-key                  .
           move      w-tes-cod-dpz        to   rf-mau-cod-dpz         .
           move      w-tes-tip-mag        to   rf-mau-tip-mag         .
           move      w-tes-num-mag        to   rf-mau-num-mag         .
           move      w-tes-var-mag        to   rf-mau-var-mag         .
           move      "pgm/mag/fls/ioc/obj/iofmau"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mau                 .
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
      *                          *-------------------------------------*
      *                          * Valori contenuti direttamente in    *
      *                          * record [mau]                        *
      *                          *-------------------------------------*
           move      rf-mau-tip-ubi       to   w-tes-tip-ubi (1)      .
           move      rf-mau-prm-ubi (1)   to   w-tes-prm-ubi (1, 1)   .
           move      rf-mau-prm-ubi (2)   to   w-tes-prm-ubi (1, 2)   .
           move      rf-mau-prm-ubi (3)   to   w-tes-prm-ubi (1, 3)   .
           move      rf-mau-prm-ubi (4)   to   w-tes-prm-ubi (1, 4)   .
           move      rf-mau-not-ubi       to   w-tes-not-ubi (1)      .
           move      rf-mau-inx-per       to   w-tes-inx-per (1)      .
      *
           if        rf-mau-inx-per       not  numeric
                     move  zero           to   w-tes-inx-per (1)      .
      *
           move      rf-mau-alx-exp       to   w-tes-alx-exp (1)      .
      *                          *-------------------------------------*
      *                          * Valori contenuti indirettamente in  *
      *                          * record [mau]                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura archivio [zub]          *
      *                              *---------------------------------*
           move      w-tes-cod-dpz        to   w-let-arc-zub-dpz      .
           move      w-tes-prm-ubi (1, 1) to   w-let-arc-zub-cod      .
           perform   let-arc-zub-000      thru let-arc-zub-999        .
           move      w-let-arc-zub-inx    to   w-tes-inx-per (1)      .
      *                              *---------------------------------*
      *                              * Lettura archivio [zmu]          *
      *                              *---------------------------------*
           move      w-tes-cod-dpz        to   w-let-arc-zmu-dpz      .
           move      w-tes-tip-ubi (1)    to   w-let-arc-zmu-cod      .
           perform   let-arc-zmu-000      thru let-arc-zmu-999        .
      *                              *---------------------------------*
      *                              * Memorizzazione valori           *
      *                              *---------------------------------*
           move      w-let-arc-zmu-des    to   w-tes-tip-ubi-des (1)  .
           move      w-let-arc-zmu-pdu    to   w-tes-tip-ubi-pdu (1)  .
           move      w-let-arc-zmu-dpu (1)
                                          to   w-tes-tip-ubi-dpu
                                              (1, 1)                  .
           move      w-let-arc-zmu-dpu (2)
                                          to   w-tes-tip-ubi-dpu
                                              (1, 2)                  .
           move      w-let-arc-zmu-dpu (3)
                                          to   w-tes-tip-ubi-dpu
                                              (1, 3)                  .
           move      w-let-arc-zmu-dpu (4)
                                          to   w-tes-tip-ubi-dpu
                                              (1, 4)                  .
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
      *              *-------------------------------------------------*
      *              * Parametro di ubicazione 1                       *
      *              *-------------------------------------------------*
           perform   pmt-prm-ub1-000      thru pmt-prm-ub1-999        .
      *              *-------------------------------------------------*
      *              * Parametro di ubicazione 2                       *
      *              *-------------------------------------------------*
           perform   pmt-prm-ub2-000      thru pmt-prm-ub2-999        .
      *              *-------------------------------------------------*
      *              * Parametro di ubicazione 3                       *
      *              *-------------------------------------------------*
           perform   pmt-prm-ub3-000      thru pmt-prm-ub3-999        .
      *              *-------------------------------------------------*
      *              * Parametro di ubicazione 4                       *
      *              *-------------------------------------------------*
           perform   pmt-prm-ub4-000      thru pmt-prm-ub4-999        .
      *              *-------------------------------------------------*
      *              * Indice di percorso                              *
      *              *-------------------------------------------------*
           perform   pmt-inx-per-000      thru pmt-inx-per-999        .
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
      *              * Trattamento file [mau]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-500.
      *                      *-----------------------------------------*
      *                      * Write record [mau]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-mau-000      thru wrt-rec-mau-999        .
           go to     scr-mov-fil-999.
       scr-mov-fil-500.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rewrite record [mau]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-mau-000      thru rew-rec-mau-999        .
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *-----------------------------------------------------------*
       del-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Delete record [mau]                             *
      *              *-------------------------------------------------*
           perform   del-rec-mau-000      thru del-rec-mau-999        .
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [mau]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-mau-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmau"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mau                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-tes-cod-dpz        to   rf-mau-cod-dpz         .
           move      w-tes-tip-mag        to   rf-mau-tip-mag         .
           move      w-tes-num-mag        to   rf-mau-num-mag         .
           move      w-tes-var-mag        to   rf-mau-var-mag         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      w-tes-tip-ubi (1)    to   rf-mau-tip-ubi         .
           move      w-tes-prm-ubi (1, 1) to   rf-mau-prm-ubi (1)     .
           move      w-tes-prm-ubi (1, 2) to   rf-mau-prm-ubi (2)     .
           move      w-tes-prm-ubi (1, 3) to   rf-mau-prm-ubi (3)     .
           move      w-tes-prm-ubi (1, 4) to   rf-mau-prm-ubi (4)     .
           move      w-tes-not-ubi (1)    to   rf-mau-not-ubi         .
           move      w-tes-inx-per (1)    to   rf-mau-inx-per         .
           move      w-tes-alx-exp (1)    to   rf-mau-alx-exp         .
       cmp-rec-mau-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [mau]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-mau-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-mau-000      thru cmp-rec-mau-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmau"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mau                 .
       wrt-rec-mau-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [mau]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-mau-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-mau-000      thru cmp-rec-mau-999        .
      *              *-------------------------------------------------*
      *              * Forced put record                               *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmau"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mau                 .
       rew-rec-mau-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [mau]                                *
      *    *-----------------------------------------------------------*
       del-rec-mau-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-mau-000      thru cmp-rec-mau-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmau"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mau                 .
       del-rec-mau-999.
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
      *                  * Tipo magazzino 'Semilavorato' :             *
      *                  * - Se gestione materie prime attiva lo si    *
      *                  *   inserisce nella lista, altrimenti no      *
      *                  *---------------------------------------------*
           if        w-prs-dps-snx        =    "S"
                     move  02             to   w-exp-tpm-amm-tpm (2)
           else      move  zero           to   w-exp-tpm-amm-tpm (2)  .
      *                  *---------------------------------------------*
      *                  * Tipo magazzino 'Materia Prima' :            *
      *                  * - Se gestione materie prime attiva lo si    *
      *                  *   inserisce nella lista, altrimenti no      *
      *                  *---------------------------------------------*
           if        w-prs-dpm-snx        =    "S"
                     move  03             to   w-exp-tpm-amm-tpm (3)
           else      move  zero           to   w-exp-tpm-amm-tpm (3)  .
      *                  *---------------------------------------------*
      *                  * Tipo magazzino 'Materiale Vario' :          *
      *                  * - Se gestione materiale vario attiva lo si  *
      *                  *   inserisce nella lista, altrimenti no      *
      *                  *---------------------------------------------*
           if        w-prs-mtv-snx        =    "S"
                     move  04             to   w-exp-tpm-amm-tpm (4)
           else      move  zero           to   w-exp-tpm-amm-tpm (4)  .
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
                     move  w-exp-tpm-amm-tpm (4)
                                          to   w-exp-tpm-amm-tpm (3)
                     move  zero           to   w-exp-tpm-amm-tpm (4)  .
           if        w-exp-tpm-amm-tpm (2)
                                          =    zero
                     move  w-exp-tpm-amm-tpm (3)
                                          to   w-exp-tpm-amm-tpm (2)
                     move  w-exp-tpm-amm-tpm (4)
                                          to   w-exp-tpm-amm-tpm (3)
                     move  zero           to   w-exp-tpm-amm-tpm (4)  .
           if        w-exp-tpm-amm-tpm (3)
                                          =    zero
                     move  w-exp-tpm-amm-tpm (4)
                                          to   w-exp-tpm-amm-tpm (3)
                     move  zero           to   w-exp-tpm-amm-tpm (4)  .
      *              *-------------------------------------------------*
      *              * Preparazione del numero di elementi in tabella  *
      *              * tipi magazzino ammessi                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-exp-tpm-amm-num      .
       det-tpm-amm-020.
           add       1                    to   w-exp-tpm-amm-num      .
           if        w-exp-tpm-amm-num    >    4
                     move  4              to   w-exp-tpm-amm-num
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
           if        w-exp-tpm-amm-c02    >    4
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
                     move  "M"            to   w-let-arc-dcp-tpr
           else if   rf-dcp-tip-pro       =    02
                     move  "S"            to   w-let-arc-dcp-tpr
           else if   rf-dcp-tip-pro       =    03
                     move  "I"            to   w-let-arc-dcp-tpr
           else if   rf-dcp-tip-pro       =    09
                     move  "X"            to   w-let-arc-dcp-tpr
           else      move  spaces         to   w-let-arc-dcp-tpr      .
           move      rf-dcp-alf-pro       to   w-let-arc-dcp-alf      .
           move      rf-dcp-des-pro       to   w-let-arc-dcp-des      .
           move      rf-dcp-umi-ven       to   w-let-arc-dcp-umi      .
           move      rf-dcp-dec-qta       to   w-let-arc-dcp-deq      .
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
           move      spaces               to   w-let-arc-dcp-tpr      .
           move      spaces               to   w-let-arc-dcp-umi      .
           move      zero                 to   w-let-arc-dcp-deq      .
       let-arc-dcp-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [dps]                         *
      *    *-----------------------------------------------------------*
       let-arc-dps-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dps-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-dps-num    =    zero  
                     go to let-arc-dps-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMSEM"             to   f-key                  .
           move      w-let-arc-dps-num    to   rf-dps-num-sem         .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-dps-400.
       let-arc-dps-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-dps-alf-sem       to   w-let-arc-dps-alf      .
           move      rf-dps-des-sem       to   w-let-arc-dps-des      .
           move      rf-dps-umi-prd       to   w-let-arc-dps-umi      .
           move      rf-dps-dec-qta       to   w-let-arc-dps-deq      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dps-999.
       let-arc-dps-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dps-flg      .
           move      all   "."            to   w-let-arc-dps-des      .
           go to     let-arc-dps-600.
       let-arc-dps-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dps-des      .
       let-arc-dps-600.
           move      spaces               to   w-let-arc-dps-alf      .
           move      spaces               to   w-let-arc-dps-umi      .
           move      zero                 to   w-let-arc-dps-deq      .
       let-arc-dps-999.
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
           move      zero                 to   w-let-arc-mtv-deq      .
       let-arc-mtv-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zmu]                         *
      *    *-----------------------------------------------------------*
       let-arc-zmu-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zmu-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice tipo ubicazione a spaces         *
      *              *-------------------------------------------------*
           if        w-let-arc-zmu-cod    =    spaces
                     go to let-arc-zmu-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "TIPUBI    "         to   f-key                  .
           move      w-let-arc-zmu-dpz    to   rf-zmu-cod-dpz         .
           move      w-let-arc-zmu-cod    to   rf-zmu-tip-ubi         .
           move      "pgm/mag/fls/ioc/obj/iofzmu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zmu                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zmu-400.
       let-arc-zmu-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zmu-des-ubi       to   w-let-arc-zmu-des      .
           move      rf-zmu-num-pdu       to   w-let-arc-zmu-pdu      .
           move      rf-zmu-des-pdu (1)   to   w-let-arc-zmu-dpu (1)  .
           move      rf-zmu-des-pdu (2)   to   w-let-arc-zmu-dpu (2)  .
           move      rf-zmu-des-pdu (3)   to   w-let-arc-zmu-dpu (3)  .
           move      rf-zmu-des-pdu (4)   to   w-let-arc-zmu-dpu (4)  .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zmu-999.
       let-arc-zmu-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zmu-flg      .
           move      all   "."            to   w-let-arc-zmu-des      .
           go to     let-arc-zmu-600.
       let-arc-zmu-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zmu-des      .
       let-arc-zmu-600.
           move      zero                 to   w-let-arc-zmu-pdu      .
           move      spaces               to   w-let-arc-zmu-dpu (1)  .
           move      spaces               to   w-let-arc-zmu-dpu (2)  .
           move      spaces               to   w-let-arc-zmu-dpu (3)  .
           move      spaces               to   w-let-arc-zmu-dpu (4)  .
       let-arc-zmu-999.
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
           move      rf-zub-inx-per       to   w-let-arc-zub-inx      .
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
           move      zero                 to   w-let-arc-zub-inx      .
       let-arc-zub-999.
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
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

      *    *===========================================================*
      *    * Subroutines per accettazione codice prodotto 'dcp'        *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acoddcp0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per accettazione codice semilavorato          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dps/prg/cpy/acoddps0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per accettazione codice materia prima         *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/acoddpm0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per accettazione codice materia varia         *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acodmtv0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice tipo ubicazione *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/acdezmu0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice ubicazione      *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/acdezub0.acs"                   .
