       Identification Division.
       Program-Id.                                 pmag620c           .
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Esecuzione overlay per programma pmag6200 : *
      *                                                                *
      *                    Rilevazione giacenza fisica semilavorati    *
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
                     "inv"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "mag620"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pmag6200"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     " REGISTRAZIONE RILEVAZIONI INVENTARIALI "       .

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
               10  w-cnt-slc-rap-a01 redefines
                   w-cnt-slc-rap-alf.
                   15  w-cnt-slc-rap-chn
                               occurs 05  pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Rappresentazione numerica del numero riga         *
      *            *---------------------------------------------------*
               10  w-cnt-slc-rap-num      pic  9(05)                  .
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
               10  w-cnt-wrk-ctr-003      pic  9(05)                  .
               10  w-cnt-wrk-ctr-008      pic  9(05)                  .
               10  w-cnt-wrk-ctr-009      pic  9(05)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [mim]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmim"                          .
      *        *-------------------------------------------------------*
      *        * [zos]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfzos"                          .
      *        *-------------------------------------------------------*
      *        * [dps]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dps/fls/rec/rfdps"                          .
      *        *-------------------------------------------------------*
      *        * [zmm]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfzmm"                          .
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [fnt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rffnt"                          .
      *        *-------------------------------------------------------*
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .
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
               10  w-tes-dpz-inu          pic  9(02)                  .
               10  w-tes-fso-dps          pic  9(08)                  .
               10  w-tes-fso-dps-alf redefines
                   w-tes-fso-dps          pic  x(08)                  .
               10  w-tes-fso-dps-des      pic  x(40)                  .
               10  w-tes-fso-dps-ord      pic  9(02)                  .
               10  w-tes-cnf-imp          pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per bufferizzazione riga corpo                  *
      *    *-----------------------------------------------------------*
       01  w-rig.
           05  w-rig-num-prg              pic  9(05)                  .
           05  w-rig-num-mag              pic  9(07)                  .
           05  w-rig-var-mag              pic  x(14)                  .
           05  w-rig-alf-mag              pic  x(14)                  .
           05  w-rig-des-mag              pic  x(40)                  .
           05  w-rig-uni-mis              pic  x(03)                  .
           05  w-rig-dec-qta              pic  9(01)                  .
           05  w-rig-cod-mic              pic  x(03)                  .
           05  w-rig-cod-mic-des          pic  x(20)                  .
           05  w-rig-tip-arc              pic  x(01)                  .
           05  w-rig-cod-arc              pic  9(07)                  .
           05  w-rig-cod-arc-des          pic  x(40)                  .
           05  w-rig-dpz-arc              pic  x(04)                  .
           05  w-rig-qta-prs              pic s9(08)v9(03)            .
           05  w-rig-flg-ril              pic  x(01)                  .
           05  w-rig-qta-rlv              pic s9(08)v9(03)            .

      *    *===========================================================*
      *    * Area di comunicazione per gestione file relative di sup-  *
      *    * porto, con buffer in grado di ospitare l'area w-rig.      *
      *    *-----------------------------------------------------------*
       01  w-rlt-sup.
           05  w-rlt-sup-ope              pic  x(02)                  .
           05  w-rlt-sup-exs              pic  x(01)                  .
           05  w-rlt-sup-prg              pic  9(05)                  .
           05  w-rlt-sup-max              pic  9(05)                  .
           05  w-rlt-sup-ctr              pic  9(05)                  .
           05  w-rlt-sup-app              pic  x(01)                  .
           05  w-rlt-sup-new              pic  x(01)                  .
           05  w-rlt-sup-lst              pic  x(01)                  .
           05  w-rlt-sup-buf.
               10  filler occurs 512      pic  x(01)                  .

      *    *===========================================================*
      *    * Work per linea corpo a video                              *
      *    *-----------------------------------------------------------*
       01  w-lin.
      *        *-------------------------------------------------------*
      *        * Numero righe di corpo effettive visibili contempora-  *
      *        * neamente in una pagina di corpo nell'area di scroll   *
      *        *-------------------------------------------------------*
           05  w-lin-num-lin-vis          pic  9(02)       value 10   .
      *        *-------------------------------------------------------*
      *        * Numero linee di display impegnate per ogni riga corpo *
      *        * nell'area di scroll                                   *
      *        *-------------------------------------------------------*
           05  w-lin-num-lin-prc          pic  9(02)       value 01   .
      *        *-------------------------------------------------------*
      *        * Prima linea di display utilizzata per le righe corpo  *
      *        * nell'area di scroll                                   *
      *        *-------------------------------------------------------*
           05  w-lin-pri-lin-vid          pic  9(02)       value 06   .
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
                   15  w-lin-imm-num-lin  pic  x(05)                  .
      *                *-----------------------------------------------*
      *                * Altri dati visualizzati                       *
      *                *-----------------------------------------------*
                   15  filler             pic  x(01)                  .
                   15  w-lin-imm-001.
                       20  w-lin-imm-des-000.
                           25  w-lin-imm-des-mag
                                          pic  x(40)                  .
                       20  w-lin-imm-des-001   redefines
                           w-lin-imm-des-000.
                           25  w-lin-imm-d25-p01
                                          pic  x(25)                  .
                           25  filler     pic  x(01)                  .
                           25  w-lin-imm-cod-p01
                                          pic  x(14)                  .
                       20  w-lin-imm-des-002 redefines
                           w-lin-imm-des-000.
                           25  w-lin-imm-d21-p02
                                          pic  x(21)                  .
                           25  filler     pic  x(01)                  .
                           25  w-lin-imm-pqs-p02
                                          pic  x(01)                  .
                           25  w-lin-imm-tco-p02
                                          pic  x(01)                  .
                           25  w-lin-imm-pqd-p02
                                          pic  x(01)                  .
                           25  filler     pic  x(01)                  .
                           25  w-lin-imm-cod-p02
                                          pic  x(14)                  .
                       20  w-lin-imm-des-003 redefines
                           w-lin-imm-des-000.
                           25  w-lin-imm-cod-p03
                                          pic  x(14)                  .
                           25  filler     pic  x(01)                  .
                           25  w-lin-imm-d25-p03
                                          pic  x(25)                  .
                       20  w-lin-imm-des-004 redefines
                           w-lin-imm-des-000.
                           25  w-lin-imm-pqs-p04
                                          pic  x(01)                  .
                           25  w-lin-imm-tco-p04
                                          pic  x(01)                  .
                           25  w-lin-imm-pqd-p04
                                          pic  x(01)                  .
                           25  filler     pic  x(01)                  .
                           25  w-lin-imm-cod-p04
                                          pic  x(14)                  .
                           25  filler     pic  x(01)                  .
                           25  w-lin-imm-d21-p04
                                          pic  x(21)                  .
                       20  filler         pic  x(01)                  .
                       20  w-lin-imm-uni-mis
                                          pic  x(03)                  .
                   15  w-lin-imm-002      redefines
                       w-lin-imm-001.
                       20  w-lin-imm-tip-arc
                                          pic  x(03)                  .
                       20  filler         pic  x(01)                  .
                       20  w-lin-imm-des-arc
                                          pic  x(40)                  .
                   15  w-lin-imm-qta-prs  pic  x(15)                  .
                   15  w-lin-imm-qta-rlv  pic  x(15)                  .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Personalizzazioni per la riga di scroll               *
      *        *-------------------------------------------------------*
           05  w-prs-rig-scr.
      *            *---------------------------------------------------*
      *            * Esposizione della descrizione                     *
      *            *                                                   *
      *            *   - 00 : Descrizione completa                     *
      *            *                                                   *
      *            *   - 01 : Primi 25 caratteri della descrizione     *
      *            *          Codice                                   *
      *            *                                                   *
      *            *   - 02 : Primi 21 caratteri della descrizione     *
      *            *          Tipo codice                              *
      *            *          Codice                                   *
      *            *                                                   *
      *            *   - 03 : Codice                                   *
      *            *          Primi 25 caratteri della descrizione     *
      *            *                                                   *
      *            *   - 04 : Tipo codice                              *
      *            *          Codice                                   *
      *            *          Primi 21 caratteri della descrizione     *
      *            *                                                   *
      *            *   - 05 : Codice                                   *
      *            *                                                   *
      *            *   - 06 : Tipo codice                              *
      *            *          Codice                                   *
      *            *                                                   *
      *            *---------------------------------------------------*
               10  w-prs-rig-scr-des      pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
           05  w-sav-num-mag              pic  9(07)                  .
           05  w-sav-alf-mag              pic  x(14)                  .
           05  w-sav-var-mag              pic  x(14)                  .
           05  w-sav-cod-mic              pic  x(03)                  .
           05  w-sav-tip-arc              pic  x(01)                  .
           05  w-sav-cod-arc              pic  9(07)                  .
           05  w-sav-dpz-arc              pic  x(04)                  .
           05  w-sav-rig-max              pic  9(05)                  .
           05  w-sav-rlt-sup.
               10  filler   occurs 1024   pic  x(01)                  .
           05  w-sav-rig.
               10  filler   occurs 1024   pic  x(01)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo archivio                              *
      *        *-------------------------------------------------------*
           05  w-exp-tip-arc.
               10  w-exp-tip-arc-num      pic  9(02)       value 2    .
               10  w-exp-tip-arc-lun      pic  9(02)       value 10   .
               10  w-exp-tip-arc-tbl.
                   15  filler             pic  x(10) value
                            "Clienti   "                              .
                   15  filler             pic  x(10) value
                            "Fornitori "                              .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
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
      *        * Work per Let su archivio [zmm]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zmm.
               10  w-let-arc-zmm-flg      pic  x(01)                  .
               10  w-let-arc-zmm-cod      pic  x(03)                  .
               10  w-let-arc-zmm-des      pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [cli]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-cli.
               10  w-let-arc-cli-flg      pic  x(01)                  .
               10  w-let-arc-cli-cod      pic  9(07)                  .
               10  w-let-arc-cli-rag      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [fnt]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-fnt.
               10  w-let-arc-fnt-flg      pic  x(01)                  .
               10  w-let-arc-fnt-cod      pic  9(07)                  .
               10  w-let-arc-fnt-rag      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcc] per la dipendenza      *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcc.
               10  w-let-arc-dcc-flg      pic  x(01)                  .
               10  w-let-arc-dcc-cod      pic  9(07)                  .
               10  w-let-arc-dcc-dpz      pic  x(04)                  .
               10  w-let-arc-dcc-rag      pic  x(40)                  .
               10  w-let-arc-dcc-via      pic  x(40)                  .
               10  w-let-arc-dcc-loc      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcf] per la dipendenza      *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcf.
               10  w-let-arc-dcf-flg      pic  x(01)                  .
               10  w-let-arc-dcf-cod      pic  9(07)                  .
               10  w-let-arc-dcf-dpz      pic  x(04)                  .
               10  w-let-arc-dcf-rag      pic  x(40)                  .
               10  w-let-arc-dcf-via      pic  x(40)                  .
               10  w-let-arc-dcf-loc      pic  x(40)                  .

      *    *===========================================================*
      *    * Work per Let su archivio [zos] per [dps]                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dps/prg/cpy/lzosdps0.ltw"                   .
              
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
      *        * Work per Det se presenti dipendenze per il fornitore  *
      *        * commerciale                                           *
      *        *-------------------------------------------------------*
           05  w-det-snd-dcf.
      *            *---------------------------------------------------*
      *            * Esito della determinazione                        *
      *            * - S : Si, il fornitore commerciale ha dipendenze  *
      *            * - N : No, il fornitore commerciale non ha dipen-  *
      *            *       denze                                       *
      *            *---------------------------------------------------*
               10  w-det-snd-dcf-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Codice fornitore commerciale                      *
      *            *---------------------------------------------------*
               10  w-det-snd-dcf-fnt      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Contatore dipendenze rilevate                     *
      *            *---------------------------------------------------*
               10  w-det-snd-dcf-ctr      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Codice dipendenza, solo se unica per il fornitore *
      *            *---------------------------------------------------*
               10  w-det-snd-dcf-dpz      pic  x(04)                  .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per emissione box di errore                      *
      *        *-------------------------------------------------------*
           05  w-box-msg-err.
               10  w-box-msg-err-msg      pic  x(56)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice filtro selezione e or-  *
      *    * dinamento per file [dps]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dps/prg/cpy/azosdps0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice semilavorato            *
      *    *-----------------------------------------------------------*
           copy      "pgm/dps/prg/cpy/acoddps0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice conto merce             *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/acdezmm0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice cliente commerciale     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmndcc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice dipendenza del cliente  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoddcc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice fornitore commerciale   *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmndcf0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice dipendenza fornitore    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acoddcf0.acl"                   .

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
       exe-acc-cmp-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione di inizio programma                         *
      *    *-----------------------------------------------------------*
       dic-ini-pgm-000.
       dic-ini-pgm-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione di fine programma                           *
      *    *-----------------------------------------------------------*
       dic-fin-pgm-000.
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
      *              * Erase delle linee impegnate                     *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Titolo sub-program                              *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo rilevazione           : Giacenza fisica semil
      -              "avorati                       "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Lineette a linea 07                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
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
      *              * Lettura variaabile di i.p.c. per codice dipen-  *
      *              * denza                                           *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "dpz-inu"            to   s-var                  .
           move      "="                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     
                     move  "#"            to   w-cnt-pre-exe-pgm
                     go to pre-exe-pgm-999.
      *              *-------------------------------------------------*
      *              * Bufferizzazione codice dipendenza in uso        *
      *              *-------------------------------------------------*
           move      s-num                to   w-tes-dpz-inu          .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Esposizione della riga in scroll            *
      *                  *---------------------------------------------*
           perform   prs-rig-scr-000      thru prs-rig-scr-999        .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice filtro ordina-  *
      *              * mento e selezione per file [dps]                *
      *              *-------------------------------------------------*
           perform   cod-zos-dps-opn-000  thru cod-zos-dps-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice prodotto 'dps'  *
      *              *-------------------------------------------------*
           perform   cod-cod-dps-opn-000  thru cod-cod-dps-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice conto merce     *
      *              *-------------------------------------------------*
           perform   cod-des-zmm-opn-000  thru cod-des-zmm-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice cliente commer- *
      *              * ciale                                           *
      *              *-------------------------------------------------*
           perform   cod-mne-dcc-opn-000  thru cod-mne-dcc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice fornitore com-  *
      *              * merciale                                        *
      *              *-------------------------------------------------*
           perform   cod-mne-dcf-opn-000  thru cod-mne-dcf-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice dipendenza del  *
      *              * cliente                                         *
      *              *-------------------------------------------------*
           perform   cod-cod-dcc-opn-000  thru cod-cod-dcc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice dipendenza del  *
      *              * fornitore                                       *
      *              *-------------------------------------------------*
           perform   cod-cod-dcf-opn-000  thru cod-cod-dcf-opn-999    .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura delle personalizzazioni relative al tipo di espo- *
      *    * sizione della riga di scroll                              *
      *    *-----------------------------------------------------------*
       prs-rig-scr-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/mag/inv/mag620[rig-scr]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Se personalizzazione esistente si spostano i    *
      *              * valori letti in area di destinazione, altri-    *
      *              * menti si normalizza l'area di destinazione      *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-rig-scr
           else      move  zero           to   w-prs-rig-scr-des      .
      *              *-------------------------------------------------*
      *              * Normalizzazione tipo esposizione descrizione    *
      *              *-------------------------------------------------*
           if        w-prs-rig-scr-des    not  = 00 and
                     w-prs-rig-scr-des    not  = 01 and
                     w-prs-rig-scr-des    not  = 02 and
                     w-prs-rig-scr-des    not  = 03 and
                     w-prs-rig-scr-des    not  = 04 and
                     w-prs-rig-scr-des    not  = 05 and
                     w-prs-rig-scr-des    not  = 06
                     move  00             to   w-prs-rig-scr-des      .
       prs-rig-scr-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice filtro ordina- *
      *              * mento e selezione per file [dps]                *
      *              *-------------------------------------------------*
           perform   cod-zos-dps-cls-000  thru cod-zos-dps-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice prodotto 'dps' *
      *              *-------------------------------------------------*
           perform   cod-cod-dps-cls-000  thru cod-cod-dps-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice conto merce    *
      *              *-------------------------------------------------*
           perform   cod-des-zmm-cls-000  thru cod-des-zmm-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice cliente com-   *
      *              * merciale                                        *
      *              *-------------------------------------------------*
           perform   cod-mne-dcc-cls-000  thru cod-mne-dcc-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice fornitore com- *
      *              * merciale                                        *
      *              *-------------------------------------------------*
           perform   cod-mne-dcf-cls-000  thru cod-mne-dcf-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice dipendenza del *
      *              * cliente                                         *
      *              *-------------------------------------------------*
           perform   cod-cod-dcc-cls-000  thru cod-cod-dcc-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice dipendenza del *
      *              * fornitore                                       *
      *              *-------------------------------------------------*
           perform   cod-cod-dcf-cls-000  thru cod-cod-dcf-cls-999    .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * File relative di supporto                       *
      *              *-------------------------------------------------*
           move      "OP"                 to   w-rlt-sup-ope          .
           perform   cll-rlt-sup-000      thru cll-rlt-sup-999        .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * File relative di supporto                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   w-rlt-sup-ope          .
           perform   cll-rlt-sup-000      thru cll-rlt-sup-999        .
      *                  *---------------------------------------------*
      *                  * Cancel                                      *
      *                  *---------------------------------------------*
           perform   cnc-rlt-sup-000      thru cnc-rlt-sup-999        .
       rou-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per gestione catena righe         *
      *    *-----------------------------------------------------------*
       cll-rlt-sup-000.
           call      "pgm/mag/prg/obj/pmag6202"
                                         using w-rlt-sup              .
       cll-rlt-sup-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione sottoprogramma per gestione catena righe    *
      *    *-----------------------------------------------------------*
       cnc-rlt-sup-000.
           cancel    "pgm/mag/prg/obj/pmag6202"                       .
       cnc-rlt-sup-999.
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
      *                  * Accettazione codice filtro di selezione ed  *
      *                  * ordinamento su file [dps]                   *
      *                  *---------------------------------------------*
           perform   acc-fso-dps-000      thru acc-fso-dps-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
       acc-key-reg-200.
      *                  *---------------------------------------------*
      *                  * Accettazione conferma impostazioni          *
      *                  *---------------------------------------------*
           perform   acc-cnf-imp-000      thru acc-cnf-imp-999        .
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
           move      08                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazioni prompts                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice filtro di selezione ed ordinamento   *
      *                  * per file [dps]                              *
      *                  *---------------------------------------------*
           perform   pmt-fso-dps-000      thru pmt-fso-dps-999        .
       pmt-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Codice filtro di selezione anagrafica prodotti   *
      *    *-----------------------------------------------------------*
       pmt-fso-dps-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice filtro di selezione :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "   anagrafica semilavorati  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-fso-dps-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice filtro di ordinamento e selezione   *
      *    *                per file [dps]                             *
      *    *-----------------------------------------------------------*
       acc-fso-dps-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-fso-dps-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-zos-dps-ope      .
           move      w-tes-fso-dps        to   w-cod-zos-dps-cod      .
           move      09                   to   w-cod-zos-dps-lin      .
           move      30                   to   w-cod-zos-dps-pos      .
           move      09                   to   w-cod-zos-dps-dln      .
           move      41                   to   w-cod-zos-dps-dps      .
           move      "<B"                 to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-zos-dps-cll-000  thru cod-zos-dps-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-zos-dps-foi-000  thru cod-zos-dps-foi-999    .
       acc-fso-dps-110.
           perform   cod-zos-dps-cll-000  thru cod-zos-dps-cll-999    .
           if        w-cod-zos-dps-ope    =    "F+"
                     go to acc-fso-dps-115.
           if        w-cod-zos-dps-ope    =    "AC"
                     go to acc-fso-dps-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-fso-dps-115.
           perform   cod-zos-dps-foi-000  thru cod-zos-dps-foi-999    .
           go to     acc-fso-dps-110.
       acc-fso-dps-120.
           move      w-cod-zos-dps-cod    to   v-num                  .
       acc-fso-dps-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-fso-dps-999.
       acc-fso-dps-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-fso-dps          .
       acc-fso-dps-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura codice filtro selezione e ordina-   *
      *                  * mento per file [dps]                        *
      *                  *---------------------------------------------*
           move      w-tes-fso-dps        to   w-let-fso-dps-cod      .
           perform   let-fso-dps-000      thru let-fso-dps-999        .
           move      w-let-fso-dps-des    to   w-tes-fso-dps-des      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione codice filtro di selezione  *
      *                  * per file [dps], descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-fso-dps-des-000  thru vis-fso-dps-des-999    .
      *                  *---------------------------------------------*
      *                  * Se codice filtro non esistente : a reimpo-  *
      *                  * stazione                                    *
      *                  *---------------------------------------------*
           if        w-let-fso-dps-flg    not  = spaces
                     go to acc-fso-dps-100.
       acc-fso-dps-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tipo ordinamento dal codice filtro  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiesta tipo ordinamento              *
      *                      *-----------------------------------------*
           move      "TO"                 to   f-ope                  .
           move      w-tes-fso-dps-alf    to   f-key                  .
           move      "pgm/dps/prg/obj/bzosdps0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
           if        f-sts                =    "01"
                     move  01             to   w-tes-fso-dps-ord
           else if   f-sts                =    "02"
                     move  02             to   w-tes-fso-dps-ord
           else if   f-sts                =    "03"
                     move  03             to   w-tes-fso-dps-ord
           else if   f-sts                =    "04"
                     move  04             to   w-tes-fso-dps-ord
           else      move  01             to   w-tes-fso-dps-ord      .
       acc-fso-dps-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-fso-dps-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-fso-dps-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-fso-dps-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-fso-dps-999.
       acc-fso-dps-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice del filtro di selezione per file *
      *    *                   [dps]                                   *
      *    *-----------------------------------------------------------*
       vis-fso-dps-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-fso-dps        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-fso-dps-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice del filtro di selezione per file *
      *    *                   [dps], descrizione                      *
      *    *-----------------------------------------------------------*
       vis-fso-dps-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-fso-dps-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-fso-dps-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Conferma impostazioni                *
      *    *-----------------------------------------------------------*
       acc-cnf-imp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Linea 23                                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      23                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Linea 24                                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      24                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Conferma impostazioni (S/N/E) ? "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione valore da impostare         *
      *                  *---------------------------------------------*
           move      spaces               to   w-tes-cnf-imp          .
       acc-cnf-imp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      24                   to   v-lin                  .
           move      33                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-tes-cnf-imp        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cnf-imp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-cnf-imp          .
       acc-cnf-imp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se impostata una function-key prevista : Ok *
      *                  *---------------------------------------------*
           if        v-key                =    "UP  " or
                     v-key                =    "DO  " or
                     v-key                =    "EXIT"
                     go to acc-cnf-imp-450.
      *                  *---------------------------------------------*
      *                  * Se impostato un carattere previsto : Ok     *
      *                  *---------------------------------------------*
           if        w-tes-cnf-imp        =    "S" or
                     w-tes-cnf-imp        =    "N" or
                     w-tes-cnf-imp        =    "E"
                     go to acc-cnf-imp-450.
      *                  *---------------------------------------------*
      *                  * Altrimenti : reimpostazione                 *
      *                  *---------------------------------------------*
           go to     acc-cnf-imp-100.
       acc-cnf-imp-450.
      *                  *---------------------------------------------*
      *                  * Cancellazione prompts                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Linea 23                                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      23                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Linea 24                                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      24                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cnf-imp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda della function-key o   *
      *                  * del carattere impostato                     *
      *                  *---------------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-cnf-imp-625
           else if   v-key                =    "DO  "
                     go to acc-cnf-imp-650
           else if   v-key                =    "EXIT"
                     go to acc-cnf-imp-675.
           if        w-tes-cnf-imp        =    "N"
                     go to acc-cnf-imp-625
           else if   w-tes-cnf-imp        =    "S"
                     go to acc-cnf-imp-650
           else if   w-tes-cnf-imp        =    "E"
                     go to acc-cnf-imp-675.
       acc-cnf-imp-625.
      *                  *---------------------------------------------*
      *                  * Se Up o 'N'                                 *
      *                  *---------------------------------------------*
           move      "UP  "               to   v-key                  .
           go to     acc-cnf-imp-999.
       acc-cnf-imp-650.
      *                  *---------------------------------------------*
      *                  * Se Do o 'S'                                 *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
           go to     acc-cnf-imp-999.
       acc-cnf-imp-675.
      *                  *---------------------------------------------*
      *                  * Se Exit o 'E'                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Status di uscita ad 'E'                 *
      *                      *-----------------------------------------*
           move      "E"                  to   w-cnt-tus-acc-key      .
           go to     acc-cnf-imp-999.
       acc-cnf-imp-999.
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
           move      "E"                  to   w-cnt-tus-acc-nok      .
           go to     acc-nok-reg-999.
       acc-nok-reg-870.
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Nr riga corpo da accettare : max + 1    *
      *                      *-----------------------------------------*
           move      w-rlt-sup-max        to   w-cnt-cor-nrg-dac      .
           add       1                    to   w-cnt-cor-nrg-dac      .
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
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Disabilitazione forzata del tasto Delt      *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-itd      .
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
       vis-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts testata                           *
      *    *-----------------------------------------------------------*
       pmt-tes-reg-000.
       pmt-tes-reg-999.
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
           if        w-cnt-cor-nrg-dac    not  > w-rlt-sup-max
                     go to acc-cor-reg-025.
      *              *-------------------------------------------------*
      *              * Se oltre numero riga massimo e Append non pos-  *
      *              * sibile si esce con status di uscita "+"         *
      *              *-------------------------------------------------*
           if        w-rlt-sup-app        not  = spaces
                     move  "+"            to   w-cnt-tus-acc-cor
                     go to acc-cor-reg-990.
      *              *-------------------------------------------------*
      *              * Se oltre numero riga massimo e Append possibile *
      *              * aggiornamento numero riga da accettare          *
      *              *-------------------------------------------------*
           move      w-rlt-sup-max        to   w-cnt-cor-nrg-dac      .
           add       1                    to   w-cnt-cor-nrg-dac      .
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
      *              * Lettura riga da file relative di supporto       *
      *              *-------------------------------------------------*
           move      "RD"                 to   w-rlt-sup-ope          .
           move      w-cnt-cor-nrg-dac    to   w-rlt-sup-prg          .
           perform   cll-rlt-sup-000      thru cll-rlt-sup-999        .
           move      w-rlt-sup-buf        to   w-rig                  .
      *              *-------------------------------------------------*
      *              * Se riga nuova si normalizza il corpo            *
      *              *-------------------------------------------------*
           if        w-rlt-sup-new        =    spaces
                     go to acc-cor-reg-050.
           perform   nor-nok-rig-000      thru nor-nok-rig-999        .
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
      *              *-------------------------------------------------*
           if        w-rlt-sup-max        =    zero
                     move  spaces         to   w-cnt-sts-imp-cor
           else      move  "#"            to   w-cnt-sts-imp-cor      .
           if        w-rlt-sup-max        =    1        and
                     w-rlt-sup-new        not  = spaces and
                     w-rlt-sup-lst        not  = spaces
                     move  spaces         to   w-cnt-sts-imp-cor      .
      *              *-------------------------------------------------*
      *              * Flag di status impostione riga attuale          *
      *              *-------------------------------------------------*
           if        w-rlt-sup-new        =    spaces
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
      *                  * Accettazione Codice prodotto                *
      *                  *---------------------------------------------*
           perform   acc-cod-mag-000      thru acc-cod-mag-999        .
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
      *                  * Accettazione Codice merce in conto          *
      *                  *---------------------------------------------*
           perform   acc-cod-mic-000      thru acc-cod-mic-999        .
           if        w-cnt-tus-acc-rig    not  = spaces
                     go to acc-cor-reg-900.
       acc-cor-reg-300.
      *                  *---------------------------------------------*
      *                  * Accettazione Tipo archivio                  *
      *                  *---------------------------------------------*
           perform   acc-tip-arc-000      thru acc-tip-arc-999        .
           if        w-cnt-tus-acc-rig    not  = spaces
                     go to acc-cor-reg-900.
           if        v-key                =    "UP  "
                     go to acc-cor-reg-200.
       acc-cor-reg-400.
      *                  *---------------------------------------------*
      *                  * Accettazione Codice archivio                *
      *                  *---------------------------------------------*
           perform   acc-cod-arc-000      thru acc-cod-arc-999        .
           if        w-cnt-tus-acc-rig    not  = spaces
                     go to acc-cor-reg-900.
           if        v-key                =    "UP  "
                     go to acc-cor-reg-300.
       acc-cor-reg-500.
      *                  *---------------------------------------------*
      *                  * Accettazione Codice dipendenza archivio     *
      *                  *---------------------------------------------*
           perform   acc-dpz-arc-000      thru acc-dpz-arc-999        .
           if        w-cnt-tus-acc-rig    not  = spaces
                     go to acc-cor-reg-900.
           if        v-key                =    "UP  "
                     go to acc-cor-reg-400.
       acc-cor-reg-600.
      *                  *---------------------------------------------*
      *                  * Accettazione Quantita' rilevata             *
      *                  *---------------------------------------------*
           perform   acc-qta-rlv-000      thru acc-qta-rlv-999        .
           if        w-cnt-tus-acc-rig    not  = spaces
                     go to acc-cor-reg-900.
           if        v-key                =    "UP  "
                     go to acc-cor-reg-500.
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
                     go to acc-cor-reg-905.
      *                  *---------------------------------------------*
      *                  * Se Exit da interno riga                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se riga New                             *
      *                      *-----------------------------------------*
           if        w-rlt-sup-new        =    spaces
                     go to acc-cor-reg-902.
      *                      *-----------------------------------------*
      *                      * Visualizzazione numero riga a spaces se *
      *                      * si e' in Append                         *
      *                      *-----------------------------------------*
           if        w-rlt-sup-new        not  = spaces and
                     w-rlt-sup-lst        not  = spaces
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
           if        w-rlt-sup-new        =    spaces or
                     w-rlt-sup-lst        =    spaces
                     move  zero           to   w-cnt-cor-nrp-vis      .
           go to     acc-cor-reg-000.
       acc-cor-reg-902.
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
      *                      * Routine post-accettazione riga          *
      *                      *-----------------------------------------*
           move      "+"                  to   w-cnt-cor-tip-agg      .
           perform   pos-rig-cor-000      thru pos-rig-cor-999        .
      *                      *-----------------------------------------*
      *                      * Se record di tipo New                   *
      *                      *-----------------------------------------*
           if        w-rlt-sup-new        =    spaces
                     go to acc-cor-reg-907.
      *                          *-------------------------------------*
      *                          * Attribuzione numero progressivo di  *
      *                          * riga                                *
      *                          *-------------------------------------*
           perform   prg-rig-cor-000      thru prg-rig-cor-999        .
      *                          *-------------------------------------*
      *                          * Put riga su file relative di sup-   *
      *                          * porto                               *
      *                          *-------------------------------------*
           move      "PT"                 to   w-rlt-sup-ope          .
           move      w-rig                to   w-rlt-sup-buf          .
           perform   cll-rlt-sup-000      thru cll-rlt-sup-999        .
           go to     acc-cor-reg-908.
       acc-cor-reg-907.
      *                      *-----------------------------------------*
      *                      * Se record non di tipo New               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Update riga su file relative di     *
      *                          * supporto                            *
      *                          *-------------------------------------*
           move      "UP"                 to   w-rlt-sup-ope          .
           move      w-cnt-cor-nrg-dac    to   w-rlt-sup-prg          .
           move      w-rig                to   w-rlt-sup-buf          .
           perform   cll-rlt-sup-000      thru cll-rlt-sup-999        .
       acc-cor-reg-908.
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
           if        w-rlt-sup-new        =    spaces
                     go to acc-cor-reg-936.
      *                          *-------------------------------------*
      *                          * Visualizzazione del numero riga a   *
      *                          * spaces se si e' in Append           *
      *                          *-------------------------------------*
           if        w-rlt-sup-new        not  = spaces and
                     w-rlt-sup-lst        not  = spaces
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
           if        w-rlt-sup-new        =    spaces or
                     w-rlt-sup-lst        =    spaces
                     move  zero           to   w-cnt-cor-nrp-vis      .
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
           if        w-rlt-sup-new        =    spaces
                     go to acc-cor-reg-941.
      *                          *-------------------------------------*
      *                          * Se si e' in Append si prepara il    *
      *                          * tipo uscita "+" e si visualizza il  *
      *                          * numero riga a spaces                *
      *                          *-------------------------------------*
           if        w-rlt-sup-new        not  = spaces and
                     w-rlt-sup-lst        not  = spaces
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
           if        w-rlt-sup-new        =    spaces
                     go to acc-cor-reg-946.
      *                          *-------------------------------------*
      *                          * Visualizzazione del numero riga a   *
      *                          * spaces                              *
      *                          *-------------------------------------*
           move      "S"                  to   w-cnt-cor-tvi-rig      .
           move      w-cnt-cor-nrg-dac    to   w-cnt-cor-nrg-dav      .
           perform   vis-lin-cor-000      thru vis-lin-cor-999        .
       acc-cor-reg-946.
      *                      *-----------------------------------------*
      *                      * Se record non New                       *
      *                      *-----------------------------------------*
           move      "S"                  to   w-cnt-tus-acc-cor      .
           go to     acc-cor-reg-990.
       acc-cor-reg-965.
      *                  *---------------------------------------------*
      *                  * Se Tab da primo campo di impostazione riga  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se record New                           *
      *                      *-----------------------------------------*
           if        w-rlt-sup-new        =    spaces
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
           if        w-rlt-sup-new        =    spaces or
                     w-rlt-sup-lst        =    spaces
                     move  zero           to   w-cnt-cor-nrp-vis      .
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
           move      w-rlt-sup-max        to   w-cnt-cor-nrg-dac      .
           if        w-rlt-sup-app        =    spaces
                     add   1              to   w-cnt-cor-nrg-dac      .
           go to     acc-cor-reg-000.
       acc-cor-reg-970.
      *                  *---------------------------------------------*
      *                  * Se Back da primo campo di impostazione riga *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se record New                           *
      *                      *-----------------------------------------*
           if        w-rlt-sup-new        =    spaces
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
           if        w-rlt-sup-new        =    spaces or
                     w-rlt-sup-lst        =    spaces
                     move  zero           to   w-cnt-cor-nrp-vis      .
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
           if        w-rlt-sup-new        =    spaces
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
           if        w-rlt-sup-new        =    spaces or
                     w-rlt-sup-lst        =    spaces
                     move  zero           to   w-cnt-cor-nrp-vis      .
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
           if        w-cnt-cor-nrg-dac    >    w-rlt-sup-max
                     move    "+"          to   w-cnt-tus-acc-cor
                     go to acc-cor-reg-990.
      *                          *-------------------------------------*
      *                          * Determinazione numero d'ordine del- *
      *                          * la prima riga appartenente alla pa- *
      *                          * gina (max + 1)                      *
      *                          *-------------------------------------*
           move      w-rlt-sup-max        to   w-cnt-wrk-ctr-001      .
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
           if        w-rlt-sup-max        =    w-cnt-wrk-ctr-001
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
           if        w-rlt-sup-new        =    spaces
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
           if        w-rlt-sup-new        =    spaces or
                     w-rlt-sup-lst        =    spaces
                     move  zero           to   w-cnt-cor-nrp-vis      .
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
           if        w-rlt-sup-new        =    spaces
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
           if        w-rlt-sup-new        =    spaces or
                     w-rlt-sup-lst        =    spaces
                     move  zero           to   w-cnt-cor-nrp-vis      .
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
      *              *-------------------------------------------------*
           if        w-rlt-sup-max        =    zero
                     move  spaces         to   w-cnt-sts-imp-cor
           else      move  "#"            to   w-cnt-sts-imp-cor      .
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
           move      06                   to   v-lin                  .
           move      15                   to   v-lto                  .
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
       vis-cor-reg-200.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se si e' entro il massimo  *
      *              * numero di records caricati oppure no            *
      *              *-------------------------------------------------*
           if        w-cnt-wrk-ctr-008    >    w-rlt-sup-max
                     go to vis-cor-reg-300
           else      go to vis-cor-reg-400.
       vis-cor-reg-300.
      *              *-------------------------------------------------*
      *              * Se si e' oltre il massimo numero di records ca- *
      *              * ricati                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione linea a Spaces              *
      *                  *---------------------------------------------*
           move      "B"                  to   w-cnt-cor-tvi-rig      .
           move      w-cnt-wrk-ctr-008    to   w-cnt-cor-nrg-dav      .
           perform   vis-lin-cor-000      thru vis-lin-cor-999        .
      *                  *---------------------------------------------*
      *                  * Continuazione                               *
      *                  *---------------------------------------------*
           go to     vis-cor-reg-500.
       vis-cor-reg-400.
      *              *-------------------------------------------------*
      *              * Se non si e' oltre il massimo numero di records *
      *              * caricati                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura file relative di supporto           *
      *                  *---------------------------------------------*
           move      "RD"                 to   w-rlt-sup-ope          .
           move      w-cnt-wrk-ctr-008    to   w-rlt-sup-prg          .
           perform   cll-rlt-sup-000      thru cll-rlt-sup-999        .
           move      w-rlt-sup-buf        to   w-rig                  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione riga                        *
      *                  *---------------------------------------------*
           move      "R"                  to   w-cnt-cor-tvi-rig      .
           move      w-cnt-wrk-ctr-008    to   w-cnt-cor-nrg-dav      .
           perform   vis-lin-cor-000      thru vis-lin-cor-999        .
       vis-cor-reg-500.
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
           else if   w-cnt-cor-tvi-rig    =    "B"
                     go to vis-lin-cor-150
           else if   w-cnt-cor-tvi-rig    =    "S"
                     go to vis-lin-cor-200
           else if   w-cnt-cor-tvi-rig    =    "N"
                     go to vis-lin-cor-300
           else      go to vis-lin-cor-400.
       vis-lin-cor-100.
      *              *-------------------------------------------------*
      *              * Se visualizzazione indicatore linea             *
      *              *-------------------------------------------------*
           move      "====>"              to   w-lin-imm-num-lin      .
           move      05                   to   v-car                  .
           go to     vis-lin-cor-900.
       vis-lin-cor-150.
      *              *-------------------------------------------------*
      *              * Se visualizzazione linea a blanks               *
      *              *-------------------------------------------------*
           move      80                   to   v-car                  .
           go to     vis-lin-cor-900.
       vis-lin-cor-200.
      *              *-------------------------------------------------*
      *              * Se visualizzazione numero linea a spaces        *
      *              *-------------------------------------------------*
           move      spaces               to   w-lin-imm-num-lin      .
           move      05                   to   v-car                  .
           go to     vis-lin-cor-900.
       vis-lin-cor-300.
      *                  *---------------------------------------------*
      *                  * Se visualizzazione numero d'ordine riga     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      spaces               to   v-edm                  .
           move      w-rlt-sup-prg        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-lin-imm-num-lin      .
           go to     vis-lin-cor-900.
       vis-lin-cor-400.
      *              *-------------------------------------------------*
      *              * Se visualizzazione riga intera                  *
      *              *-------------------------------------------------*
       vis-lin-cor-420.
      *                  *---------------------------------------------*
      *                  * Editing Numero d'ordine riga                *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      spaces               to   v-edm                  .
           move      w-rlt-sup-prg        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-lin-imm-num-lin      .
      *                  *---------------------------------------------*
      *                  * Se tipo visualizzazione 001                 *
      *                  *---------------------------------------------*
           if        w-rig-cod-mic        not  = spaces
                     go to vis-lin-cor-460.
      *                      *-----------------------------------------*
      *                      * Editing Descrizione prodotto            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione a seconda della persona- *
      *                          * lizzazione del tipo di esposizione  *
      *                          * riga di scroll                      *
      *                          *-------------------------------------*
           if        w-prs-rig-scr-des    =    00
                     go to vis-lin-cor-440
           else if   w-prs-rig-scr-des    =    01
                     go to vis-lin-cor-441
           else if   w-prs-rig-scr-des    =    02
                     go to vis-lin-cor-442
           else if   w-prs-rig-scr-des    =    03
                     go to vis-lin-cor-443
           else if   w-prs-rig-scr-des    =    04
                     go to vis-lin-cor-444
           else if   w-prs-rig-scr-des    =    05
                     go to vis-lin-cor-445
           else if   w-prs-rig-scr-des    =    06
                     go to vis-lin-cor-446
           else      go to vis-lin-cor-440.
       vis-lin-cor-440.
      *                          *-------------------------------------*
      *                          * Se personalizzazione tipo : 00      *
      *                          *-------------------------------------*
           move      w-rig-des-mag        to   w-lin-imm-des-mag      .
           go to     vis-lin-cor-450.
       vis-lin-cor-441.
      *                          *-------------------------------------*
      *                          * Se personalizzazione tipo : 01      *
      *                          *-------------------------------------*
           move      w-rig-des-mag        to   w-lin-imm-d25-p01      .
           move      w-rig-alf-mag        to   w-lin-imm-cod-p01      .
           go to     vis-lin-cor-450.
       vis-lin-cor-442.
      *                          *-------------------------------------*
      *                          * Se personalizzazione tipo : 02      *
      *                          *-------------------------------------*
           move      w-rig-des-mag        to   w-lin-imm-d21-p02      .
           move      "["                  to   w-lin-imm-pqs-p02      .
           move      "S"                  to   w-lin-imm-tco-p02      .
           move      "]"                  to   w-lin-imm-pqd-p02      .
           move      w-rig-alf-mag        to   w-lin-imm-cod-p02      .
           go to     vis-lin-cor-450.
       vis-lin-cor-443.
      *                          *-------------------------------------*
      *                          * Se personalizzazione tipo : 03      *
      *                          *-------------------------------------*
           move      w-rig-alf-mag        to   w-lin-imm-cod-p03      .
           move      w-rig-des-mag        to   w-lin-imm-d25-p03      .
           go to     vis-lin-cor-450.
       vis-lin-cor-444.
      *                          *-------------------------------------*
      *                          * Se personalizzazione tipo : 04      *
      *                          *-------------------------------------*
           move      "["                  to   w-lin-imm-pqs-p04      .
           move      "S"                  to   w-lin-imm-tco-p04      .
           move      "]"                  to   w-lin-imm-pqd-p04      .
           move      w-rig-alf-mag        to   w-lin-imm-cod-p04      .
           move      w-rig-des-mag        to   w-lin-imm-d21-p04      .
           go to     vis-lin-cor-450.
       vis-lin-cor-445.
      *                          *-------------------------------------*
      *                          * Se personalizzazione tipo : 05      *
      *                          *-------------------------------------*
           move      w-rig-alf-mag        to   w-lin-imm-cod-p01      .
           go to     vis-lin-cor-450.
       vis-lin-cor-446.
      *                          *-------------------------------------*
      *                          * Se personalizzazione tipo : 06      *
      *                          *-------------------------------------*
           move      "["                  to   w-lin-imm-pqs-p02      .
           move      "S"                  to   w-lin-imm-tco-p02      .
           move      "]"                  to   w-lin-imm-pqd-p02      .
           move      w-rig-alf-mag        to   w-lin-imm-cod-p02      .
           go to     vis-lin-cor-450.
       vis-lin-cor-450.
      *                      *-----------------------------------------*
      *                      * Editing Unita' di misura                *
      *                      *-----------------------------------------*
           move      w-rig-uni-mis        to   w-lin-imm-uni-mis      .
           go to     vis-lin-cor-480.
       vis-lin-cor-460.
      *                  *---------------------------------------------*
      *                  * Se tipo visualizzazione 002                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing Tipo archivio                   *
      *                      *-----------------------------------------*
           if        w-rig-tip-arc        =    "C"
                     move  "[C]"          to   w-lin-imm-tip-arc
           else if   w-rig-tip-arc        =    "F"
                     move  "[F]"          to   w-lin-imm-tip-arc
           else      move  "[?]"          to   w-lin-imm-tip-arc      .
      *                      *-----------------------------------------*
      *                      * Editing Descrizione archivio            *
      *                      *-----------------------------------------*
           move      w-rig-cod-arc-des    to   w-lin-imm-des-arc      .
       vis-lin-cor-480.
      *                  *---------------------------------------------*
      *                  * Editing Quantita' presunta                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      w-rig-dec-qta        to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           if        w-rig-qta-prs        =    zero
                     move  "GD"           to   v-edm
           else      move  "G"            to   v-edm                  .
           move      w-rig-qta-prs        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-lin-imm-qta-prs      .
      *                  *---------------------------------------------*
      *                  * Editing Quantita' rilevata                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      w-rig-dec-qta        to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           if        w-rig-flg-ril        =    spaces
                     if    w-rig-qta-rlv  =    zero
                           move  "GBD"    to   v-edm
                     else  move  "GB"     to   v-edm
           else      if    w-rig-qta-rlv  =    zero
                           move  "GD"     to   v-edm
                     else  move  "G"      to   v-edm                  .
           move      w-rig-qta-rlv        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-lin-imm-qta-rlv      .
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
           move      16                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cor-reg-050.
      *              *-------------------------------------------------*
      *              * Linea di fincatura e sottolineatura             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda della personalizzazio- *
      *                  * di tipo esposizione della descrizione       *
      *                  *---------------------------------------------*
           if        w-prs-rig-scr-des    =    00
                     go to pmt-cor-reg-100
           else if   w-prs-rig-scr-des    =    01 or
                     w-prs-rig-scr-des    =    05
                     go to pmt-cor-reg-110
           else if   w-prs-rig-scr-des    =    02 or
                     w-prs-rig-scr-des    =    06
                     go to pmt-cor-reg-120
           else if   w-prs-rig-scr-des    =    03
                     go to pmt-cor-reg-130
           else if   w-prs-rig-scr-des    =    04
                     go to pmt-cor-reg-140
           else      go to pmt-cor-reg-100.
       pmt-cor-reg-100.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 00              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Fincatura                               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      " N.R.           Descrizione prodotto           Udm
      -              "     Presunto      Rilevato   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "----- ---------------------------------------- ---
      -              " -------------- --------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     pmt-cor-reg-200.
       pmt-cor-reg-110.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 01 o 05         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Fincatura                               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      " N.R.        Descrizione            Codice     Udm
      -              "     Presunto      Rilevato   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "----- ------------------------- -------------- ---
      -              " -------------- --------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     pmt-cor-reg-200.
       pmt-cor-reg-120.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 02 o 06         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Fincatura                               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      " N.R.      Descrizione            Codice       Udm
      -              "     Presunto      Rilevato   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "----- --------------------- ------------------ ---
      -              " -------------- --------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     pmt-cor-reg-200.
       pmt-cor-reg-130.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 03              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Fincatura                               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      " N.R.     Codice            Descrizione        Udm
      -              "     Presunto      Rilevato   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "----- -------------- ------------------------- ---
      -              " -------------- --------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     pmt-cor-reg-200.
       pmt-cor-reg-140.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 04              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Fincatura                               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      " N.R.       Codice            Descrizione      Udm
      -              "     Presunto      Rilevato   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "----- ------------------ --------------------- ---
      -              " -------------- --------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     pmt-cor-reg-200.
       pmt-cor-reg-200.
      *              *-------------------------------------------------*
      *              * Trattini a linea 16                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
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
           if        w-rlt-sup-new        not  = spaces
                     go to vis-rig-cor-999.
      *              *-------------------------------------------------*
      *              * Codice prodotto                                 *
      *              *-------------------------------------------------*
           perform   vis-cod-mag-000      thru vis-cod-mag-999        .
           perform   vis-cod-mag-des-000  thru vis-cod-mag-des-999    .
           perform   vis-cod-mag-umi-000  thru vis-cod-mag-umi-999    .
      *              *-------------------------------------------------*
      *              * Merce in conto                                  *
      *              *-------------------------------------------------*
           perform   vis-cod-mic-000      thru vis-cod-mic-999        .
           perform   vis-cod-mic-des-000  thru vis-cod-mic-des-999    .
      *              *-------------------------------------------------*
      *              * Tipo archivio                                   *
      *              *-------------------------------------------------*
           perform   vis-tip-arc-000      thru vis-tip-arc-999        .
      *              *-------------------------------------------------*
      *              * Codice archivio                                 *
      *              *-------------------------------------------------*
           perform   vis-cod-arc-000      thru vis-cod-arc-999        .
           perform   vis-cod-arc-des-000  thru vis-cod-arc-des-999    .
      *              *-------------------------------------------------*
      *              * Dipendenza archivio                             *
      *              *-------------------------------------------------*
           perform   vis-dpz-arc-000      thru vis-dpz-arc-999        .
      *              *-------------------------------------------------*
      *              * Quantita' presunta                              *
      *              *-------------------------------------------------*
           perform   vis-qta-prs-000      thru vis-qta-prs-999        .
      *              *-------------------------------------------------*
      *              * Quantita' rilevata                              *
      *              *-------------------------------------------------*
           perform   vis-qta-rlv-000      thru vis-qta-rlv-999        .
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
           move      17                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts per riga corpo espansa  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice prodotto                             *
      *                  *---------------------------------------------*
           perform   pmt-cod-mag-000      thru pmt-cod-mag-999        .
      *                  *---------------------------------------------*
      *                  * Merce in conto                              *
      *                  *---------------------------------------------*
           perform   pmt-cod-mic-000      thru pmt-cod-mic-999        .
      *                  *---------------------------------------------*
      *                  * Tipo archivio                               *
      *                  *---------------------------------------------*
           perform   pmt-tip-arc-000      thru pmt-tip-arc-999        .
      *                  *---------------------------------------------*
      *                  * Codice archivio                             *
      *                  *---------------------------------------------*
           perform   pmt-cod-arc-000      thru pmt-cod-arc-999        .
      *                  *---------------------------------------------*
      *                  * Dipendenza archivio                         *
      *                  *---------------------------------------------*
           perform   pmt-dpz-arc-000      thru pmt-dpz-arc-999        .
      *                  *---------------------------------------------*
      *                  * Quantita' presunta                          *
      *                  *---------------------------------------------*
           perform   pmt-qta-prs-000      thru pmt-qta-prs-999        .
      *                  *---------------------------------------------*
      *                  * Quantita' rilevata                          *
      *                  *---------------------------------------------*
           perform   pmt-qta-rlv-000      thru pmt-qta-rlv-999        .
       pmt-rig-cor-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice prodotto alfanumerico     *
      *    *-----------------------------------------------------------*
       pmt-cod-mag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice prodotto :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-mag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Merce in conto                   *
      *    *-----------------------------------------------------------*
       pmt-cod-mic-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Merce in c/     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-mic-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tipo archivio                    *
      *    *-----------------------------------------------------------*
       pmt-tip-arc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      54                   to   v-pos                  .
           move      "Tipo archivio :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-tip-arc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice archivio                  *
      *    *-----------------------------------------------------------*
       pmt-cod-arc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice archivio :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-arc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Dipendenza archivio              *
      *    *-----------------------------------------------------------*
       pmt-dpz-arc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      71                   to   v-pos                  .
           move      "Dip.:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-dpz-arc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Quantita' presunta               *
      *    *-----------------------------------------------------------*
       pmt-qta-prs-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Q.ta' presunta  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-qta-prs-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Quantita' rilevata               *
      *    *-----------------------------------------------------------*
       pmt-qta-rlv-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Q.ta' rilevata  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-qta-rlv-999.
           exit.

      *    *===========================================================*
      *    * Accettazione 1. campo riga corpo espansa : Codice prodot- *
      *    *                                            to alfanumeri- *
      *    *                                            co             *
      *    *-----------------------------------------------------------*
       acc-cod-mag-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-rig-alf-mag        to   w-sav-alf-mag          .
       acc-cod-mag-100.
      *              *-------------------------------------------------*
      *              * Ripristino valore precedente                    *
      *              *-------------------------------------------------*
           move      w-sav-alf-mag        to   w-rig-alf-mag          .
       acc-cod-mag-125.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Parametri generali di accettazione          *
      *                  *---------------------------------------------*
           move      "AC"                 to   w-cod-cod-dps-ope      .
           move      "L"                  to   w-cod-cod-dps-tac      .
           move      zero                 to   w-cod-cod-dps-num      .
           move      w-rig-alf-mag        to   w-cod-cod-dps-alf      .
           move      17                   to   w-cod-cod-dps-lin      .
           move      19                   to   w-cod-cod-dps-pos      .
           move      17                   to   w-cod-cod-dps-dln      .
           move      35                   to   w-cod-cod-dps-dps      .
           move      spaces               to   v-edm                  .
      *                  *---------------------------------------------*
      *                  * Tasti funzione                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Up   : sempre ammesso, a meno che non   *
      *                      *        si sia alla riga numero 1        *
      *                      *-----------------------------------------*
           if        w-cnt-cor-nrg-dac    >    1
                     move  "UP  "         to   v-pfk (01)
           else      move  spaces         to   v-pfk (01)             .
      *                      *-----------------------------------------*
      *                      * Down : sempre ammesso                   *
      *                      *-----------------------------------------*
           move      "DOWN"               to   v-pfk (02)             .
      *                      *-----------------------------------------*
      *                      * Find : sempre ammesso                   *
      *                      *-----------------------------------------*
           move      "FIND"               to   v-pfk (03)             .
      *                      *-----------------------------------------*
      *                      * Insr : mai ammesso                      *
      *                      *-----------------------------------------*
           move      spaces               to   v-pfk (04)             .
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
                     move  "DO  "         to   v-pfk (05)
           else      move  spaces         to   v-pfk (05)             .
      *                      *-----------------------------------------*
      *                      * Remv : mai ammesso                      *
      *                      *-----------------------------------------*
           move      spaces               to   v-pfk (06)             .
      *                      *-----------------------------------------*
      *                      * Prsc : sempre ammesso, a meno che non   *
      *                      *        si sia alla pagina numero 1      *
      *                      *-----------------------------------------*
           if        w-cnt-cor-nrg-dac    >    w-lin-num-lin-vis
                     move  "PRSC"         to   v-pfk (07)
           else      move  spaces         to   v-pfk (07)             .
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
                     move  "BACK"         to   v-pfk (09)
           else      move  spaces         to   v-pfk (09)             .
      *                      *-----------------------------------------*
      *                      * Tab  : sempre ammesso, a meno che non   *
      *                      *        si sia gia' sull'ultima riga e   *
      *                      *        che questa sia in Append         *
      *                      *-----------------------------------------*
           if        w-rlt-sup-new        =    spaces or
                     w-rlt-sup-lst        =    spaces
                     move  "TAB "         to   v-pfk (10)             .
      *                      *-----------------------------------------*
      *                      * Slct : sempre ammesso, a meno che non   *
      *                      *        si sia gia' sull'unica riga      *
      *                      *-----------------------------------------*
           if        w-rlt-sup-max        not  = 1
                     move  "SLCT"         to   v-pfk (11)             .
       acc-cod-mag-150.
      *                  *---------------------------------------------*
      *                  * Esecuzione accettazione                     *
      *                  *---------------------------------------------*
           perform   cod-cod-dps-cll-000  thru cod-cod-dps-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-dps-foi-000  thru cod-cod-dps-foi-999    .
       acc-cod-mag-152.
           perform   cod-cod-dps-cll-000  thru cod-cod-dps-cll-999    .
           if        w-cod-cod-dps-ope    =    "F+"
                     go to acc-cod-mag-154.
           if        w-cod-cod-dps-ope    =    "AC"
                     go to acc-cod-mag-156.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-mag-154.
           perform   cod-cod-dps-foi-000  thru cod-cod-dps-foi-999    .
           go to     acc-cod-mag-152.
       acc-cod-mag-156.
           move      w-cod-cod-dps-alf    to   v-alf                  .
       acc-cod-mag-200.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-rig
                     go to acc-cod-mag-999.
       acc-cod-mag-225.
      *              *-------------------------------------------------*
      *              * Test se Return                                  *
      *              *-------------------------------------------------*
           if        v-key                =    spaces
                     go to acc-cod-mag-400.
       acc-cod-mag-250.
      *              *-------------------------------------------------*
      *              * Se Slct                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to acc-cod-mag-300.
       acc-cod-mag-255.
      *                  *---------------------------------------------*
      *                  * Conversione numero riga impostato in forma- *
      *                  * to numerico                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Valore impostato in comodo per tratta-  *
      *                      * mento                                   *
      *                      *-----------------------------------------*
           move      v-alf                to   w-cnt-slc-rap-alf      .
      *                      *-----------------------------------------*
      *                      * Se nel movimento e' avvenuto un tronca- *
      *                      * mento : a reimpostazione                *
      *                      *-----------------------------------------*
           if        w-cnt-slc-rap-alf    not  = v-alf
                     go to acc-cod-mag-100.
       acc-cod-mag-260.
      *                      *-----------------------------------------*
      *                      * Conversione vera e propria; se errori : *
      *                      * a reimpostazione                        *
      *                      *-----------------------------------------*
           move      zero                 to   w-cnt-slc-rap-num      .
           move      zero                 to   w-cnt-wrk-ctr-001      .
       acc-cod-mag-262.
           add       1                    to   w-cnt-wrk-ctr-001      .
           if        w-cnt-wrk-ctr-001    >    5
                     go to acc-cod-mag-270.
           if        w-cnt-slc-rap-chr
                    (w-cnt-wrk-ctr-001)   =    spaces
                     go to acc-cod-mag-264.
           if       (w-cnt-wrk-ctr-001)   <    "0"   or
                     w-cnt-slc-rap-chr
                    (w-cnt-wrk-ctr-001)   >    "9"
                     go to acc-cod-mag-100.
           multiply  10                   by   w-cnt-slc-rap-num      .
           add       w-cnt-slc-rap-chn
                    (w-cnt-wrk-ctr-001)   to   w-cnt-slc-rap-num      .
           go to     acc-cod-mag-262.
       acc-cod-mag-264.
           add       1                    to   w-cnt-wrk-ctr-001      .
           if        w-cnt-wrk-ctr-001    >    5
                     go to acc-cod-mag-270.
           if        w-cnt-slc-rap-chr
                    (w-cnt-wrk-ctr-001)   =    spaces
                     go to acc-cod-mag-264
           else      go to acc-cod-mag-100.
       acc-cod-mag-270.
      *                      *-----------------------------------------*
      *                      * Se conversione senza errori             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se numero riga minore di 1 oppure   *
      *                          * maggiore del massimo : reimposta-   *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-cnt-slc-rap-num    =    zero       or
                     w-cnt-slc-rap-num    >    w-rlt-sup-max
                     go to acc-cod-mag-100.
      *                          *-------------------------------------*
      *                          * Se numero riga impostato pari a nu- *
      *                          * numero riga attuale : reimpostazio- *
      *                          * ne                                  *
      *                          *-------------------------------------*
           if        w-cnt-slc-rap-num    =    w-cnt-cor-nrg-dac
                     go to acc-cod-mag-100.
      *                          *-------------------------------------*
      *                          * Preparazione parametri e uscita     *
      *                          *-------------------------------------*
           move      w-cnt-slc-rap-num    to   w-cnt-slc-num-rig      .
           move      "."                  to   w-cnt-tus-acc-rig      .
           go to     acc-cod-mag-999.
       acc-cod-mag-300.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-rig
                     go to acc-cod-mag-999.
      *              *-------------------------------------------------*
      *              * Se premuto un altro tasto funzione non deve es- *
      *              * sere avvenuta variazione del campo di imposta-  *
      *              * zione                                           *
      *              *-------------------------------------------------*
           if        v-mod                not  = spaces
                     go to acc-cod-mag-100.
      *              *-------------------------------------------------*
      *              * Se Up                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "UP  "
                     move  "U"            to   w-cnt-tus-acc-rig
                     go to acc-cod-mag-999.
      *              *-------------------------------------------------*
      *              * Se Down                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DOWN"
                     move  "D"            to   w-cnt-tus-acc-rig
                     go to acc-cod-mag-999.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-rig
                             go to acc-cod-mag-999
                     else    go to acc-cod-mag-100.
      *              *-------------------------------------------------*
      *              * Se Tab                                          *
      *              *-------------------------------------------------*
           if        v-key                =    "TAB "
                     move  "T"            to   w-cnt-tus-acc-rig
                     go to acc-cod-mag-999.
      *              *-------------------------------------------------*
      *              * Se Back                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "BACK"
                     move  "B"            to   w-cnt-tus-acc-rig
                     go to acc-cod-mag-999.
      *              *-------------------------------------------------*
      *              * Se Nxsc                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "NXSC"
                     move  "N"            to   w-cnt-tus-acc-rig
                     go to acc-cod-mag-999.
      *              *-------------------------------------------------*
      *              * Se Prsc                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "PRSC"
                     move  "P"            to   w-cnt-tus-acc-rig
                     go to acc-cod-mag-999.
       acc-cod-mag-400.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-rig-alf-mag          .
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se impostazione a vuoto                     *
      *                  *    - se in Append : come Down purche' non   *
      *                  *                     ci siano zero righe     *
      *                  *    - altrimenti   : reimpostazione          *
      *                  *---------------------------------------------*
           if        w-rig-alf-mag        =    spaces
                     if    w-rlt-sup-new  not  = spaces and
                           w-rlt-sup-lst  not  = spaces and
                           w-cnt-sts-imp-cor
                                          not  = spaces
                           move  "D"      to   w-cnt-tus-acc-rig
                           go to acc-cod-mag-999
                     else  go to acc-cod-mag-100.
      *                  *---------------------------------------------*
      *                  * Se record non New non si ammette la modifi- *
      *                  * ca del valore                               *
      *                  *---------------------------------------------*
           if        w-rlt-sup-new        =    spaces and
                     w-rig-alf-mag        not  = w-sav-alf-mag
                     go to acc-cod-mag-100.
      *                  *---------------------------------------------*
      *                  * Se record non New : oltre                   *
      *                  *---------------------------------------------*
           if        w-rlt-sup-new        =    spaces
                     go to acc-cod-mag-600.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [dps]                      *
      *                  *---------------------------------------------*
           move      w-rig-alf-mag        to   w-let-arc-dps-alf      .
           perform   let-arc-dps-000      thru let-arc-dps-999        .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori record               *
      *                  *---------------------------------------------*
           move      w-let-arc-dps-num    to   w-rig-num-mag          .
           move      w-let-arc-dps-des    to   w-rig-des-mag          .
           move      w-let-arc-dps-umi    to   w-rig-uni-mis          .
           move      w-let-arc-dps-deq    to   w-rig-dec-qta          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione valori associati            *
      *                  *---------------------------------------------*
           perform   vis-cod-mag-des-000  thru vis-cod-mag-des-999    .
           perform   vis-cod-mag-umi-000  thru vis-cod-mag-umi-999    .
      *                  *---------------------------------------------*
      *                  * Se record non trovato : reimpostazione      *
      *                  *---------------------------------------------*
           if        w-let-arc-dps-flg    not  = spaces
                     go to acc-cod-mag-100.
       acc-cod-mag-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-mag-800.
       acc-cod-mag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice prodotto alfanumerico            *
      *    *-----------------------------------------------------------*
       vis-cod-mag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      19                   to   v-pos                  .
           move      w-rig-alf-mag        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-mag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice prodotto, descrizione            *
      *    *-----------------------------------------------------------*
       vis-cod-mag-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-rig-des-mag        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-mag-des-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice prodotto, unita' di misura       *
      *    *-----------------------------------------------------------*
       vis-cod-mag-umi-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      w-rig-uni-mis        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-mag-umi-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Merce in conto                             *
      *    *-----------------------------------------------------------*
       acc-cod-mic-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Solo se riga 'new' e in 'append'        *
      *                      *-----------------------------------------*
           if        w-rlt-sup-new        =    spaces or
                     w-rlt-sup-lst        =    spaces
                     go to acc-cod-mic-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-rig-cod-mic        to   w-sav-cod-mic          .
       acc-cod-mic-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-zmm-ope      .
           move      w-rig-cod-mic        to   w-cod-des-zmm-cod      .
           move      18                   to   w-cod-des-zmm-lin      .
           move      19                   to   w-cod-des-zmm-pos      .
           move      18                   to   w-cod-des-zmm-dln      .
           move      23                   to   w-cod-des-zmm-dps      .
           move      spaces               to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           if        w-cnt-sts-imp-rig    not  = spaces
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
                     move  "E"            to   w-cnt-tus-acc-rig
                     go to acc-cod-mic-999.
      *              *-------------------------------------------------*
      *              * Valore impostato                                *
      *              *-------------------------------------------------*
           move      v-alf                to   w-rig-cod-mic          .
       acc-cod-mic-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella [zmm]                       *
      *                  *---------------------------------------------*
           move      w-rig-cod-mic        to   w-let-arc-zmm-cod      .
           perform   let-arc-zmm-000      thru let-arc-zmm-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zmm-des    to   w-rig-cod-mic-des      .
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
      *                  * Se valore a spazi : reimpostazione          *
      *                  *---------------------------------------------*
           if        w-rig-cod-mic        =    spaces
                     go to acc-cod-mic-100.
       acc-cod-mic-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-mic-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-rig-000
                                          thru cnt-tdo-rig-999
                     if      w-cnt-tdo-rig-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-rig
                             go to acc-cod-mic-999
                     else    move  spaces to   w-cnt-tdo-rig-flg
                             go to acc-cod-mic-100.
       acc-cod-mic-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice merce in conto                   *
      *    *-----------------------------------------------------------*
       vis-cod-mic-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      19                   to   v-pos                  .
           move      w-rig-cod-mic        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-mic-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Descrizione per codice merce in conto   *
      *    *-----------------------------------------------------------*
       vis-cod-mic-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-rig-cod-mic-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-mic-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Tipo archivio                              *
      *    *-----------------------------------------------------------*
       acc-tip-arc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Solo se riga 'new' e in 'append'        *
      *                      *-----------------------------------------*
           if        w-rlt-sup-new        =    spaces or
                     w-rlt-sup-lst        =    spaces
                     go to acc-tip-arc-999.
      *                      *-----------------------------------------*
      *                      * Solo se conto merce diverso da spaces   *
      *                      *-----------------------------------------*
           if        w-rig-cod-mic        =    spaces
                     go to acc-tip-arc-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-rig-tip-arc        to   w-sav-tip-arc          .
       acc-tip-arc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-arc-lun    to   v-car                  .
           move      w-exp-tip-arc-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      18                   to   v-lin                  .
           move      70                   to   v-pos                  .
           move      w-exp-tip-arc-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-rig    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           if        w-rig-tip-arc        =    "C"
                     move  01             to   v-num
           else if   w-rig-tip-arc        =    "F"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-rig
                     go to acc-tip-arc-999.
       acc-tip-arc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "C"            to   w-rig-tip-arc
           else if   v-num                =    02
                     move  "F"            to   w-rig-tip-arc
           else      move  spaces         to   w-rig-tip-arc          .
       acc-tip-arc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se spaces : reimpostazione                  *
      *                  *---------------------------------------------*
           if        w-rig-tip-arc        =    spaces
                     go to acc-tip-arc-100.
       acc-tip-arc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore attuale come precedente : oltre   *
      *                  *---------------------------------------------*
           if        w-rig-tip-arc        =    w-sav-tip-arc
                     go to acc-tip-arc-800.
      *                  *---------------------------------------------*
      *                  * Trattamento codice, descrizione e dipenden- *
      *                  * za archivio                                 *
      *                  *---------------------------------------------*
           move      zero                 to   w-rig-cod-arc          .
           move      spaces               to   w-rig-cod-arc-des      .
           move      spaces               to   w-rig-dpz-arc          .
           perform   vis-cod-arc-000      thru vis-cod-arc-999        .
           perform   vis-cod-arc-des-000  thru vis-cod-arc-des-999    .
           perform   vis-dpz-arc-000      thru vis-dpz-arc-999        .
       acc-tip-arc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-rig-000
                                          thru cnt-tdo-rig-999
                     if      w-cnt-tdo-rig-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-rig
                             go to acc-tip-arc-999
                     else    move  spaces to   w-cnt-tdo-rig-flg
                             go to acc-tip-arc-100.
       acc-tip-arc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Tipo archivio                           *
      *    *-----------------------------------------------------------*
       vis-tip-arc-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-arc-lun    to   v-car                  .
           move      w-exp-tip-arc-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      18                   to   v-lin                  .
           move      70                   to   v-pos                  .
           move      w-exp-tip-arc-tbl    to   v-txt                  .
           if        w-rig-tip-arc        =    "C"
                     move  01             to   v-num
           else if   w-rig-tip-arc        =    "F"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-arc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice archivio                            *
      *    *-----------------------------------------------------------*
       acc-cod-arc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Solo se riga 'new' e in 'append'        *
      *                      *-----------------------------------------*
           if        w-rlt-sup-new        =    spaces or
                     w-rlt-sup-lst        =    spaces
                     go to acc-cod-arc-999.
      *                      *-----------------------------------------*
      *                      * Solo se conto merce diverso da spaces   *
      *                      *-----------------------------------------*
           if        w-rig-cod-mic        =    spaces
                     go to acc-cod-arc-999.
       acc-cod-arc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo archivio      *
      *                  *---------------------------------------------*
           if        w-rig-tip-arc        =    "C"
                     go to acc-cod-arc-200
           else if   w-rig-tip-arc        =    "F"
                     go to acc-cod-arc-400
           else      go to acc-cod-arc-999.
       acc-cod-arc-200.
      *                  *---------------------------------------------*
      *                  * Se tipo archivio : Cliente                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Accettazione codice Cliente             *
      *                      *-----------------------------------------*
           perform   acc-cod-cli-000      thru acc-cod-cli-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     acc-cod-arc-999.
       acc-cod-arc-400.
      *                  *---------------------------------------------*
      *                  * Se tipo archivio : Fornitore                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Accettazione codice Fornitore           *
      *                      *-----------------------------------------*
           perform   acc-cod-fnt-000      thru acc-cod-fnt-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     acc-cod-arc-999.
       acc-cod-arc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice archivio                         *
      *    *-----------------------------------------------------------*
       vis-cod-arc-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      19                   to   v-lin                  .
           move      19                   to   v-pos                  .
           move      "<B"                 to   v-edm                  .
           move      w-rig-cod-arc        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-arc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Descrizione archivio                    *
      *    *-----------------------------------------------------------*
       vis-cod-arc-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-rig-cod-arc-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-arc-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice cliente                             *
      *    *-----------------------------------------------------------*
       acc-cod-cli-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-rig-cod-arc        to   w-sav-cod-arc          .
       acc-cod-cli-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-dcc-ope      .
           move      w-rig-cod-arc        to   w-cod-mne-dcc-cod      .
           move      19                   to   w-cod-mne-dcc-lin      .
           move      19                   to   w-cod-mne-dcc-pos      .
           move      19                   to   w-cod-mne-dcc-rln      .
           move      30                   to   w-cod-mne-dcc-rps      .
           move      zero                 to   w-cod-mne-dcc-vln      .
           move      zero                 to   w-cod-mne-dcc-vps      .
           move      zero                 to   w-cod-mne-dcc-lln      .
           move      zero                 to   w-cod-mne-dcc-lps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           if        w-cnt-sts-imp-rig    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
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
                     move  "E"            to   w-cnt-tus-acc-rig
                     go to acc-cod-cli-999.
       acc-cod-cli-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-rig-cod-arc          .
       acc-cod-cli-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura file [cli]                          *
      *                  *---------------------------------------------*
           move      w-rig-cod-arc        to   w-let-arc-cli-cod      .
           perform   let-arc-cli-000      thru let-arc-cli-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione ragione sociale              *
      *                  *---------------------------------------------*
           move      w-let-arc-cli-rag    to   w-rig-cod-arc-des      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione ragione sociale             *
      *                  *---------------------------------------------*
           perform   vis-cod-arc-des-000  thru vis-cod-arc-des-999    .
      *                  *---------------------------------------------*
      *                  * Se cliente non esistente : reimpostazione   *
      *                  *---------------------------------------------*
           if        w-let-arc-cli-flg    not  = spaces
                     go to acc-cod-cli-100.
      *                  *---------------------------------------------*
      *                  * Se codice a zero : reimpostazione, a meno   *
      *                  * che non si sia in Up                        *
      *                  *---------------------------------------------*
           if        w-rig-cod-arc        not  = zero
                     go to acc-cod-cli-600.
           if        v-key                =    "UP  "
                     go to acc-cod-cli-600
           else      go to acc-cod-cli-100.
       acc-cod-cli-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore attuale pari al valore precedente *
      *                  * : nessuna dipendenza                        *
      *                  *---------------------------------------------*
           if        w-rig-cod-arc        =    w-sav-cod-arc
                     go to acc-cod-cli-800.
      *                  *---------------------------------------------*
      *                  * Normalizzazione valori legati al codice di- *
      *                  * pendenza del cliente                        *
      *                  *---------------------------------------------*
           move      spaces               to   w-rig-dpz-arc          .
           perform   vis-dpz-arc-000      thru vis-dpz-arc-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione se presenti dipendenze per   *
      *                  * il cliente commerciale                      *
      *                  *---------------------------------------------*
           move      w-rig-cod-arc        to   w-det-snd-dcc-cli      .
           perform   det-snd-dcc-000      thru det-snd-dcc-999        .
       acc-cod-cli-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-rig-000
                                          thru cnt-tdo-rig-999
                     if      w-cnt-tdo-rig-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-rig
                             go to acc-cod-cli-999
                     else    move  spaces to   w-cnt-tdo-rig-flg
                             go to acc-cod-cli-100.
       acc-cod-cli-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice fornitore                           *
      *    *-----------------------------------------------------------*
       acc-cod-fnt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-rig-cod-arc        to   w-sav-cod-arc          .
       acc-cod-fnt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-dcf-ope      .
           move      w-rig-cod-arc        to   w-cod-mne-dcf-cod      .
           move      19                   to   w-cod-mne-dcf-lin      .
           move      19                   to   w-cod-mne-dcf-pos      .
           move      19                   to   w-cod-mne-dcf-rln      .
           move      30                   to   w-cod-mne-dcf-rps      .
           move      zero                 to   w-cod-mne-dcf-vln      .
           move      zero                 to   w-cod-mne-dcf-vps      .
           move      zero                 to   w-cod-mne-dcf-lln      .
           move      zero                 to   w-cod-mne-dcf-lps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           if        w-cnt-sts-imp-rig    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           perform   cod-mne-dcf-cll-000  thru cod-mne-dcf-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-dcf-foi-000  thru cod-mne-dcf-foi-999    .
       acc-cod-fnt-110.
           perform   cod-mne-dcf-cll-000  thru cod-mne-dcf-cll-999    .
           if        w-cod-mne-dcf-ope    =    "F+"
                     go to acc-cod-fnt-115.
           if        w-cod-mne-dcf-ope    =    "AC"
                     go to acc-cod-fnt-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-fnt-115.
           perform   cod-mne-dcf-foi-000  thru cod-mne-dcf-foi-999    .
           go to     acc-cod-fnt-110.
       acc-cod-fnt-120.
           move      w-cod-mne-dcf-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-rig
                     go to acc-cod-fnt-999.
       acc-cod-fnt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-rig-cod-arc          .
       acc-cod-fnt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura file [fnt]                          *
      *                  *---------------------------------------------*
           move      w-rig-cod-arc        to   w-let-arc-fnt-cod      .
           perform   let-arc-fnt-000      thru let-arc-fnt-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione ragione sociale              *
      *                  *---------------------------------------------*
           move      w-let-arc-fnt-rag    to   w-rig-cod-arc-des      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione ragione sociale             *
      *                  *---------------------------------------------*
           perform   vis-cod-arc-des-000  thru vis-cod-arc-des-999    .
      *                  *---------------------------------------------*
      *                  * Se fntente non esistente : reimpostazione   *
      *                  *---------------------------------------------*
           if        w-let-arc-fnt-flg    not  = spaces
                     go to acc-cod-fnt-100.
      *                  *---------------------------------------------*
      *                  * Se codice a zero : reimpostazione, a meno   *
      *                  * che non si sia in Up                        *
      *                  *---------------------------------------------*
           if        w-rig-cod-arc        not  = zero
                     go to acc-cod-fnt-600.
           if        v-key                =    "UP  "
                     go to acc-cod-fnt-600
           else      go to acc-cod-fnt-100.
       acc-cod-fnt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore attuale pari al valore precedente *
      *                  * : nessuna dipendenza                        *
      *                  *---------------------------------------------*
           if        w-rig-cod-arc        =    w-sav-cod-arc
                     go to acc-cod-fnt-800.
      *                  *---------------------------------------------*
      *                  * Normalizzazione valori legati al codice di- *
      *                  * pendenza del fornitore                      *
      *                  *---------------------------------------------*
           move      spaces               to   w-rig-dpz-arc          .
           perform   vis-dpz-arc-000      thru vis-dpz-arc-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione se presenti dipendenze per   *
      *                  * il fornitore commerciale                    *
      *                  *---------------------------------------------*
           move      w-rig-cod-arc        to   w-det-snd-dcf-fnt      .
           perform   det-snd-dcf-000      thru det-snd-dcf-999        .
       acc-cod-fnt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-rig-000
                                          thru cnt-tdo-rig-999
                     if      w-cnt-tdo-rig-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-rig
                             go to acc-cod-fnt-999
                     else    move  spaces to   w-cnt-tdo-rig-flg
                             go to acc-cod-fnt-100.
       acc-cod-fnt-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Dipendenza archivio                        *
      *    *-----------------------------------------------------------*
       acc-dpz-arc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Solo se riga 'new' e in 'append'        *
      *                      *-----------------------------------------*
           if        w-rlt-sup-new        =    spaces or
                     w-rlt-sup-lst        =    spaces
                     go to acc-dpz-arc-999.
      *                      *-----------------------------------------*
      *                      * Solo se conto merce diverso da spaces   *
      *                      *-----------------------------------------*
           if        w-rig-cod-mic        =    spaces
                     go to acc-dpz-arc-999.
       acc-dpz-arc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo archivio      *
      *                  *---------------------------------------------*
           if        w-rig-tip-arc        =    "C"
                     go to acc-dpz-arc-200
           else if   w-rig-tip-arc        =    "F"
                     go to acc-dpz-arc-400
           else      go to acc-dpz-arc-999.
       acc-dpz-arc-200.
      *                  *---------------------------------------------*
      *                  * Se tipo archivio : Cliente                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Accettazione dipendenza Cliente         *
      *                      *-----------------------------------------*
           perform   acc-dpz-cli-000      thru acc-dpz-cli-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     acc-dpz-arc-999.
       acc-dpz-arc-400.
      *                  *---------------------------------------------*
      *                  * Se tipo archivio : Fornitore                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Accettazione dipendenza Fornitore       *
      *                      *-----------------------------------------*
           perform   acc-dpz-fnt-000      thru acc-dpz-fnt-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     acc-dpz-arc-999.
       acc-dpz-arc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Dipendenza archivio                     *
      *    *-----------------------------------------------------------*
       vis-dpz-arc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      77                   to   v-pos                  .
           move      w-rig-dpz-arc        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dpz-arc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice dipendenza del cliente              *
      *    *-----------------------------------------------------------*
       acc-dpz-cli-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test  se campo da accettare                 *
      *                  *---------------------------------------------*
           if        w-det-snd-dcc-snx    =    "N"
                     go to acc-dpz-cli-999.
       acc-dpz-cli-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-dcc-ope      .
           move      w-rig-cod-arc        to   w-cod-cod-dcc-cli      .
           move      w-rig-dpz-arc        to   w-cod-cod-dcc-cod      .
           move      19                   to   w-cod-cod-dcc-lin      .
           move      77                   to   w-cod-cod-dcc-pos      .
           move      zero                 to   w-cod-cod-dcc-rln      .
           move      zero                 to   w-cod-cod-dcc-rps      .
           move      zero                 to   w-cod-cod-dcc-vln      .
           move      zero                 to   w-cod-cod-dcc-vps      .
           move      zero                 to   w-cod-cod-dcc-lln      .
           move      zero                 to   w-cod-cod-dcc-lps      .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           if        w-cnt-sts-imp-rig    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
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
                     move  "E"            to   w-cnt-tus-acc-rig
                     go to acc-dpz-cli-999.
       acc-dpz-cli-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-rig-dpz-arc          .
       acc-dpz-cli-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura dipendenza per cliente              *
      *                  *---------------------------------------------*
           move      w-rig-cod-arc        to   w-let-arc-dcc-cod      .
           move      w-rig-dpz-arc        to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                  *---------------------------------------------*
      *                  * Se dipendenza non esistente: reimpostazione *
      *                  *---------------------------------------------*
           if        w-let-arc-dcc-flg    not  = spaces
                     go to acc-dpz-cli-100.
       acc-dpz-cli-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dpz-cli-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-rig-000
                                          thru cnt-tdo-rig-999
                     if      w-cnt-tdo-rig-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-rig
                             go to acc-dpz-cli-999
                     else    move  spaces to   w-cnt-tdo-rig-flg
                             go to acc-dpz-cli-100.
       acc-dpz-cli-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice dipendenza del fornitore            *
      *    *-----------------------------------------------------------*
       acc-dpz-fnt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test  se campo da accettare                 *
      *                  *---------------------------------------------*
           if        w-det-snd-dcf-snx    =    "N"
                     go to acc-dpz-fnt-999.
       acc-dpz-fnt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-dcf-ope      .
           move      w-rig-cod-arc        to   w-cod-cod-dcf-fnt      .
           move      w-rig-dpz-arc        to   w-cod-cod-dcf-cod      .
           move      19                   to   w-cod-cod-dcf-lin      .
           move      77                   to   w-cod-cod-dcf-pos      .
           move      zero                 to   w-cod-cod-dcf-rln      .
           move      zero                 to   w-cod-cod-dcf-rps      .
           move      zero                 to   w-cod-cod-dcf-vln      .
           move      zero                 to   w-cod-cod-dcf-vps      .
           move      zero                 to   w-cod-cod-dcf-lln      .
           move      zero                 to   w-cod-cod-dcf-lps      .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           if        w-cnt-sts-imp-rig    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           perform   cod-cod-dcf-cll-000  thru cod-cod-dcf-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-dcf-foi-000  thru cod-cod-dcf-foi-999    .
       acc-dpz-fnt-110.
           perform   cod-cod-dcf-cll-000  thru cod-cod-dcf-cll-999    .
           if        w-cod-cod-dcf-ope    =    "F+"
                     go to acc-dpz-fnt-115.
           if        w-cod-cod-dcf-ope    =    "AC"
                     go to acc-dpz-fnt-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-dpz-fnt-115.
           perform   cod-cod-dcf-foi-000  thru cod-cod-dcf-foi-999    .
           go to     acc-dpz-fnt-110.
       acc-dpz-fnt-120.
           move      w-cod-cod-dcf-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-rig
                     go to acc-dpz-fnt-999.
       acc-dpz-fnt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-rig-dpz-arc          .
       acc-dpz-fnt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura dipendenza per fornitore            *
      *                  *---------------------------------------------*
           move      w-rig-cod-arc        to   w-let-arc-dcf-cod      .
           move      w-rig-dpz-arc        to   w-let-arc-dcf-dpz      .
           perform   let-arc-dcf-000      thru let-arc-dcf-999        .
      *                  *---------------------------------------------*
      *                  * Se dipendenza non esistente: reimpostazione *
      *                  *---------------------------------------------*
           if        w-let-arc-dcf-flg    not  = spaces
                     go to acc-dpz-fnt-100.
       acc-dpz-fnt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dpz-fnt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-rig-000
                                          thru cnt-tdo-rig-999
                     if      w-cnt-tdo-rig-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-rig
                             go to acc-dpz-fnt-999
                     else    move  spaces to   w-cnt-tdo-rig-flg
                             go to acc-dpz-fnt-100.
       acc-dpz-fnt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Quantita' presunta                *
      *    *-----------------------------------------------------------*
       vis-qta-prs-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      w-rig-dec-qta        to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           if        w-rig-qta-prs        =    zero
                     move  "<GD"          to   v-edm
           else      move  "<G"           to   v-edm                  .
           move      20                   to   v-lin                  .
           move      19                   to   v-pos                  .
           move      w-rig-qta-prs        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-qta-prs-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Quantita' rilevata                         *
      *    *-----------------------------------------------------------*
       acc-qta-rlv-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione valore di default              *
      *                  *---------------------------------------------*
           if        w-rig-flg-ril        not  = spaces
                     go to acc-qta-rlv-100.
           move      w-rig-qta-prs        to   w-rig-qta-rlv          .
       acc-qta-rlv-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      w-rig-dec-qta        to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<GD"                to   v-edm                  .
           move      21                   to   v-lin                  .
           move      19                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-rig    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-rig-qta-rlv        to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-qta-rlv-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-rig
                     go to acc-qta-rlv-999.
      *              *-------------------------------------------------*
      *              * Valore impostato in destinazione                *
      *              *-------------------------------------------------*
           move      v-num                to   w-rig-qta-rlv          .
       acc-qta-rlv-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-qta-rlv-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-qta-rlv-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-rig-000
                                          thru cnt-tdo-rig-999
                     if      w-cnt-tdo-rig-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-rig
                             go to acc-qta-rlv-999
                     else    move  spaces to   w-cnt-tdo-rig-flg
                             go to acc-qta-rlv-100.
       acc-qta-rlv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Quantita' rilevata                *
      *    *-----------------------------------------------------------*
       vis-qta-rlv-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      w-rig-dec-qta        to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           if        w-rig-flg-ril        =    spaces
                     if    w-rig-qta-rlv  =    zero
                           move  "<GBD"   to   v-edm
                     else  move  "<GB"    to   v-edm
           else      if    w-rig-qta-rlv  =    zero
                           move  "<GD"    to   v-edm
                     else  move  "<G"     to   v-edm                  .
           move      21                   to   v-lin                  .
           move      19                   to   v-pos                  .
           move      w-rig-qta-rlv        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-qta-rlv-999.
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
      *    * Controllo su impostazione tasto Do riga corpo             *
      *    *-----------------------------------------------------------*
       cnt-tdo-rig-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-rig-flg      .
      *              *-------------------------------------------------*
      *              * Determinazione Flag di rilevazione eseguita     *
      *              *-------------------------------------------------*
           move      "#"                  to   w-rig-flg-ril          .
       cnt-tdo-rig-100.
      *              *-------------------------------------------------*
      *              * Se riga non New : a fine controlli              *
      *              *-------------------------------------------------*
           if        w-rlt-sup-new        =    spaces
                     go to cnt-tdo-rig-800.
      *              *-------------------------------------------------*
      *              * Controlli su impostazioni riga                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice conto merce                  *
      *                  *---------------------------------------------*
           if        w-rig-cod-mic        not  = spaces
                     go to cnt-tdo-rig-120.
           move      "Manca il tipo conto merce !"
                                          to   w-box-msg-err-msg      .
           go to     cnt-tdo-rig-900.
       cnt-tdo-rig-120.
      *                  *---------------------------------------------*
      *                  * Test su tipo archivio                       *
      *                  *---------------------------------------------*
           if        w-rig-tip-arc        not  = spaces
                     go to cnt-tdo-rig-140.
           move      "Tipo archivio indefinito !"
                                          to   w-box-msg-err-msg      .
           go to     cnt-tdo-rig-900.
       cnt-tdo-rig-140.
      *                  *---------------------------------------------*
      *                  * Test su codice archivio                     *
      *                  *---------------------------------------------*
           if        w-rig-cod-arc        not  = zero
                     go to cnt-tdo-rig-200.
           move      "Manca il codice archivio !"
                                          to   w-box-msg-err-msg      .
           go to     cnt-tdo-rig-900.
       cnt-tdo-rig-200.
      *              *-------------------------------------------------*
      *              * Controllo se record impostato gia' esistente    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tentativo di lettura record [mim]           *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "MAGTGC    "         to   f-key                  .
           move      w-tes-dpz-inu        to   rf-mim-cod-dpz         .
           move      02                   to   rf-mim-tip-mag         .
           move      w-rig-num-mag        to   rf-mim-num-mag         .
           move      w-rig-var-mag        to   rf-mim-var-mag         .
           move      02                   to   rf-mim-tip-gia         .
           move      w-rig-tip-arc        to   rf-mim-tip-arc         .
           move      w-rig-cod-arc        to   rf-mim-cod-arc         .
           move      w-rig-dpz-arc        to   rf-mim-dpz-arc         .
           move      w-rig-cod-mic        to   rf-mim-cod-mic         .
           move      "pgm/mag/fls/ioc/obj/iofmim"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mim                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato : oltre               *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cnt-tdo-rig-300.
      *                      *-----------------------------------------*
      *                      * Composizione messaggio di errore        *
      *                      *-----------------------------------------*
           move      "Attenzione : riga di inventario gia' esistente !"
                                          to   w-box-msg-err-msg      .
           go to     cnt-tdo-rig-900.
       cnt-tdo-rig-300.
      *              *-------------------------------------------------*
      *              * Controllo se record impostato gia' presente fra *
      *              * gli elementi 'nuovi' della catena               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio area catena                     *
      *                  *---------------------------------------------*
           move      w-rlt-sup            to   w-sav-rlt-sup          .
      *                  *---------------------------------------------*
      *                  * Salvataggio area w-rig                      *
      *                  *---------------------------------------------*
           move      w-rig                to   w-sav-rig              .
      *                  *---------------------------------------------*
      *                  * Salvataggio valori fondamentali di w-rig    *
      *                  *---------------------------------------------*
           move      w-rig-alf-mag        to   w-sav-alf-mag          .
           move      w-rig-var-mag        to   w-sav-var-mag          .
           move      w-rig-cod-mic        to   w-sav-cod-mic          .
           move      w-rig-tip-arc        to   w-sav-tip-arc          .
           move      w-rig-cod-arc        to   w-sav-cod-arc          .
           move      w-rig-dpz-arc        to   w-sav-dpz-arc          .
      *              *-------------------------------------------------*
      *              * Inizializzazione contatore records              *
      *              *-------------------------------------------------*
           move      w-sav-rig-max        to   w-rlt-sup-ctr          .
       cnt-tdo-rig-320.
      *              *-------------------------------------------------*
      *              * Incremento contatore records                    *
      *              *-------------------------------------------------*
           add       1                    to   w-rlt-sup-ctr          .
      *              *-------------------------------------------------*
      *              * Se maggiore del numero di records caricati  :   *
      *              * a fine controlli                                *
      *              *-------------------------------------------------*
           if        w-rlt-sup-ctr        >    w-rlt-sup-max
                     go to cnt-tdo-rig-700.
      *              *-------------------------------------------------*
      *              * Lettura riga da file relative di supporto       *
      *              *-------------------------------------------------*
           move      "RD"                 to   w-rlt-sup-ope          .
           move      w-rlt-sup-ctr        to   w-rlt-sup-prg          .
           perform   cll-rlt-sup-000      thru cll-rlt-sup-999        .
      *              *-------------------------------------------------*
      *              * Movimento da buffer catena movimenti a work     *
      *              *-------------------------------------------------*
           move      w-rlt-sup-buf        to   w-rig                  .
      *              *-------------------------------------------------*
      *              * Se record non presente : riciclo                *
      *              *-------------------------------------------------*
           if        w-rig-alf-mag        not  = w-sav-alf-mag or
                     w-rig-var-mag        not  = w-sav-var-mag or
                     w-rig-cod-mic        not  = w-sav-cod-mic or
                     w-rig-tip-arc        not  = w-sav-tip-arc or
                     w-rig-cod-arc        not  = w-sav-cod-arc or
                     w-rig-dpz-arc        not  = w-sav-dpz-arc
                     go to cnt-tdo-rig-320.
      *                  *---------------------------------------------*
      *                  * Composizione messaggio di errore            *
      *                  *---------------------------------------------*
           move      "Attenzione : riga di inventario gia' presente in c
      -              "atena !"
                                          to   w-box-msg-err-msg      .
      *                  *---------------------------------------------*
      *                  * Flag di uscita a errore                     *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-tdo-rig-flg      .
       cnt-tdo-rig-700.
      *                  *---------------------------------------------*
      *                  * Ripristino area catena                      *
      *                  *---------------------------------------------*
           move      w-sav-rlt-sup        to   w-rlt-sup              .
      *                  *---------------------------------------------*
      *                  * Ripristino area w-rig                       *
      *                  *---------------------------------------------*
           move      w-sav-rig            to   w-rig                  .
      *                  *---------------------------------------------*
      *                  * Test su flag di uscita                      *
      *                  *---------------------------------------------*
           if        w-cnt-tdo-rig-flg    not  = spaces
                     go to cnt-tdo-rig-900.
       cnt-tdo-rig-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cnt-tdo-rig-999.
       cnt-tdo-rig-900.
      *              *-------------------------------------------------*
      *              * Trattamento errore                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Emissione messaggio di errore               *
      *                  *---------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di uscita a errore                     *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-tdo-rig-flg      .
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
           move      w-rlt-sup-max        to   w-rig-num-prg          .
           add       1                    to   w-rig-num-prg          .
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
      *              *-------------------------------------------------*
      *              * Inizializzazione file relative di supporto      *
      *              *-------------------------------------------------*
           move      "BE"                 to   w-rlt-sup-ope          .
           perform   cll-rlt-sup-000      thru cll-rlt-sup-999        .
       nor-key-nok-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati chiave                               *
      *    *-----------------------------------------------------------*
       nor-key-reg-000.
           move      zero                 to   w-tes-fso-dps          .
           move      spaces               to   w-tes-fso-dps-des      .
           move      zero                 to   w-tes-fso-dps-ord      .
           move      spaces               to   w-tes-cnf-imp          .
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *-----------------------------------------------------------*
       nor-nok-tes-000.
       nor-nok-tes-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave riga corpo                *
      *    *-----------------------------------------------------------*
       nor-nok-rig-000.
           move      zero                 to   w-rig-num-prg          .
           move      zero                 to   w-rig-num-mag          .
           move      spaces               to   w-rig-var-mag          .
           move      spaces               to   w-rig-alf-mag          .
           move      spaces               to   w-rig-des-mag          .
           move      spaces               to   w-rig-uni-mis          .
           move      zero                 to   w-rig-dec-qta          .
           move      spaces               to   w-rig-cod-mic          .
           move      spaces               to   w-rig-cod-mic-des      .
           move      spaces               to   w-rig-tip-arc          .
           move      zero                 to   w-rig-cod-arc          .
           move      spaces               to   w-rig-cod-arc-des      .
           move      spaces               to   w-rig-dpz-arc          .
           move      zero                 to   w-rig-qta-prs          .
           move      spaces               to   w-rig-flg-ril          .
           move      zero                 to   w-rig-qta-rlv          .
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
      *              * Messaggio di caricamento in esecuzione          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Linea 23                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      23                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Linea 24                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      24                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "               --- Caricamento dati inventariali i
      -              "n esecuzione ---              "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Inizializzazione numero progressivo riga        *
      *              *-------------------------------------------------*
           move      zero                 to   w-rig-num-prg          .
      *              *-------------------------------------------------*
      *              * Tipo funzionamento : Inserimento                *
      *              *-------------------------------------------------*
           move      "I"                  to   w-cnt-mfu-tip-fun      .
      *              *-------------------------------------------------*
      *              * Start filtro di ordinamento e selezione per il  *
      *              * file [dps]                                      *
      *              *-------------------------------------------------*
           move      "ST"                 to   f-ope                  .
           move      w-tes-fso-dps        to   f-key                  .
           move      "pgm/dps/prg/obj/bzosdps0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : a trattamento errore      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rou-let-reg-300.
       rou-let-reg-100.
      *              *-------------------------------------------------*
      *              * Read Next filtro di ordinamento e selezione per *
      *              * file [dps]                                      *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dps/prg/obj/bzosdps0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
      *                  *---------------------------------------------*
      *                  * Test se 'At end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rou-let-reg-300.
      *                  *---------------------------------------------*
      *                  * Start su file [mim]                         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "MAGTGC    "         to   f-key                  .
           move      w-tes-dpz-inu        to   rf-mim-cod-dpz         .
           move      02                   to   rf-mim-tip-mag         .
           move      rf-dps-num-sem       to   rf-mim-num-mag         .
           move      zero                 to   rf-mim-tip-gia         .
           move      spaces               to   rf-mim-tip-arc         .
           move      zero                 to   rf-mim-cod-arc         .
           move      spaces               to   rf-mim-cod-mic         .
           move      "pgm/mag/fls/ioc/obj/iofmim"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mim                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : riciclo su lettura filtro *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rou-let-reg-100.
       rou-let-reg-200.
      *                  *---------------------------------------------*
      *                  * Lettura sequenziale record [mim]            *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmim"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mim                 .
      *                  *---------------------------------------------*
      *                  * Test se 'At end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rou-let-reg-100.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
           if        rf-mim-cod-dpz       not  = w-tes-dpz-inu or
                     rf-mim-tip-mag       not  = 02            or
                     rf-mim-num-mag       not  = rf-dps-num-sem
                     go to rou-let-reg-100.
      *                  *---------------------------------------------*
      *                  * Selezione sul record                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su tipo giacenza                   *
      *                      *-----------------------------------------*
           if        rf-mim-tip-gia       =    03
                     go to rou-let-reg-200.
      *                  *---------------------------------------------*
      *                  * Incremento numero progressivo riga          *
      *                  *---------------------------------------------*
           if        w-rig-num-prg        <    99999
                     add   1              to   w-rig-num-prg
           else      go to rou-let-reg-300.
       rou-let-reg-220.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori contenuti diretta-   *
      *                  * mente in record [mim]                       *
      *                  *---------------------------------------------*
           move      rf-mim-num-mag       to   w-rig-num-mag          .
           move      rf-mim-var-mag       to   w-rig-var-mag          .
           move      rf-mim-alf-mag       to   w-rig-alf-mag          .
           move      rf-mim-des-mag       to   w-rig-des-mag          .
           move      rf-mim-umi-mag       to   w-rig-uni-mis          .
           move      rf-mim-dec-qta       to   w-rig-dec-qta          .
           move      rf-mim-cod-mic       to   w-rig-cod-mic          .
           move      rf-mim-tip-arc       to   w-rig-tip-arc          .
           move      rf-mim-cod-arc       to   w-rig-cod-arc          .
           move      rf-mim-dpz-arc       to   w-rig-dpz-arc          .
           move      rf-mim-qta-prs       to   w-rig-qta-prs          .
           move      rf-mim-flg-ril       to   w-rig-flg-ril          .
           move      rf-mim-qta-rlv       to   w-rig-qta-rlv          .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori contenuti indiretta- *
      *                  * mente in record [mim]                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura descrizione merce in conto      *
      *                      *-----------------------------------------*
           move      w-rig-cod-mic        to   w-let-arc-zmm-cod      .
           perform   let-arc-zmm-000      thru let-arc-zmm-999        .
           move      w-let-arc-zmm-des    to   w-rig-cod-mic-des      .
      *                      *-----------------------------------------*
      *                      * Lettura decrizione archivio             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del tipo ar- *
      *                          * chivio                              *
      *                          *-------------------------------------*
           if        w-rig-tip-arc        =    "C"
                     go to  rou-let-reg-222
           else if   w-rig-tip-arc        =    "F"
                     go to  rou-let-reg-224.
.       rou-let-reg-222.
      *                          *-------------------------------------*
      *                          * Tipo archivio : Clienti             *
      *                          *-------------------------------------*
           move      w-rig-cod-arc        to   w-let-arc-cli-cod      .
           perform   let-arc-cli-000      thru let-arc-cli-999        .
           move      w-let-arc-cli-rag    to   w-rig-cod-arc-des      .
           go to     rou-let-reg-230.
.       rou-let-reg-224.
      *                          *-------------------------------------*
      *                          * Tipo archivio : Fornitori           *
      *                          *-------------------------------------*
           move      w-rig-cod-arc        to   w-let-arc-fnt-cod      .
           perform   let-arc-fnt-000      thru let-arc-fnt-999        .
           move      w-let-arc-fnt-rag    to   w-rig-cod-arc-des      .
           go to     rou-let-reg-230.
.       rou-let-reg-230.
      *                      *-----------------------------------------*
      *                      * Aggiustamento tipo funzionamento        *
      *                      *-----------------------------------------*
           if        w-rig-flg-ril        not  = spaces and
                     w-cnt-mfu-tip-fun    not  = "M"
                     move  "M"            to   w-cnt-mfu-tip-fun      .
      *                      *-----------------------------------------*
      *                      * Put su catena movimenti [rig]           *
      *                      *-----------------------------------------*
           move      "PT"                 to   w-rlt-sup-ope          .
           move      w-rig-num-prg        to   w-rlt-sup-prg          .
           move      w-rig                to   w-rlt-sup-buf          .
           perform   cll-rlt-sup-000      thru cll-rlt-sup-999        .
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura [mim] successivo      *
      *                      *-----------------------------------------*
           go to     rou-let-reg-200.
        rou-let-reg-300.
      *              *-------------------------------------------------*
      *              * Eliminazione messaggio di caricamento in esecu- *
      *              * zione                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Linea 23                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      23                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Linea 24                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      24                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del numero di records ca-  *
      *              * ricati in catena                                *
      *              *-------------------------------------------------*
           if        w-rlt-sup-max        =    zero
                     go to rou-let-reg-320
           else      go to rou-let-reg-340.
       rou-let-reg-320.
      *              *-------------------------------------------------*
      *              * Se numero di records caricati : zero            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Nessuna riga di inventario da rilevare !          
      -              "          "         to   w-box-msg-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Status di uscita ad errore                  *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-rou-let-reg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rou-let-reg-999.
       rou-let-reg-340.
      *              *-------------------------------------------------*
      *              * Se numero di records caricati maggiore di zero  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio del numero riga massimo         *
      *                  *---------------------------------------------*
           move      w-rlt-sup-max        to   w-sav-rig-max          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
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
      *    * Routine pre-richiesta di ratifica tasto Delete            *
      *    *-----------------------------------------------------------*
       pre-snx-del-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-snx-del      .
      *              *-------------------------------------------------*
      *              * Si ignora comunque il tasto Delt                *
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
      *              *-------------------------------------------------*
      *              * Si ignora comunque il tasto Delt                *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-pos-snx-del      .
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
       pos-cnf-ann-999.
           exit.

      *    *===========================================================*
      *    * Scrittura movimento su file                               *
      *    *-----------------------------------------------------------*
       scr-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Messaggio di aggiornamento in esecuzione        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Linea 23                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      23                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Linea 24                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      24                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "     --- Aggiornamento rilevazioni inventariali im
      -              "postate in esecuzione ---     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       scr-mov-fil-050.
      *              *-------------------------------------------------*
      *              * Contatore records scaricati a zero              *
      *              *-------------------------------------------------*
           move      zero                 to   w-rlt-sup-ctr          .
       scr-mov-fil-100.
      *              *-------------------------------------------------*
      *              * Incremento contatore records scaricati          *
      *              *-------------------------------------------------*
           add       1                    to   w-rlt-sup-ctr          .
      *              *-------------------------------------------------*
      *              * Se maggiore del numero di records caricati  :   *
      *              * uscita                                          *
      *              *-------------------------------------------------*
           if        w-rlt-sup-ctr        >    w-rlt-sup-max
                     go to scr-mov-fil-900.
      *              *-------------------------------------------------*
      *              * Lettura riga da file relative di supporto       *
      *              *-------------------------------------------------*
           move      "RD"                 to   w-rlt-sup-ope          .
           move      w-rlt-sup-ctr        to   w-rlt-sup-prg          .
           perform   cll-rlt-sup-000      thru cll-rlt-sup-999        .
      *              *-------------------------------------------------*
      *              * Movimento da buffer catena movimenti a work     *
      *              *-------------------------------------------------*
           move      w-rlt-sup-buf        to   w-rig                  .
      *              *-------------------------------------------------*
      *              * Selezione sul record                            *
      *              *-------------------------------------------------*
           if        w-rig-flg-ril        =    spaces
                     go to scr-mov-fil-100.
       scr-mov-fil-200.
      *              *-------------------------------------------------*
      *              * Trattamento file [mim]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se riga non nuova                           *
      *                  *---------------------------------------------*
           if        w-rig-num-prg        >   w-sav-rig-max
                     go to scr-mov-fil-250.
      *                      *-----------------------------------------*
      *                      * Update [mim]                            *
      *                      *-----------------------------------------*
           perform   upd-rec-mim-000      thru upd-rec-mim-999        .
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura riga successiva       *
      *                      *-----------------------------------------*
           go to     scr-mov-fil-100.
       scr-mov-fil-250.
      *                  *---------------------------------------------*
      *                  * Se riga nuova                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Write record [mim]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-mim-000      thru wrt-rec-mim-999        .
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura riga corpo successiva *
      *                      *-----------------------------------------*
           go to     scr-mov-fil-100.
       scr-mov-fil-900.
      *              *-------------------------------------------------*
      *              * Eliminazione messaggio di aggiornamento in ese- *
      *              * cuzione                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Linea 23                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      23                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Linea 24                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      24                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record righe [mim]                              *
      *    *-----------------------------------------------------------*
       wrt-rec-mim-000.
      *              *-------------------------------------------------*
      *              * Test se record da scrivere                      *
      *              *-------------------------------------------------*
           if        w-rig-qta-rlv        =    zero
                     go to wrt-rec-mim-999.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [mim]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmim"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mim                 .
      *              *-------------------------------------------------*
      *              * Composizione record [mim]                       *
      *              *-------------------------------------------------*
           move      w-tes-dpz-inu        to   rf-mim-cod-dpz         .
           move      02                   to   rf-mim-tip-mag         .
           move      w-rig-num-mag        to   rf-mim-num-mag         .
           move      w-rig-var-mag        to   rf-mim-var-mag         .
           move      w-rig-alf-mag        to   rf-mim-alf-mag         .
           move      w-rig-des-mag        to   rf-mim-des-mag         .
           move      w-rig-uni-mis        to   rf-mim-umi-mag         .
           move      w-rig-dec-qta        to   rf-mim-dec-qta         .
           move      spaces               to   rf-mim-tip-ubi         .
           move      spaces               to   rf-mim-prm-ubi (1)     .
           move      spaces               to   rf-mim-prm-ubi (2)     .
           move      spaces               to   rf-mim-prm-ubi (3)     .
           move      spaces               to   rf-mim-prm-ubi (4)     .
           if        w-rig-cod-mic        =    spaces
                     move  01             to   rf-mim-tip-gia
           else      move  02             to   rf-mim-tip-gia         .
           move      w-rig-cod-mic        to   rf-mim-cod-mic         .
           move      w-rig-tip-arc        to   rf-mim-tip-arc         .
           move      w-rig-cod-arc        to   rf-mim-cod-arc         .
           move      w-rig-dpz-arc        to   rf-mim-dpz-arc         .
           move      w-rig-qta-prs        to   rf-mim-qta-prs         .
           move      w-rig-flg-ril        to   rf-mim-flg-ril         .
           move      w-rig-qta-rlv        to   rf-mim-qta-rlv         .
           move      spaces               to   rf-mim-alx-exp         .
      *              *-------------------------------------------------*
      *              * Put record [mim]                                *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmim"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mim                 .
       wrt-rec-mim-999.
           exit.

      *    *===========================================================*
      *    * Aggiornamento record righe [mim]                          *
      *    *-----------------------------------------------------------*
       upd-rec-mim-000.
      *              *-------------------------------------------------*
      *              * Ottenimento record                              *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "MAGTGC    "         to   f-key                  .
           move      w-tes-dpz-inu        to   rf-mim-cod-dpz         .
           move      02                   to   rf-mim-tip-mag         .
           move      w-rig-num-mag        to   rf-mim-num-mag         .
           move      w-rig-var-mag        to   rf-mim-var-mag         .
           if        w-rig-cod-mic        =    spaces
                     move  01             to   rf-mim-tip-gia
           else      move  02             to   rf-mim-tip-gia         .
           move      w-rig-tip-arc        to   rf-mim-tip-arc         .
           move      w-rig-cod-arc        to   rf-mim-cod-arc         .
           move      w-rig-dpz-arc        to   rf-mim-dpz-arc         .
           move      w-rig-cod-mic        to   rf-mim-cod-mic         .
           move      "pgm/mag/fls/ioc/obj/iofmim"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mim                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to upd-rec-mim-999.
      *              *-------------------------------------------------*
      *              * Aggiornamento record                            *
      *              *-------------------------------------------------*
           move      w-rig-flg-ril        to   rf-mim-flg-ril         .
           move      w-rig-qta-rlv        to   rf-mim-qta-rlv         .
      *              *-------------------------------------------------*
      *              * Update record                                   *
      *              *-------------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmim"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mim                 .
      *              *-------------------------------------------------*
      *              * Release record                                  *
      *              *-------------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmim"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mim                 .
       upd-rec-mim-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio [zos] per [dps]                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dps/prg/cpy/lzosdps0.lts"                   .

      *    *===========================================================*
      *    * Routine lettura archivio [dps]                            *
      *    *-----------------------------------------------------------*
       let-arc-dps-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dps-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice alfanumerico a spaces            *
      *              *-------------------------------------------------*
           if        w-let-arc-dps-alf    =    spaces
                     go to let-arc-dps-500.
      *              *-------------------------------------------------*
      *              * Start per codice alfanumerico                   *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "ALFSEM    "         to   f-key                  .
           move      w-let-arc-dps-alf    to   rf-dps-alf-sem         .
           move      zero                 to   rf-dps-num-sem         .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-dps-400.
      *              *-------------------------------------------------*
      *              * Lettura primo record                            *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
      *                  *---------------------------------------------*
      *                  * Test se 'At end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-arc-dps-400.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
           if        rf-dps-alf-sem       not  = w-let-arc-dps-alf
                     go to let-arc-dps-400.
       let-arc-dps-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-dps-num-sem       to   w-let-arc-dps-num      .
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
           move      zero                 to   w-let-arc-dps-num      .
           move      spaces               to   w-let-arc-dps-umi      .
           move      zero                 to   w-let-arc-dps-deq      .
       let-arc-dps-999.
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
           go to     let-arc-cli-999.
       let-arc-cli-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cli-rag      .
       let-arc-cli-999.
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
           go to     let-arc-fnt-999.
       let-arc-fnt-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-fnt-rag      .
       let-arc-fnt-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio [dcc] per la dipendenza          *
      *    *-----------------------------------------------------------*
       let-arc-dcc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice cliente a zero                   *
      *              *-------------------------------------------------*
           if        w-let-arc-dcc-cod    =    zero
                     go to let-arc-dcc-500.
      *              *-------------------------------------------------*
      *              * Test se codice dipendenza a spaces              *
      *              *-------------------------------------------------*
           if        w-let-arc-dcc-dpz    =    spaces
                     go to let-arc-dcc-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
           move      w-let-arc-dcc-cod    to   rf-dcc-cod-cli         .
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
           go to     let-arc-dcc-600.
       let-arc-dcc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcc-rag      .
       let-arc-dcc-600.
           move      spaces               to   w-let-arc-dcc-via      .
           move      spaces               to   w-let-arc-dcc-loc      .
       let-arc-dcc-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio [dcf] per la dipendenza          *
      *    *-----------------------------------------------------------*
       let-arc-dcf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcf-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice fornitore a zero                 *
      *              *-------------------------------------------------*
           if        w-let-arc-dcf-cod    =    zero
                     go to let-arc-dcf-500.
      *              *-------------------------------------------------*
      *              * Test se codice dipendenza a spaces              *
      *              *-------------------------------------------------*
           if        w-let-arc-dcf-dpz    =    spaces
                     go to let-arc-dcf-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFNT    "         to   f-key                  .
           move      w-let-arc-dcf-cod    to   rf-dcf-cod-fnt         .
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
           move      spaces               to   w-let-arc-dcf-via      .
           move      spaces               to   w-let-arc-dcf-loc      .
       let-arc-dcf-999.
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
      *    * Determinazione se presenti dipendenze per il fornitore    *
      *    * commerciale                                               *
      *    *-----------------------------------------------------------*
       det-snd-dcf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di esito                   *
      *              *-------------------------------------------------*
           move      "N"                  to   w-det-snd-dcf-snx      .
      *              *-------------------------------------------------*
      *              * Normalizzazione codice dipendenza unica         *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-snd-dcf-dpz      .
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-snd-dcf-ctr      .
      *              *-------------------------------------------------*
      *              * Start su file [dcf]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODFNT    "         to   f-key                  .
           move      w-det-snd-dcf-fnt    to   rf-dcf-cod-fnt         .
           move      spaces               to   rf-dcf-dpz-fnt         .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : uscita con flag a 'no'        *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-snd-dcf-999.
       det-snd-dcf-100.
      *              *-------------------------------------------------*
      *              * Next su [dcf]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *              *-------------------------------------------------*
      *              * Se 'at end' : a test finale                     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-snd-dcf-800.
      *              *-------------------------------------------------*
      *              * Max su [dcf], se non superato : a test finale   *
      *              *-------------------------------------------------*
           if        rf-dcf-cod-fnt       not  = w-det-snd-dcf-fnt
                     go to det-snd-dcf-800.
       det-snd-dcf-200.
      *              *-------------------------------------------------*
      *              * Test sul codice dipendenza                      *
      *              *-------------------------------------------------*
           if        rf-dcf-dpz-fnt       =    spaces
                     go to det-snd-dcf-100.
       det-snd-dcf-300.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   w-det-snd-dcf-ctr      .
       det-snd-dcf-300.
      *              *-------------------------------------------------*
      *              * Bufferizzazione del primo codice dipendenza     *
      *              *-------------------------------------------------*
           if        w-det-snd-dcf-ctr    >    1
                     go to det-snd-dcf-500.
           move      rf-dcf-dpz-fnt       to   w-det-snd-dcf-dpz      .
       det-snd-dcf-500.
      *              *-------------------------------------------------*
      *              * Riciclo a record [dcf] successivo               *
      *              *-------------------------------------------------*
           go to     det-snd-dcf-100.
       det-snd-dcf-800.
      *              *-------------------------------------------------*
      *              * Determinazione finale                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se durante la ricerca e' stato trovato un   *
      *                  * numero di dipendenze superiore a zero si    *
      *                  * esce con il flag di presenza a 'S'          *
      *                  *---------------------------------------------*
           if        w-det-snd-dcf-ctr    >    zero
                     go to det-snd-dcf-900
           else      go to det-snd-dcf-999.
       det-snd-dcf-900.
      *              *-------------------------------------------------*
      *              * Uscita per dipendenze trovate                   *
      *              *-------------------------------------------------*
           move      "S"                  to   w-det-snd-dcf-snx      .
       det-snd-dcf-999.
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
      *    * Subroutines per l'accettazione del codice filtro ordina-  *
      *    * mento e selezione per file [dps]                          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dps/prg/cpy/azosdps0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per accettazione codice semilavorato          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dps/prg/cpy/acoddps0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice conto merce     *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/acdezmm0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice cliente commerciale *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmndcc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice dipendenza cliente  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoddcc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice fornitore com-  *
      *    * merciale                                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmndcf0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice dipendenza fornit.  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acoddcf0.acs"                   .
