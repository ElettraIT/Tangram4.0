       Identification Division.
       Program-Id.                                 pbfo0100           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    bfo                 *
      *                                Settore:    tab                 *
      *                                   Fase:    bfo010              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 25/07/92    *
      *                       Ultima revisione:    NdK del 21/11/23    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione archivio tipi movimento per bol-   *
      *                    le fornitori                                *
      *                                                                *
      *                    PROMEMORIA: Eliminare la gestione delle     *
      *                                righe corpo (Dislocazioni)      *
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
                     "tab"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "bfo010"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pbfo0100"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "   TIPI MOVIMENTO PER BOLLE FORNITORI   "       .

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

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [ybf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfybf"                          .
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
      *        * [ada]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfada"                          .

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
               10  w-tes-pwd-tmb          pic  x(08)                  .
               10  w-tes-int-ftr          pic  9(02)                  .
               10  w-tes-tmo-ftr          pic  x(05)                  .
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
               10  w-tes-def-tar          pic  x(01)                  .
               10  w-tes-snv-tar          pic  x(01)                  .
               10  w-tes-lst-tar          pic  x(04)                  .
               10  w-tes-org-doc          pic  9(02)                  .
               10  w-tes-prv-doc          pic  9(02)                  .
               10  w-tes-mov-afd          pic  9(02)                  .
               10  w-tes-def-tmf          pic  x(05)                  .
               10  w-tes-vld-dpz          pic  x(01)                  .
               10  w-tes-cod-dsl          pic  x(07)                  .
               10  w-tes-cod-dsl-des      pic  x(40)                  .
               10  w-tes-def-tpr          pic  x(05)                  .
               10  w-tes-alx-gen.
                   15  filler occurs 40   pic  x(01)                  .

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
           05  w-sav-cau-mag              pic  9(05)                  .
           05  w-sav-def-tar              pic  x(01)                  .
           05  w-sav-mov-afd              pic  9(02)                  .
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
               10  w-exp-int-ftr-num      pic  9(02)       value 2    .
               10  w-exp-int-ftr-lun      pic  9(02)       value 40   .
               10  w-exp-int-ftr-tbl.
                   15  filler             pic  x(40) value
                     "Non interessa la fatturazione fornitori "       .
                   15  filler             pic  x(40) value
                     "Fatturazione differita successiva       "       .
      *        *-------------------------------------------------------*
      *        * Work per : Movimento a fronte di                      *
      *        *-------------------------------------------------------*
           05  w-exp-mov-afd.
               10  w-exp-mov-afd-num      pic  9(02)       value 2    .
               10  w-exp-mov-afd-lun      pic  9(02)       value 30   .
               10  w-exp-mov-afd-tbl.
                   15  filler             pic  x(30) value
                            "Niente                        "          .
                   15  filler             pic  x(30) value
                            "Ordine fornitore              "          .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo archivio di default                   *
      *        *-------------------------------------------------------*
           05  w-exp-def-tar.
               10  w-exp-def-tar-num      pic  9(02)       value 5    .
               10  w-exp-def-tar-lun      pic  9(02)       value 35   .
               10  w-exp-def-tar-tbl.
                   15  filler             pic  x(35) value
                            "Nessun tipo archivio proposto      "     .
                   15  filler             pic  x(35) value
                            "Non c'e tipo archivio              "     .
                   15  filler             pic  x(35) value
                            "Archivio Fornitori                 "     .
                   15  filler             pic  x(35) value
                            "Archivio Clienti                   "     .
                   15  filler             pic  x(35) value
                            "Archivio Dipendenze                "     .
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
      *        *-------------------------------------------------------*
      *        * Work per : Validita' per le dipendenze                *
      *        *-------------------------------------------------------*
           05  w-exp-vld-dpz.
               10  w-exp-vld-dpz-num      pic  9(02)       value 4    .
               10  w-exp-vld-dpz-lun      pic  9(02)       value 40   .
               10  w-exp-vld-dpz-tbl.
                   15  filler             pic  x(40) value
                            "Valido per tutte le dipendenze          ".
                   15  filler             pic  x(40) value
                            "Valido solo per la Sede                 ".
                   15  filler             pic  x(40) value
                            "Valido solo per le dipendenze           ".
                   15  filler             pic  x(40) value
                            "Valido solo per le dipendenze indicate  ".

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su tabella [ada]                         *
      *        *-------------------------------------------------------*
           05  w-let-arc-ada.
               10  w-let-arc-ada-flg      pic  x(01)                  .
               10  w-let-arc-ada-cod      pic  9(02)                  .
               10  w-let-arc-ada-des      pic  x(20)                  .
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
               10  w-wrk-lta-lst-amm      pic  x(04) value "NFCD"     .
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
      *    * Area di interfaccia per sottoprogramma         "pazi000d" *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/pazi000d.pgl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione tipo movimento bolle fornitori *
      *    *-----------------------------------------------------------*
           copy      "pgm/bfo/prg/cpy/acdeybf0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice causale di magazzino    *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/acmnzmc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice conto merce             *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/acdezmm0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice dislocazione            *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/acdezub0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice dipendenza dell'azienda *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/acoddpz0.acl"                   .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per Err su controllo tasto Do non chiave         *
      *        *-------------------------------------------------------*
           05  w-err-box-err.
               10  w-err-box-err-msg      pic  x(56)                  .

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
      *              * bolla fornitore                                 *
      *              *-------------------------------------------------*
           perform   cod-des-ybf-opn-000  thru cod-des-ybf-opn-999    .
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
      *              * Open modulo accettazione codice dislocazione    *
      *              *-------------------------------------------------*
           perform   cod-des-zub-opn-000  thru cod-des-zub-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice dipendenza del- *
      *              * l'azienda                                       *
      *              *-------------------------------------------------*
           perform   cod-cod-dpz-opn-000  thru cod-cod-dpz-opn-999    .
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
      *              * bolla fornitore                                 *
      *              *-------------------------------------------------*
           perform   cod-des-ybf-cls-000  thru cod-des-ybf-cls-999    .
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
      *              * Close modulo accettazione codice dislocazione   *
      *              *-------------------------------------------------*
           perform   cod-des-zub-cls-000  thru cod-des-zub-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice dipendenza del-*
      *              * l'azienda                                       *
      *              *-------------------------------------------------*
           perform   cod-cod-dpz-cls-000  thru cod-cod-dpz-cls-999    .
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
           call      "pgm/bfo/prg/obj/pbfo0102"
                                         using w-cat-rig              .
       cll-sub-cat-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione sottoprogramma per gestione catena righe    *
      *    *-----------------------------------------------------------*
       cnc-sub-cat-000.
           cancel    "pgm/bfo/prg/obj/pbfo0102"                       .
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
           move      "AC"                 to   w-cod-des-ybf-ope      .
           move      w-tes-cod-tmb        to   w-cod-des-ybf-cod      .
           move      04                   to   w-cod-des-ybf-lin      .
           move      30                   to   w-cod-des-ybf-pos      .
           move      06                   to   w-cod-des-ybf-dln      .
           move      30                   to   w-cod-des-ybf-dps      .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
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
      *              * Assestamento status di impostazione per corpo   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-cor      .
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
      *                  * ATTUALMENTE INIBITO                         *
      *                  *---------------------------------------------*
           go to     acc-nok-reg-800.
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
      *                      * Ad accettazione testata                 *
      *                      *                                         *
      *                      * Ad accettazione corpo (INIBITO)         *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
______*    go to     acc-nok-reg-400.
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
                     acc-tes-reg-200
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
      *                  * Interessa la fatturazione                   *
      *                  *---------------------------------------------*
           perform   acc-int-ftr-000      thru acc-int-ftr-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-105.
       acc-tes-reg-115.
      *                  *---------------------------------------------*
      *                  * Causale magazzino                           *
      *                  *---------------------------------------------*
           perform   acc-cau-mag-000      thru acc-cau-mag-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-110.
       acc-tes-reg-120.
      *                  *---------------------------------------------*
      *                  * Codice c/merce                              *
      *                  *---------------------------------------------*
           perform   acc-cod-mic-000      thru acc-cod-mic-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-115.
       acc-tes-reg-125.
      *                  *---------------------------------------------*
      *                  * Default tipo archivio                       *
      *                  *---------------------------------------------*
           perform   acc-def-tar-000      thru acc-def-tar-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-120.
       acc-tes-reg-130.
      *                  *---------------------------------------------*
      *                  * Tipo archivio modificabile                  *
      *                  *---------------------------------------------*
           perform   acc-snv-tar-000      thru acc-snv-tar-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-125.
       acc-tes-reg-135.
      *                  *---------------------------------------------*
      *                  * Tipi archivio ammessi                       *
      *                  *---------------------------------------------*
           perform   acc-lst-tar-000      thru acc-lst-tar-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-130.
       acc-tes-reg-140.
      *                  *---------------------------------------------*
      *                  * Origine del documento                       *
      *                  *---------------------------------------------*
           perform   acc-org-doc-000      thru acc-org-doc-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-135.
       acc-tes-reg-145.
      *                  *---------------------------------------------*
      *                  * Movimento a fronte di                       *
      *                  *---------------------------------------------*
           perform   acc-mov-afd-000      thru acc-mov-afd-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-140.
       acc-tes-reg-150.
      *                  *---------------------------------------------*
      *                  * Default tipo movimento a fronte             *
      *                  *---------------------------------------------*
           perform   acc-def-tmf-000      thru acc-def-tmf-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-145.
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
                     go to acc-tes-reg-150.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
           go to     acc-tes-reg-999.
       acc-tes-reg-200.
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
                     move  "-"            to   w-cnt-tus-acc-tes
                     go to acc-tes-reg-999.
       acc-tes-reg-205.
      *                  *---------------------------------------------*
      *                  * Codice dislocazione da proporre             *
      *                  *---------------------------------------------*
           perform   acc-cod-dsp-000      thru acc-cod-dsp-999        .
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
                     go to acc-tes-reg-205.
      *                  *---------------------------------------------*
      *                  * Presa visione per pagina 2                  *
      *                  *---------------------------------------------*
______*    perform   acc-pre-vpg-000      thru acc-pre-vpg-999        .
______*    if        v-key                =    "NXSC"
______*              move  "+"            to   w-cnt-tus-acc-tes      .
______*    if        v-key                =    "PRSC"
______*              move  "-"            to   w-cnt-tus-acc-tes      .
______*    if        w-cnt-tus-acc-tes    not  = spaces
______*              go to acc-tes-reg-999.
______*    if        v-key                =    "UP  "
______*              go to acc-tes-reg-205.
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
      *              * Interessa la fatturazione                       *
      *              *-------------------------------------------------*
           perform   vis-int-ftr-000      thru vis-int-ftr-999        .
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
      *              *-------------------------------------------------*
      *              * Origine del documento                           *
      *              *-------------------------------------------------*
           perform   vis-org-doc-000      thru vis-org-doc-999        .
      *              *-------------------------------------------------*
      *              * Movimento a fronte di                           *
      *              *-------------------------------------------------*
           perform   vis-mov-afd-000      thru vis-mov-afd-999        .
      *              *-------------------------------------------------*
      *              * Default tipo movimento a fronte                 *
      *              *-------------------------------------------------*
           perform   vis-def-tmf-000      thru vis-def-tmf-999        .
           go to     vis-tes-reg-999.
       vis-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Validita' per le dipendenze                     *
      *              *-------------------------------------------------*
           perform   vis-vld-dpz-000      thru vis-vld-dpz-999        .
      *              *-------------------------------------------------*
      *              * Codice dislocazione da proporre                 *
      *              *-------------------------------------------------*
           perform   vis-cod-dsp-000      thru vis-cod-dsp-999        .
           perform   vis-cod-dsp-des-000  thru vis-cod-dsp-des-999    .
      *              *-------------------------------------------------*
      *              * Default per accettazione tipo riga corpo        *
      *              *-------------------------------------------------*
           perform   vis-def-tpr-000      thru vis-def-tpr-999        .
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
      *              * Interessa la fatturazione                       *
      *              *-------------------------------------------------*
           perform   pmt-int-ftr-000      thru pmt-int-ftr-999        .
      *              *-------------------------------------------------*
      *              * Causale magazzino                               *
      *              *-------------------------------------------------*
           perform   pmt-cau-mag-000      thru pmt-cau-mag-999        .
      *              *-------------------------------------------------*
      *              * Codice c/merce                                  *
      *              *-------------------------------------------------*
           perform   pmt-cod-mic-000      thru pmt-cod-mic-999        .
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
      *              *-------------------------------------------------*
      *              * Origine del documento                           *
      *              *-------------------------------------------------*
           perform   pmt-org-doc-000      thru pmt-org-doc-999        .
      *              *-------------------------------------------------*
      *              * Movimento a fronte di                           *
      *              *-------------------------------------------------*
           perform   pmt-mov-afd-000      thru pmt-mov-afd-999        .
      *              *-------------------------------------------------*
      *              * Default tipo movimento a fronte                 *
      *              *-------------------------------------------------*
           perform   pmt-def-tmf-000      thru pmt-def-tmf-999        .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Validita' per le dipendenze                     *
      *              *-------------------------------------------------*
           perform   pmt-vld-dpz-000      thru pmt-vld-dpz-999        .
      *              *-------------------------------------------------*
      *              * Codice dislocazione proposto                    *
      *              *-------------------------------------------------*
           perform   pmt-cod-dsp-000      thru pmt-cod-dsp-999        .
      *              *-------------------------------------------------*
      *              * Default per accettazione tipo riga corpo        *
      *              *-------------------------------------------------*
           perform   pmt-def-tpr-000      thru pmt-def-tpr-999        .
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
       pmt-pwd-tmb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Interessa la fatturazione        *
      *    *-----------------------------------------------------------*
       pmt-int-ftr-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Modalita' di fatturazione  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-int-ftr-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Causale magazzino                *
      *    *-----------------------------------------------------------*
       pmt-cau-mag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
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
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice c/merce magazzino   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-mic-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Default tipo archivio            *
      *    *-----------------------------------------------------------*
       pmt-def-tar-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
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
           move      14                   to   v-lin                  .
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
           move      15                   to   v-lin                  .
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
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Origine del documento      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-org-doc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Movimento a fronte di            *
      *    *-----------------------------------------------------------*
       pmt-mov-afd-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
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
           move      20                   to   v-lin                  .
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
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Validita' per le dipendenze:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-vld-dpz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice dislocazione proposto     *
      *    *-----------------------------------------------------------*
       pmt-cod-dsp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Ubicazione di entrata      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-dsp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Default per accettazione tipo    *
      *    *                          riga corpo                       *
      *    *-----------------------------------------------------------*
       pmt-def-tpr-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo riga corpo proposto   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-def-tpr-999.
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
       acc-des-tmb-450.
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
      *    * Visualizzazione campo : Descrizione tipo movimento        *
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
      *    * Accettazione campo testata : Password per tipo movimento  *
      *    *-----------------------------------------------------------*
       acc-pwd-tmb-000.
       acc-pwd-tmb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Password per tipo movim.  *
      *    *-----------------------------------------------------------*
       vis-pwd-tmb-000.
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
           move      spaces               to   v-edm                  .
           move      08                   to   v-lin                  .
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
      *                      * Se il movimento interessa la fattura-   *
      *                      * zione                                   *
      *                      *-----------------------------------------*
           if        w-tes-int-ftr (1)    not  = 02
                     go to acc-int-ftr-800.
      *                          *-------------------------------------*
      *                          * Trattamento dati relativi all'ar-   *
      *                          * chivio                              *
      *                          *-------------------------------------*
           move      "F"                  to   w-tes-def-tar (1)      .
           move      "N"                  to   w-tes-snv-tar (1)      .
           move      "F"                  to   w-tes-lst-tar (1)      .
           perform   vis-def-tar-000      thru vis-def-tar-999        .
           perform   vis-snv-tar-000      thru vis-snv-tar-999        .
           perform   vis-lst-tar-000      thru vis-lst-tar-999        .
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
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-int-ftr-tbl    to   v-txt                  .
           move      w-tes-int-ftr (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-int-ftr-999.
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
           move      10                   to   w-cod-mne-zmc-lin      .
           move      30                   to   w-cod-mne-zmc-pos      .
           move      10                   to   w-cod-mne-zmc-dln      .
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
      *                  * Controllo che non sia una causale di solo   *
      *                  * valore                                      *
      *                  *---------------------------------------------*
           if        w-let-arc-zmc-trv    =    "C" or
                     w-let-arc-zmc-trv    =    "X" or
                     w-let-arc-zmc-trv    =    "R" or
                     w-let-arc-zmc-trv    =    "Y"
                     go to acc-cau-mag-500.
      *                  *---------------------------------------------*
      *                  * Controllo che sia una causale di carico     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-let-arc-zmc-mdm    =    01
                     go to acc-cau-mag-440.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "La causale dev'essere di carico !"
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A controllo successivo                  *
      *                      *-----------------------------------------*
           go to     acc-cau-mag-440.
       acc-cau-mag-440.
      *                  *---------------------------------------------*
      *                  * Se documento che interessa la fatturazione, *
      *                  * controllo che tipo archivio sia 'F'         *
      *                  *---------------------------------------------*
           if        w-tes-int-ftr (1)    =    01
                     go to acc-cau-mag-480.
      *                      *-----------------------------------------*
      *                      * Se il tipo archivio di default non e'   *
      *                      * variabile                               *
      *                      *-----------------------------------------*
           if        w-let-arc-zmc-vaa    =    "N"
                     if    w-let-arc-zmc-dfa
                                          =    "F"
                           go to acc-cau-mag-480
                     else  go to acc-cau-mag-500.
      *                      *-----------------------------------------*
      *                      * Se il tipo archivio di default e' varia-*
      *                      * bile                                    *
      *                      *-----------------------------------------*
           if        w-let-arc-zmc-dfa    =    "F"
                     go to acc-cau-mag-480.
           if        w-let-arc-zmc-lsa    =    spaces
                     go to acc-cau-mag-480.
           move      zero                 to   w-acc-cau-mag-ctr      .
           inspect   w-let-arc-zmc-lsa
                                      tallying w-acc-cau-mag-ctr
                                      for all  "F"                    .
           if        w-acc-cau-mag-ctr    =    zero
                     go to acc-cau-mag-500.
       acc-cau-mag-480.
      *                  *---------------------------------------------*
      *                  * Controlli superati                          *
      *                  *---------------------------------------------*
           go to     acc-cau-mag-600.
       acc-cau-mag-500.
      *                  *---------------------------------------------*
      *                  * Controlli non superati                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Causale di magazzino incompatibile !"
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-cau-mag-100.
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
       acc-cau-mag-700.
      *                  *---------------------------------------------*
      *                  * Trattamento tipo archivio                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il tipo movimento interessa la fat-  *
      *                      * turazione i dati relativi all'archivio  *
      *                      * sono gia' stati forzati                 *
      *                      *-----------------------------------------*
           if        w-tes-int-ftr (1)    =    02
                     go to acc-cau-mag-800.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione valori per tipo archivio*
      *                      *-----------------------------------------*
           move      w-let-arc-zmc-dfa    to   w-tes-def-tar (1)      .
           move      w-let-arc-zmc-vaa    to   w-tes-snv-tar (1)      .
           move      w-let-arc-zmc-lsa    to   w-tes-lst-tar (1)      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione dati                    *
      *                      *-----------------------------------------*
           perform   vis-def-tar-000      thru vis-def-tar-999        .
           perform   vis-snv-tar-000      thru vis-snv-tar-999        .
           perform   vis-lst-tar-000      thru vis-lst-tar-999        .
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
           move      10                   to   v-lin                  .
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
           move      10                   to   v-lin                  .
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
           move      11                   to   w-cod-des-zmm-lin      .
           move      30                   to   w-cod-des-zmm-pos      .
           move      11                   to   w-cod-des-zmm-dln      .
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
           move      11                   to   v-lin                  .
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
           move      11                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-tes-cod-mic-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-mic-des-999.
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
           if        w-tes-int-ftr (1)    =    02
                     go to acc-def-tar-999.
           if        w-tes-cau-mag (1)    not  = zero
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
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-def-tar-tbl    to   v-txt                  .
           if        w-tes-def-tar (1)    =    spaces
                     move  01             to   v-num
           else if   w-tes-def-tar (1)    =    "N"
                     move  02             to   v-num
           else if   w-tes-def-tar (1)    =    "F"
                     move  03             to   v-num
           else if   w-tes-def-tar (1)    =    "C"
                     move  04             to   v-num
           else if   w-tes-def-tar (1)    =    "D"
                     move  05             to   v-num
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
                     move  "F"            to   w-tes-def-tar (1)
           else if   v-num                =    04
                     move  "C"            to   w-tes-def-tar (1)
           else if   v-num                =    05
                     move  "D"            to   w-tes-def-tar (1)
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
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-def-tar-tbl    to   v-txt                  .
           if        w-tes-def-tar (1)    =    spaces
                     move  01             to   v-num
           else if   w-tes-def-tar (1)    =    "N"
                     move  02             to   v-num
           else if   w-tes-def-tar (1)    =    "F"
                     move  03             to   v-num
           else if   w-tes-def-tar (1)    =    "C"
                     move  04             to   v-num
           else if   w-tes-def-tar (1)    =    "D"
                     move  05             to   v-num
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
           if        w-tes-int-ftr (1)    =    02
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
           move      spaces               to   v-edm                  .
           move      14                   to   v-lin                  .
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
           move      14                   to   v-lin                  .
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
           if        w-tes-int-ftr (1)    =    02
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
           move      15                   to   v-lin                  .
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
           move      15                   to   v-lin                  .
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
           move      spaces               to   v-edm                  .
           move      17                   to   v-lin                  .
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
           move      17                   to   v-lin                  .
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
           move      spaces               to   v-edm                  .
           move      19                   to   v-lin                  .
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
                     go to acc-mov-afd-600.
           if        v-key                =    "UP  "
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
           move      19                   to   v-lin                  .
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
           move      20                   to   v-lin                  .
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
           move      20                   to   v-lin                  .
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
           move      spaces               to   v-edm                  .
           move      07                   to   v-lin                  .
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
           move      07                   to   v-lin                  .
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
      *    * Accettazione campo testata : Dislocazione da proporre     *
      *    *-----------------------------------------------------------*
       acc-cod-dsp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-dsp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-zub-ope      .
           move      01                   to   w-cod-des-zub-dpz      .
           move      w-tes-cod-dsl (1)    to   w-cod-des-zub-cod      .
           move      09                   to   w-cod-des-zub-lin      .
           move      30                   to   w-cod-des-zub-pos      .
           move      09                   to   w-cod-des-zub-dln      .
           move      41                   to   w-cod-des-zub-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           perform   cod-des-zub-cll-000  thru cod-des-zub-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-zub-foi-000  thru cod-des-zub-foi-999    .
       acc-cod-dsp-110.
           perform   cod-des-zub-cll-000  thru cod-des-zub-cll-999    .
           if        w-cod-des-zub-ope    =    "F+"
                     go to acc-cod-dsp-115.
           if        w-cod-des-zub-ope    =    "AC"
                     go to acc-cod-dsp-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-dsp-115.
           perform   cod-des-zub-foi-000  thru cod-des-zub-foi-999    .
           go to     acc-cod-dsp-110.
       acc-cod-dsp-120.
           move      w-cod-des-zub-cod    to   v-alf                  .
       acc-cod-dsp-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-dsp-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-dsp-999.
      *              *-------------------------------------------------*
      *              * Valore impostato                                *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-cod-dsl (1)      .
       acc-cod-dsp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella [zub]                       *
      *                  *---------------------------------------------*
           move      01                   to   w-let-arc-zub-dpz      .
           move      w-tes-cod-dsl (1)    to   w-let-arc-zub-cod      .
           perform   let-arc-zub-000      thru let-arc-zub-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zub-des    to   w-tes-cod-dsl-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-cod-dsp-des-000  thru vis-cod-dsp-des-999    .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : reimpostazione          *
      *                  *---------------------------------------------*
           if        w-let-arc-zub-flg    not  = spaces
                     go to acc-cod-dsp-100.
       acc-cod-dsp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-dsp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-dsp-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-dsp-100.
       acc-cod-dsp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Dislocazione da proporre          *
      *    *-----------------------------------------------------------*
       vis-cod-dsp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-dsl (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-dsp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Descrizione codice dislocazione   *
      *    *-----------------------------------------------------------*
       vis-cod-dsp-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-dsl-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-dsp-des-999.
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
           move      11                   to   v-lin                  .
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
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-def-tpr (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-def-tpr-999.
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
      *                  * Accettazione codice dislocazione            *
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
      *                      * Editing codice dislocazione             *
      *                      *-----------------------------------------*
           move      w-rig-cod-dsl (1)    to   w-lin-imm-cod-dsl      .
      *                      *-----------------------------------------*
      *                      * Editing descrizione dislocazione        *
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
           move      16                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo movimento :"   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Codice                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      18                   to   v-pos                  .
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
           move      25                   to   v-pos                  .
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
           move      "N.R          Dipendenza                 Dislocazio
      -              "ne di origine"
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
      *              * Campo di accettazione corpo : Dislocazione      *
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
      *                  * Dislocazione                                *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Dislocazione      :"
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
      *                  * Lettura tabella [ada]                       *
      *                  *---------------------------------------------*
           move      w-rig-cod-dpz (1)    to   w-let-arc-ada-cod      .
           perform   let-arc-ada-000      thru let-arc-ada-999        .
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
      *    * Accettazione ennesimo campo riga corpo espansa : Codice   *
      *    * dislocazione                                              *
      *    *-----------------------------------------------------------*
       acc-cod-dsl-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campo attualmente non gestito               *
      *                  *---------------------------------------------*
           go to     acc-cod-dsl-999.
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
      *              * Test su : codice c/merce                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cau-mag (1)    =    zero
                     go to cnt-tdo-nok-200.
           if        w-tes-cau-mag-ttc (1)
                                          =    01 or
                     w-tes-cau-mag-ttc (1)
                                          =    02
                     go to cnt-tdo-nok-200.
      *                  *---------------------------------------------*
      *                  * Controllo                                   *
      *                  *---------------------------------------------*
           if        w-tes-cod-mic (1)    not  = spaces
                     go to cnt-tdo-nok-200.
           move      "Manca il codice c/merce !"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-200.
      *              *-------------------------------------------------*
      *              * Test su : tipo archivio                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo movimento valido per fatturazione   *
      *                  *---------------------------------------------*
           if        w-tes-int-ftr (1)    =    01
                     go to cnt-tdo-nok-300.
           if        w-tes-def-tar (1)    =    "F" and
                     w-tes-snv-tar (1)    =    "N" and
                     w-tes-lst-tar (1)    =    "F"
                     go to cnt-tdo-nok-300.
           move      "Dati relativi all'archivio errati per la fatturazi
      -              "one !"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-300.
      *              *-------------------------------------------------*
      *              * Test su : origine del documento                 *
      *              *-------------------------------------------------*
           if        w-tes-org-doc (1)    not  = zero
                     go to cnt-tdo-nok-400.
           move      "Manca il tipo origine del documento"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-400.
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
           if        w-tes-mov-afd (1)    not  = 02 and
                     w-tes-mov-afd (1)    not  = 03
                     go to cnt-tdo-nok-500.
      *                  *---------------------------------------------*
      *                  * Controllo che nella lista archivi sia pre-  *
      *                  * sente il tipo 'F'                           *
      *                  *---------------------------------------------*
           move      zero                 to   w-wrk-lta-inx-001      .
           inspect   w-tes-lst-tar (1)
                                      tallying w-wrk-lta-inx-001
                     for                  all  "F"                    .
           if        w-wrk-lta-inx-001    >    zero
                     go to cnt-tdo-nok-500.
           move      "Movimento a fronte incompatibile con tipo archivio
      -              " !"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-500.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Validita' per le dipendenze                 *
      *                  *---------------------------------------------*
           if        w-dpz-ctr-dpz        >    1
                     go to cnt-tdo-nok-600.
      *                      *-----------------------------------------*
      *                      * Valore forzato                          *
      *                      *-----------------------------------------*
           move      spaces               to   w-tes-vld-dpz (1)      .
       cnt-tdo-nok-600.
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
      *              *-------------------------------------------------*
      *              * Inizializzazione catena [rig]                   *
      *              *-------------------------------------------------*
           move      "BE"                 to   w-cat-rig-ope          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
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
           move      spaces               to   w-tes-pwd-tmb (1)      .
           move      zero                 to   w-tes-int-ftr (1)      .
           move      spaces               to   w-tes-tmo-ftr (1)      .
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
           move      spaces               to   w-tes-def-tar (1)      .
           move      spaces               to   w-tes-snv-tar (1)      .
           move      spaces               to   w-tes-lst-tar (1)      .
           move      zero                 to   w-tes-org-doc (1)      .
           move      zero                 to   w-tes-prv-doc (1)      .
           move      zero                 to   w-tes-mov-afd (1)      .
           move      spaces               to   w-tes-def-tmf (1)      .
           move      spaces               to   w-tes-vld-dpz (1)      .
           move      spaces               to   w-tes-cod-dsl (1)      .
           move      spaces               to   w-tes-cod-dsl-des (1)  .
           move      spaces               to   w-tes-def-tpr (1)      .
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
      *              * Lettura archivio [ybf]                          *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODTMB    "         to   f-key                  .
           move      w-tes-cod-tmb        to   rf-ybf-cod-tmb         .
           move      zero                 to   rf-ybf-cod-dpz         .
           move      "pgm/bfo/fls/ioc/obj/iofybf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ybf                 .
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
                     go to rou-let-reg-055.
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
       rou-let-reg-055.
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
      *                      * Determinazione valori attuali testata   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Valori contenuti direttamente da    *
      *                          * record [ybf]                        *
      *                          *-------------------------------------*
           move      rf-ybf-des-tmb       to   w-tes-des-tmb (1)      .
           move      rf-ybf-des-key       to   w-tes-des-key (1)      .
           move      rf-ybf-pwd-tmb       to   w-tes-pwd-tmb (1)      .
           move      rf-ybf-int-ftr       to   w-tes-int-ftr (1)      .
           move      rf-ybf-tmo-ftr       to   w-tes-tmo-ftr (1)      .
           move      rf-ybf-cau-mag       to   w-tes-cau-mag (1)      .
           move      rf-ybf-cod-mic       to   w-tes-cod-mic (1)      .
           move      rf-ybf-def-tar       to   w-tes-def-tar (1)      .
           move      rf-ybf-snv-tar       to   w-tes-snv-tar (1)      .
           move      rf-ybf-lst-tar       to   w-tes-lst-tar (1)      .
           move      rf-ybf-org-doc       to   w-tes-org-doc (1)      .
           move      rf-ybf-prv-doc       to   w-tes-prv-doc (1)      .
           move      rf-ybf-mov-afd       to   w-tes-mov-afd (1)      .
           move      rf-ybf-def-tmf       to   w-tes-def-tmf (1)      .
           move      rf-ybf-vld-dpz       to   w-tes-vld-dpz (1)      .
           move      rf-ybf-cod-dsl       to   w-tes-cod-dsl (1)      .
           move      rf-ybf-def-tpr       to   w-tes-def-tpr (1)      .
           move      rf-ybf-alx-gen       to   w-tes-alx-gen (1)      .
      *                          *-------------------------------------*
      *                          * Valori contenuti indirettamente in  *
      *                          * record [ybf]                        *
      *                          *-------------------------------------*
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
      *                              * Lettura tabella [zub]           *
      *                              *---------------------------------*
           move      01                   to   w-let-arc-zub-dpz      .
           move      w-tes-cod-dsl (1)    to   w-let-arc-zub-cod      .
           perform   let-arc-zub-000      thru let-arc-zub-999        .
           move      w-let-arc-zub-des    to   w-tes-cod-dsl-des (1)  .
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
           move      "NL"                 to   f-cfr                  .
           move      "CODTMB    "         to   f-key                  .
           move      w-tes-cod-tmb        to   rf-ybf-cod-tmb         .
           move      zero                 to   rf-ybf-cod-dpz         .
           move      "pgm/bfo/fls/ioc/obj/iofybf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ybf                 .
      *                      *-----------------------------------------*
      *                      * Se errore di start : fine lettura       *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to  rou-let-reg-800.
       rou-let-reg-600.
      *                  *---------------------------------------------*
      *                  * Lettura sequenziale file [ybf]              *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofybf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ybf                 .
      *                      *-----------------------------------------*
      *                      * Se at end : fine lettura                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to  rou-let-reg-800.
      *                  *---------------------------------------------*
      *                  * Se oltre il massimo : fine lettura          *
      *                  *---------------------------------------------*
           if        rf-ybf-cod-tmb       not  = w-tes-cod-tmb
                     go to rou-let-reg-800.
      *                  *---------------------------------------------*
      *                  * Se record con codice dipendenza a zero : ri-*
      *                  * ciclo in lettura                            *
      *                  *---------------------------------------------*
           if        rf-ybf-cod-dpz       =    zero
                     go to rou-let-reg-600.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione riga                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Valori attuali                          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Valori contenuti direttamente in    *
      *                          * record [ybf]                        *
      *                          *-------------------------------------*
           move      rf-ybf-cod-dpz       to   w-rig-cod-dpz (1)      .
           move      rf-ybf-cod-dsl       to   w-rig-cod-dsl (1)      .
           move      rf-ybf-alx-dpz       to   w-rig-alx-dpz (1)      .
      *                          *-------------------------------------*
      *                          * Valori contenuti indirettamente in  *
      *                          * record [ybf]                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura tabella [ada]           *
      *                              *---------------------------------*
           move      w-rig-cod-dpz (1)    to   w-let-arc-ada-cod      .
           perform   let-arc-ada-000      thru let-arc-ada-999        .
           move      w-let-arc-ada-des    to   w-rig-cod-dpz-des (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [zub]          *
      *                              *---------------------------------*
           move      w-rig-cod-dpz (1)    to   w-let-arc-zub-dpz      .
           move      w-rig-cod-dsl (1)    to   w-let-arc-zub-cod      .
           perform   let-arc-zub-000      thru let-arc-zub-999        .
           move      w-let-arc-zub-des    to   w-rig-cod-dsl-des (1)  .
       rou-let-reg-700.
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
      *                  * Riciclo a lettura [ybf] successivo          *
      *                  *---------------------------------------------*
           go to     rou-let-reg-600.
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
      *              * Delete [ybf] relativo alla dipendenza           *
      *              *-------------------------------------------------*
           perform   del-rec-ybd-000      thru del-rec-ybd-999        .
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
      *              * Trattamento file [ybf] dipendenza               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-500.
       scr-mov-fil-400.
      *                      *-----------------------------------------*
      *                      * Write record [ybf] relativo alla dipen- *
      *                      * denza                                   *
      *                      *-----------------------------------------*
           perform   wrt-rec-ybd-000      thru wrt-rec-ybd-999        .
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
      *                          * Rewrite [ybf] relativo alla dipen-  *
      *                          * denza                               *
      *                          *-------------------------------------*
           perform   rew-rec-ybd-000      thru rew-rec-ybd-999        .
      *                          *-------------------------------------*
      *                          * Riciclo a lettura riga corpo suc-   *
      *                          * cessiva                             *
      *                          *-------------------------------------*
           go to     scr-mov-fil-300.
       scr-mov-fil-700.
      *              *-------------------------------------------------*
      *              * Trattamento file [ybf] generale                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-800.
      *                      *-----------------------------------------*
      *                      * Write record [ybf] generale             *
      *                      *-----------------------------------------*
           perform   wrt-rec-ybg-000      thru wrt-rec-ybg-999        .
           go to     scr-mov-fil-999.
       scr-mov-fil-800.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rewrite record [ybf] generale           *
      *                      *-----------------------------------------*
           perform   rew-rec-ybg-000      thru rew-rec-ybg-999        .
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
      *                  * Delete [ybf] relativo alla dipendenza       *
      *                  *---------------------------------------------*
           perform   del-rec-ybd-000      thru del-rec-ybd-999        .
      *                  *---------------------------------------------*
      *                  * Riciclo a lettura riga corpo successiva     *
      *                  *---------------------------------------------*
           go to     del-mov-fil-100.
       del-mov-fil-500.
      *              *-------------------------------------------------*
      *              * Cancellazione file [ybf] generale               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Delete record [ybf] generale                *
      *                  *---------------------------------------------*
           perform   del-rec-ybg-000      thru del-rec-ybg-999        .
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [ybf] relativo alla dipendenza        *
      *    *-----------------------------------------------------------*
       cmp-rec-ybd-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofybf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ybf                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           move      w-tes-cod-tmb        to   rf-ybf-cod-tmb         .
           move      w-rig-cod-dpz (1)    to   rf-ybf-cod-dpz         .
           move      w-rig-cod-dsl (1)    to   rf-ybf-cod-dsl         .
           move      w-rig-alx-dpz (1)    to   rf-ybf-alx-dpz         .
       cmp-rec-ybd-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [ybf] relativo alla dipendenza           *
      *    *-----------------------------------------------------------*
       wrt-rec-ybd-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-ybd-000      thru cmp-rec-ybd-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofybf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ybf                 .
       wrt-rec-ybd-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record righe [ybf] relativo alla dipendenza   *
      *    *-----------------------------------------------------------*
       rew-rec-ybd-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-ybd-000      thru cmp-rec-ybd-999        .
      *              *-------------------------------------------------*
      *              * Force Put record                                *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofybf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ybf                 .
       rew-rec-ybd-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [ybf] relativo alla dipendenza       *
      *    *-----------------------------------------------------------*
       del-rec-ybd-000.
      *              *-------------------------------------------------*
      *              * Composizione chiave primaria                    *
      *              *-------------------------------------------------*
           move      w-tes-cod-tmb        to   rf-ybf-cod-tmb         .
           move      w-rig-cod-dpz (2)    to   rf-ybf-cod-dpz         .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofybf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ybf                 .
       del-rec-ybd-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [ybf] generale                        *
      *    *-----------------------------------------------------------*
       cmp-rec-ybg-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofybf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ybf                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           move      w-tes-cod-tmb        to   rf-ybf-cod-tmb         .
           move      zero                 to   rf-ybf-cod-dpz         .
           move      w-tes-des-tmb (1)    to   rf-ybf-des-tmb         .
           move      w-tes-des-key (1)    to   rf-ybf-des-key         .
           move      w-tes-pwd-tmb (1)    to   rf-ybf-pwd-tmb         .
           move      w-tes-int-ftr (1)    to   rf-ybf-int-ftr         .
           move      w-tes-tmo-ftr (1)    to   rf-ybf-tmo-ftr         .
           move      w-tes-cau-mag (1)    to   rf-ybf-cau-mag         .
           move      w-tes-cod-mic (1)    to   rf-ybf-cod-mic         .
           move      zero                 to   rf-ybf-cam-agg         .
           move      w-tes-def-tar (1)    to   rf-ybf-def-tar         .
           move      w-tes-snv-tar (1)    to   rf-ybf-snv-tar         .
           move      w-tes-lst-tar (1)    to   rf-ybf-lst-tar         .
           move      w-tes-org-doc (1)    to   rf-ybf-org-doc         .
           move      w-tes-prv-doc (1)    to   rf-ybf-prv-doc         .
           move      w-tes-mov-afd (1)    to   rf-ybf-mov-afd         .
           move      w-tes-def-tmf (1)    to   rf-ybf-def-tmf         .
           move      w-tes-vld-dpz (1)    to   rf-ybf-vld-dpz         .
           move      w-tes-cod-dsl (1)    to   rf-ybf-cod-dsl         .
           move      w-tes-def-tpr (1)    to   rf-ybf-def-tpr         .
           move      w-tes-alx-gen (1)    to   rf-ybf-alx-gen         .
       cmp-rec-ybg-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [ybf] generale                           *
      *    *-----------------------------------------------------------*
       wrt-rec-ybg-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-ybg-000      thru cmp-rec-ybg-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofybf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ybf                 .
       wrt-rec-ybg-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [ybf] generale                         *
      *    *-----------------------------------------------------------*
       rew-rec-ybg-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-ybg-000      thru cmp-rec-ybg-999        .
      *              *-------------------------------------------------*
      *              * Force Put record                                *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofybf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ybf                 .
       rew-rec-ybg-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [ybf] generale                       *
      *    *-----------------------------------------------------------*
       del-rec-ybg-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-ybg-000      thru cmp-rec-ybg-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofybf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ybf                 .
       del-rec-ybg-999.
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
      *    * Routine di lettura tabella [ada]                          *
      *    *-----------------------------------------------------------*
       let-arc-ada-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-ada-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-ada-cod    =    zero
                     go to let-arc-ada-500.
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
           if        f-sts                not  = e-not-err
                     go to let-arc-ada-400.
       let-arc-ada-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-ada-cod-mne       to   w-let-arc-ada-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-ada-999.
       let-arc-ada-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-ada-flg      .
           move      all   "."            to   w-let-arc-ada-des      .
           go to     let-arc-ada-999.
       let-arc-ada-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-ada-des      .
       let-arc-ada-999.
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
      *    * fornitore                                                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/bfo/prg/cpy/acdeybf0.acs"                   .

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

