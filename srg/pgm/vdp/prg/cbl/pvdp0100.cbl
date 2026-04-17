       Identification Division.
       Program-Id.                                 pvdp0100           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    vdp                 *
      *                                Settore:    tab                 *
      *                                   Fase:    vdp010              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 12/10/92    *
      *                       Ultima revisione:    NdK del 30/04/10    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione archivio tipi movimento per ver-   *
      *                    samenti da produzione                       *
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
                     "vdp"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "tab"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "vdp010"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pvdp0100"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "TIPI MOVIMENTO VERSAMENTI DA PRODUZIONE "       .

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
      *        * [yvp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/vdp/fls/rec/rfyvp"                          .
      *        *-------------------------------------------------------*
      *        * [zmc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfzmc"                          .
      *        *-------------------------------------------------------*
      *        * [zmm]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfzmm"                          .
      *        *-------------------------------------------------------*
      *        * [zmd]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfzmd"                          .
      *        *-------------------------------------------------------*
      *        * [ycp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cdp/fls/rec/rfycp"                          .
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
               10  w-tes-cod-tmv          pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
               10  w-tes-des-tmv          pic  x(30)                  .
               10  w-tes-des-key          pic  x(30)                  .
               10  w-tes-pwd-tmv          pic  x(08)                  .
               10  w-tes-cau-car          pic  9(05)                  .
               10  w-tes-cau-car-des      pic  x(30)                  .
               10  w-tes-cau-car-tmc      pic  9(02)                  .
               10  w-tes-cau-car-cmc      pic  x(03)                  .
               10  w-tes-cau-car-vmc      pic  x(01)                  .
               10  w-tes-cmc-car          pic  x(03)                  .
               10  w-tes-cmc-car-des      pic  x(20)                  .
               10  w-tes-cau-sca          pic  9(05)                  .
               10  w-tes-cau-sca-des      pic  x(30)                  .
               10  w-tes-cau-sca-tmc      pic  9(02)                  .
               10  w-tes-cau-sca-cmc      pic  x(03)                  .
               10  w-tes-cau-sca-vmc      pic  x(01)                  .
               10  w-tes-cmc-sca          pic  x(03)                  .
               10  w-tes-cmc-sca-des      pic  x(20)                  .
               10  w-tes-tde-dib          pic  9(02)                  .
               10  w-tes-snx-cad          pic  x(01)                  .
               10  w-tes-cau-cad          pic  9(05)                  .
               10  w-tes-cau-cad-des      pic  x(30)                  .
               10  w-tes-cau-cad-tmc      pic  9(02)                  .
               10  w-tes-cau-cad-cmc      pic  x(03)                  .
               10  w-tes-cau-cad-vmc      pic  x(01)                  .
               10  w-tes-cmc-cad          pic  x(03)                  .
               10  w-tes-cmc-cad-des      pic  x(20)                  .
               10  w-tes-org-doc          pic  9(02)                  .
               10  w-tes-prv-doc          pic  9(02)                  .
               10  w-tes-sgl-num          pic  x(03)                  .
               10  w-tes-mov-afd          pic  9(02)                  .
               10  w-tes-chi-com          pic  x(01)                  .
               10  w-tes-obl-afd          pic  9(02)                  .
               10  w-tes-def-tmf          pic  x(05)                  .
               10  w-tes-def-tmf-des      pic  x(30)                  .
               10  w-tes-mod-tmf          pic  x(01)                  .
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
           05  w-sav-snx-cad              pic  x(01)                  .
           05  w-sav-mov-afd              pic  9(02)                  .
           05  w-sav-chi-com              pic  x(01)                  .
           05  w-sav-def-tmf              pic  x(05)                  .
           05  w-sav-cod-dpz              pic  9(02)                  .
           05  w-sav-cat-rig.
               10  filler  occurs 300     pic  x(01)                  .
           05  w-sav-rig.
               10  filler  occurs 300     pic  x(01)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo esplosione distinta base              *
      *        *-------------------------------------------------------*
           05  w-exp-tde-dib.
               10  w-exp-tde-dib-num      pic  9(02)       value 3    .
               10  w-exp-tde-dib-lun      pic  9(02)       value 30   .
               10  w-exp-tde-dib-tbl.
                   15  filler             pic  x(30) value
                            "Ad un livello                 "          .
                   15  filler             pic  x(30) value
                            "Scalare totale                "          .
                   15  filler             pic  x(30) value
                            "Scalare ai semilavorati finali"          .
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
      *        * Work per : Movimento a fronte di                      *
      *        *-------------------------------------------------------*
           05  w-exp-mov-afd.
               10  w-exp-mov-afd-num      pic  9(02)       value 2    .
               10  w-exp-mov-afd-lun      pic  9(02)       value 25   .
               10  w-exp-mov-afd-tbl.
                   15  filler             pic  x(25) value
                            "Niente                   "               .
                   15  filler             pic  x(25) value
                            "Commessa di produzione   "               .
      *        *-------------------------------------------------------*
      *        * Work per : Obbligatorieta' riferimenti commessa       *
      *        *-------------------------------------------------------*
           05  w-exp-obl-afd.
               10  w-exp-obl-afd-num      pic  9(02)       value 2    .
               10  w-exp-obl-afd-lun      pic  9(02)       value 15   .
               10  w-exp-obl-afd-tbl.
                   15  filler             pic  x(15) value
                            "Obbligatoria   "                         .
                   15  filler             pic  x(15) value
                            "Facoltativa    "                         .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [ada]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-ada.
               10  w-let-arc-ada-flg      pic  x(01)                  .
               10  w-let-arc-ada-cod      pic  9(02)                  .
               10  w-let-arc-ada-den      pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zmc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zmc.
               10  w-let-arc-zmc-flg      pic  x(01)                  .
               10  w-let-arc-zmc-cod      pic  9(05)                  .
               10  w-let-arc-zmc-des      pic  x(30)                  .
               10  w-let-arc-zmc-trv      pic  x(01)                  .
               10  w-let-arc-zmc-tip      pic  9(02)                  .
               10  w-let-arc-zmc-tmc      pic  9(02)                  .
               10  w-let-arc-zmc-tcm      pic  x(01)                  .
               10  w-let-arc-zmc-cmc      pic  x(03)                  .
               10  w-let-arc-zmc-vmc      pic  x(01)                  .
               10  w-let-arc-zmc-dta      pic  x(01)                  .
               10  w-let-arc-zmc-vta      pic  x(01)                  .
               10  w-let-arc-zmc-lta      pic  x(04)                  .
               10  w-let-arc-zmc-dtm      pic  x(01)                  .
               10  w-let-arc-zmc-vtm      pic  x(01)                  .
               10  w-let-arc-zmc-ltm      pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zmm]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zmm.
               10  w-let-arc-zmm-flg      pic  x(01)                  .
               10  w-let-arc-zmm-cod      pic  x(03)                  .
               10  w-let-arc-zmm-des      pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zmd]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zmd.
               10  w-let-arc-zmd-flg      pic  x(01)                  .
               10  w-let-arc-zmd-dpz      pic  9(02)                  .
               10  w-let-arc-zmd-cod      pic  x(07)                  .
               10  w-let-arc-zmd-des      pic  x(30)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [ycp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-ycp.
               10  w-let-arc-ycp-flg      pic  x(01)                  .
               10  w-let-arc-ycp-cod      pic  x(05)                  .
               10  w-let-arc-ycp-des      pic  x(30)                  .
               10  w-let-arc-ycp-vld      pic  9(02)                  .
               10  w-let-arc-ycp-dpz      pic  9(02)                  .
               10  w-let-arc-ycp-ord      pic  9(02)                  .
               10  w-let-arc-ycp-prd      pic  9(02)                  .
               10  w-let-arc-ycp-sgl      pic  9(02)                  .

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
      *    * Work per routine acc-cau-car-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-acc-cau-car.
           05  w-acc-cau-car-ctr          pic  9(02)                  .

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
               10  w-err-box-err-m03      pic  x(65)                  .

      *    *===========================================================*
      *    * Area di interfaccia per sottoprogramma         "pazi000d" *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/pazi000d.pgl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione tipo movimento versamenti da   *
      *    * produzione                                                *
      *    *-----------------------------------------------------------*
           copy      "pgm/vdp/prg/cpy/acdeyvp0.acl"                   .

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
           copy      "pgm/mag/prg/cpy/acdezmd0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice dipendenza dell'azienda *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/acoddpz0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione tipo movimento commesse di     *
      *    * produzione                                                *
      *    *-----------------------------------------------------------*
           copy      "pgm/cdp/prg/cpy/acdeycp0.acl"                   .

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
      *              * Open modulo accettazione tipo movimento per i   *
      *              * versamenti da produzione                        *
      *              *-------------------------------------------------*
           perform   cod-des-yvp-opn-000  thru cod-des-yvp-opn-999    .
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
           perform   cod-des-zmd-opn-000  thru cod-des-zmd-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice dipendenza del- *
      *              * l'azienda                                       *
      *              *-------------------------------------------------*
           perform   cod-cod-dpz-opn-000  thru cod-cod-dpz-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione tipo movimento per     *
      *              * commesse di produzione                          *
      *              *-------------------------------------------------*
           perform   cod-des-ycp-opn-000  thru cod-des-ycp-opn-999    .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione tipo movimento per i  *
      *              * versamenti da produzione                        *
      *              *-------------------------------------------------*
           perform   cod-des-yvp-cls-000  thru cod-des-yvp-cls-999    .
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
           perform   cod-des-zmd-cls-000  thru cod-des-zmd-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice dipendenza del-*
      *              * l'azienda                                       *
      *              *-------------------------------------------------*
           perform   cod-cod-dpz-cls-000  thru cod-cod-dpz-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione tipo movimento per    *
      *              * commesse di produzione                          *
      *              *-------------------------------------------------*
           perform   cod-des-ycp-cls-000  thru cod-des-ycp-cls-999    .
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
      *              * [yvp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/vdp/fls/ioc/obj/iofyvp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yvp                 .
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
      *              * [zmd]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzmd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zmd                 .
      *              *-------------------------------------------------*
      *              * [ycp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cdp/fls/ioc/obj/iofycp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ycp                 .
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
      *              * [yvp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/vdp/fls/ioc/obj/iofyvp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yvp                 .
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
      *              * [zmd]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzmd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zmd                 .
      *              *-------------------------------------------------*
      *              * [ycp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cdp/fls/ioc/obj/iofycp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ycp                 .
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
           call      "pgm/vdp/prg/obj/pvdp0102"
                                         using w-cat-rig              .
       cll-sub-cat-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione sottoprogramma per gestione catena righe    *
      *    *-----------------------------------------------------------*
       cnc-sub-cat-000.
           cancel    "pgm/vdp/prg/obj/pvdp0102"                       .
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
           perform   acc-cod-tmv-000      thru acc-cod-tmv-999        .
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
           perform   vis-cod-tmv-000      thru vis-cod-tmv-999        .
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
           perform   pmt-cod-tmv-000      thru pmt-cod-tmv-999        .
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
       pmt-cod-tmv-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice tipo movimento      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-tmv-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Codice tipo movimento         *
      *    *-----------------------------------------------------------*
       acc-cod-tmv-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-tmv-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-yvp-ope      .
           move      w-tes-cod-tmv        to   w-cod-des-yvp-cod      .
           move      04                   to   w-cod-des-yvp-lin      .
           move      30                   to   w-cod-des-yvp-pos      .
           move      07                   to   w-cod-des-yvp-dln      .
           move      30                   to   w-cod-des-yvp-dps      .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-des-yvp-cll-000  thru cod-des-yvp-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-yvp-foi-000  thru cod-des-yvp-foi-999    .
       acc-cod-tmv-110.
           perform   cod-des-yvp-cll-000  thru cod-des-yvp-cll-999    .
           if        w-cod-des-yvp-ope    =    "F+"
                     go to acc-cod-tmv-115.
           if        w-cod-des-yvp-ope    =    "AC"
                     go to acc-cod-tmv-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-tmv-115.
           perform   cod-des-yvp-foi-000  thru cod-des-yvp-foi-999    .
           go to     acc-cod-tmv-110.
       acc-cod-tmv-120.
           move      w-cod-des-yvp-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-tmv-999.
       acc-cod-tmv-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-cod-tmv          .
       acc-cod-tmv-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      w-tes-cod-tmv        to   w-all-str-alf          .
           move      05                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-cod-tmv-100.
       acc-cod-tmv-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-tmv-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-tmv-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-tmv-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-tmv-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-tmv-999.
       acc-cod-tmv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Codice tipo movimento      *
      *    *-----------------------------------------------------------*
       vis-cod-tmv-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-tmv        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-tmv-999.
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
      *              * La testata e' composta di nr. 2 pagina          *
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
           perform   acc-des-tmv-000      thru acc-des-tmv-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-105.
      *                  *---------------------------------------------*
      *                  * Password per il movimento                   *
      *                  *---------------------------------------------*
           perform   acc-pwd-tmv-000      thru acc-pwd-tmv-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-100.
       acc-tes-reg-110.
      *                  *---------------------------------------------*
      *                  * Causale magazzino di carico                 *
      *                  *---------------------------------------------*
           perform   acc-cau-car-000      thru acc-cau-car-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-105.
       acc-tes-reg-115.
      *                  *---------------------------------------------*
      *                  * Codice c/merce di carico                    *
      *                  *---------------------------------------------*
           perform   acc-cmc-car-000      thru acc-cmc-car-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-110.
       acc-tes-reg-120.
      *                  *---------------------------------------------*
      *                  * Causale magazzino di scarico componenti     *
      *                  *---------------------------------------------*
           perform   acc-cau-sca-000      thru acc-cau-sca-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-115.
       acc-tes-reg-125.
      *                  *---------------------------------------------*
      *                  * Codice c/merce di scarico componenti        *
      *                  *---------------------------------------------*
           perform   acc-cmc-sca-000      thru acc-cmc-sca-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-120.
       acc-tes-reg-130.
      *                  *---------------------------------------------*
      *                  * Tipo esplosione distinta base               *
      *                  *---------------------------------------------*
           perform   acc-tde-dib-000      thru acc-tde-dib-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-125.
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
                     go to acc-tes-reg-130.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
           go to     acc-tes-reg-999.
       acc-tes-reg-200.
      *                  *---------------------------------------------*
      *                  * Si/No gestione consumi addizionali          *
      *                  *---------------------------------------------*
           perform   acc-snx-cad-000      thru acc-snx-cad-999        .
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
      *                  * Causale magazzino di scarico consumi        *
      *                  *---------------------------------------------*
           perform   acc-cau-cad-000      thru acc-cau-cad-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-200.
       acc-tes-reg-215.
      *                  *---------------------------------------------*
      *                  * Codice c/merce di scarico consumi           *
      *                  *---------------------------------------------*
           perform   acc-cmc-cad-000      thru acc-cmc-cad-999        .
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
                     go to acc-tes-reg-215.
       acc-tes-reg-225.
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
                     go to acc-tes-reg-220.
       acc-tes-reg-230.
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
                     go to acc-tes-reg-225.
       acc-tes-reg-235.
      *                  *---------------------------------------------*
      *                  * Movimento di chiusura                       *
      *                  *---------------------------------------------*
           perform   acc-chi-com-000      thru acc-chi-com-999        .
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
      *                  * Obbligatorieta' movimento a fronte          *
      *                  *---------------------------------------------*
           perform   acc-obl-afd-000      thru acc-obl-afd-999        .
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
                     go to acc-tes-reg-240.
       acc-tes-reg-250.
      *                  *---------------------------------------------*
      *                  * Modificabilita' tipo movimento a fronte     *
      *                  *---------------------------------------------*
           perform   acc-mod-tmf-000      thru acc-mod-tmf-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-245.
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
                     depending            on   w-cnt-sts-imp-npt      .
           go to     vis-tes-reg-999.
       vis-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Descrizione tipo movimento                      *
      *              *-------------------------------------------------*
           perform   vis-des-tmv-000      thru vis-des-tmv-999        .
      *              *-------------------------------------------------*
      *              * Password per il movimento                       *
      *              *-------------------------------------------------*
           perform   vis-pwd-tmv-000      thru vis-pwd-tmv-999        .
      *              *-------------------------------------------------*
      *              * Causale magazzino di carico                     *
      *              *-------------------------------------------------*
           perform   vis-cau-car-000      thru vis-cau-car-999        .
           perform   vis-cau-car-des-000  thru vis-cau-car-des-999    .
      *              *-------------------------------------------------*
      *              * Codice c/merce di carico                        *
      *              *-------------------------------------------------*
           perform   vis-cmc-car-000      thru vis-cmc-car-999        .
           perform   vis-cmc-car-des-000  thru vis-cmc-car-des-999    .
      *              *-------------------------------------------------*
      *              * Causale magazzino di scarico componenti         *
      *              *-------------------------------------------------*
           perform   vis-cau-sca-000      thru vis-cau-sca-999        .
           perform   vis-cau-sca-des-000  thru vis-cau-sca-des-999    .
      *              *-------------------------------------------------*
      *              * Codice c/merce di scarico componenti            *
      *              *-------------------------------------------------*
           perform   vis-cmc-sca-000      thru vis-cmc-sca-999        .
           perform   vis-cmc-sca-des-000  thru vis-cmc-sca-des-999    .
      *              *-------------------------------------------------*
      *              * Tipo esplosione distinta base                   *
      *              *-------------------------------------------------*
           perform   vis-tde-dib-000      thru vis-tde-dib-999        .
           go to     vis-tes-reg-999.
       vis-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Si/No gestione consumi addizionali              *
      *              *-------------------------------------------------*
           perform   vis-snx-cad-000      thru vis-snx-cad-999        .
      *              *-------------------------------------------------*
      *              * Causale magazzino di scarico consumi            *
      *              *-------------------------------------------------*
           perform   vis-cau-cad-000      thru vis-cau-cad-999        .
           perform   vis-cau-cad-des-000  thru vis-cau-cad-des-999    .
      *              *-------------------------------------------------*
      *              * Codice c/merce di scarico consumi               *
      *              *-------------------------------------------------*
           perform   vis-cmc-cad-000      thru vis-cmc-cad-999        .
           perform   vis-cmc-cad-des-000  thru vis-cmc-cad-des-999    .
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
      *              * Movimento di chiusura                           *
      *              *-------------------------------------------------*
           perform   vis-chi-com-000      thru vis-chi-com-999        .
      *              *-------------------------------------------------*
      *              * Obbligatorieta' movimento a fronte              *
      *              *-------------------------------------------------*
           perform   vis-obl-afd-000      thru vis-obl-afd-999        .
      *              *-------------------------------------------------*
      *              * Default tipo movimento a fronte                 *
      *              *-------------------------------------------------*
           perform   vis-def-tmf-000      thru vis-def-tmf-999        .
           perform   vis-def-tmf-des-000  thru vis-def-tmf-des-999    .
      *              *-------------------------------------------------*
      *              * Modificabilita' tipo movimento a fronte         *
      *              *-------------------------------------------------*
           perform   vis-mod-tmf-000      thru vis-mod-tmf-999        .
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
           perform   pmt-des-tmv-000      thru pmt-des-tmv-999        .
      *              *-------------------------------------------------*
      *              * Password per il movimento                       *
      *              *-------------------------------------------------*
           perform   pmt-pwd-tmv-000      thru pmt-pwd-tmv-999        .
      *              *-------------------------------------------------*
      *              * Causale magazzino di carico                     *
      *              *-------------------------------------------------*
           perform   pmt-cau-car-000      thru pmt-cau-car-999        .
      *              *-------------------------------------------------*
      *              * Codice c/merce di carico                        *
      *              *-------------------------------------------------*
           perform   pmt-cmc-car-000      thru pmt-cmc-car-999        .
      *              *-------------------------------------------------*
      *              * Causale magazzino di scarico componenti         *
      *              *-------------------------------------------------*
           perform   pmt-cau-sca-000      thru pmt-cau-sca-999        .
      *              *-------------------------------------------------*
      *              * Codice c/merce di scarico componenti            *
      *              *-------------------------------------------------*
           perform   pmt-cmc-sca-000      thru pmt-cmc-sca-999        .
      *              *-------------------------------------------------*
      *              * Tipo esplosione distinta base                   *
      *              *-------------------------------------------------*
           perform   pmt-tde-dib-000      thru pmt-tde-dib-999        .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Si/No gestione consumi addizionali              *
      *              *-------------------------------------------------*
           perform   pmt-snx-cad-000      thru pmt-snx-cad-999        .
      *              *-------------------------------------------------*
      *              * Causale magazzino di scarico consumi            *
      *              *-------------------------------------------------*
           perform   pmt-cau-cad-000      thru pmt-cau-cad-999        .
      *              *-------------------------------------------------*
      *              * Codice c/merce di scarico consumi               *
      *              *-------------------------------------------------*
           perform   pmt-cmc-cad-000      thru pmt-cmc-cad-999        .
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
      *              * Movimento di chiusura                           *
      *              *-------------------------------------------------*
           perform   pmt-chi-com-000      thru pmt-chi-com-999        .
      *              *-------------------------------------------------*
      *              * Obbligatorieta' movimento a fronte              *
      *              *-------------------------------------------------*
           perform   pmt-obl-afd-000      thru pmt-obl-afd-999        .
      *              *-------------------------------------------------*
      *              * Default tipo movimento a fronte                 *
      *              *-------------------------------------------------*
           perform   pmt-def-tmf-000      thru pmt-def-tmf-999        .
      *              *-------------------------------------------------*
      *              * Modificabilita' tipo movimento a fronte         *
      *              *-------------------------------------------------*
           perform   pmt-mod-tmf-000      thru pmt-mod-tmf-999        .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Descrizione tipo movimento       *
      *    *-----------------------------------------------------------*
       pmt-des-tmv-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Descrizione tipo movimento :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-des-tmv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Password per il movimento        *
      *    *-----------------------------------------------------------*
       pmt-pwd-tmv-000.
       pmt-pwd-tmv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Causale magazzino di carico      *
      *    *-----------------------------------------------------------*
       pmt-cau-car-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                   Dati per carico risultato della
      -              " produzione                   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "- Codice causale magazzino :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cau-car-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice c/merce di carico         *
      *    *-----------------------------------------------------------*
       pmt-cmc-car-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "- Codice c/merce magazzino :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cmc-car-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Causale magazzino di scarico     *
      *    * componenti                                                *
      *    *-----------------------------------------------------------*
       pmt-cau-sca-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "          Dati per scarico componenti utilizzati c
      -              "ome da distinta base          "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "- Codice causale magazzino :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cau-sca-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice c/merce di scarico compo- *
      *    * nenti                                                     *
      *    *-----------------------------------------------------------*
       pmt-cmc-sca-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "- Codice c/merce magazzino :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cmc-sca-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tipo esplosione distinta base    *
      *    *-----------------------------------------------------------*
       pmt-tde-dib-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "- Tipo esplosione distinta :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                      base  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tde-dib-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/No gestione consumi addizio-  *
      *    * nali                                                      *
      *    *-----------------------------------------------------------*
       pmt-snx-cad-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "    Dati per scarico componenti addizionali utiliz
      -              "zati in fase di produzione    "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "- Gestione consumi manuali :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-cad-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Causale magazzino di scarico     *
      *    * consumi                                                   *
      *    *-----------------------------------------------------------*
       pmt-cau-cad-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "- Codice causale magazzino :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cau-cad-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice c/merce di scarico consu- *
      *    * mi                                                        *
      *    *-----------------------------------------------------------*
       pmt-cmc-cad-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "- Codice c/merce magazzino :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cmc-cad-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Origine del documento            *
      *    *-----------------------------------------------------------*
       pmt-org-doc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
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
           move      14                   to   v-lin                  .
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
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Movimento a fronte di      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-mov-afd-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Movimento di chiusura commessa   *
      *    *-----------------------------------------------------------*
       pmt-chi-com-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Movimento di pareggiamento :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-chi-com-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Obbligatorieta' movimento a      *
      *    * fronte                                                    *
      *    *-----------------------------------------------------------*
       pmt-obl-afd-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Richiesta riferimenti      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-obl-afd-999.
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
      *    * Visualizzazione prompt : Modificabilita' tipo movimento a *
      *    * fronte                                                    *
      *    *-----------------------------------------------------------*
       pmt-mod-tmf-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo movimento modificabile:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-mod-tmf-999.
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
       acc-des-tmv-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-des-tmv-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-des-tmv (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-des-tmv-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-des-tmv-999.
       acc-des-tmv-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-des-tmv (1)      .
       acc-des-tmv-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione         *
      *                  *---------------------------------------------*
           if        w-tes-des-tmv (1)    =    spaces
                     go to acc-des-tmv-100.
       acc-des-tmv-450.
      *                  *---------------------------------------------*
      *                  * Se valore a non spaces il primo carattere   *
      *                  * non deve essere a spaces                    *
      *                  *---------------------------------------------*
           if        w-tes-des-tmv (1)
                    (01 : 01)             =    spaces
                     go to acc-des-tmv-100.
       acc-des-tmv-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione descrizione in uppercase       *
      *                  *---------------------------------------------*
           move      w-tes-des-tmv (1)    to   w-all-str-alf          .
           move      30                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-tes-des-key (1)      .
       acc-des-tmv-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-des-tmv-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-des-tmv-100.
       acc-des-tmv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione tipo movimento*
      *    *-----------------------------------------------------------*
       vis-des-tmv-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-des-tmv (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-tmv-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Password per tipo movimento  *
      *    *-----------------------------------------------------------*
       acc-pwd-tmv-000.
       acc-pwd-tmv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Password per tipo movim.  *
      *    *-----------------------------------------------------------*
       vis-pwd-tmv-000.
       vis-pwd-tmv-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Causale di magazzino di ca-  *
      *    * rico                                                      *
      *    *-----------------------------------------------------------*
       acc-cau-car-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cau-car-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zmc-ope      .
           move      w-tes-cau-car (1)    to   w-cod-mne-zmc-cod      .
           move      11                   to   w-cod-mne-zmc-lin      .
           move      30                   to   w-cod-mne-zmc-pos      .
           move      11                   to   w-cod-mne-zmc-dln      .
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
       acc-cau-car-110.
           perform   cod-mne-zmc-cll-000  thru cod-mne-zmc-cll-999    .
           if        w-cod-mne-zmc-ope    =    "F+"
                     go to acc-cau-car-115.
           if        w-cod-mne-zmc-ope    =    "AC"
                     go to acc-cau-car-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cau-car-115.
           perform   cod-mne-zmc-foi-000  thru cod-mne-zmc-foi-999    .
           go to     acc-cau-car-110.
       acc-cau-car-120.
           move      w-cod-mne-zmc-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cau-car-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cau-car-999.
       acc-cau-car-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cau-car (1)      .
       acc-cau-car-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zmc]                      *
      *                  *---------------------------------------------*
           move      w-tes-cau-car (1)    to   w-let-arc-zmc-cod      .
           perform   let-arc-zmc-000      thru let-arc-zmc-999        .
      *                  *---------------------------------------------*
      *                  * Trattamento descrizione                     *
      *                  *---------------------------------------------*
           move      w-let-arc-zmc-des    to   w-tes-cau-car-des (1)  .
           perform   vis-cau-car-des-000  thru vis-cau-car-des-999    .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : reimpostazione          *
      *                  *---------------------------------------------*
           if        w-let-arc-zmc-flg    not  = spaces
                     go to acc-cau-car-100.
      *                  *---------------------------------------------*
      *                  * Se causale di magazzino a zero : si saltano *
      *                  * ulteriori controlli                         *
      *                  *---------------------------------------------*
           if        w-tes-cau-car (1)    =    zero
                     go to acc-cau-car-600.
       acc-cau-car-420.
      *                  *---------------------------------------------*
      *                  * Test su trattamento valore della causale    *
      *                  *---------------------------------------------*
           if        w-let-arc-zmc-trv    =    "N" or
                     w-let-arc-zmc-trv    =    "I" or
                     w-let-arc-zmc-trv    =    "S"
                     go to acc-cau-car-424.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Trattamento valore per la causale errato !        
      -              "     "              to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-cau-car-100.
       acc-cau-car-424.
      *                  *---------------------------------------------*
      *                  * Test su tipo movimento di magazzino         *
      *                  *---------------------------------------------*
           if        w-let-arc-zmc-tip    =    01
                     go to acc-cau-car-426.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "La causale di magazzino non e' 'di carico' !      
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                          *-------------------------------------*
      *                          * A reimpostazione                    *
      *                          *-------------------------------------*
           go to     acc-cau-car-100.
       acc-cau-car-426.
      *                  *---------------------------------------------*
      *                  * Controlli superati                          *
      *                  *---------------------------------------------*
           go to     acc-cau-car-600.
       acc-cau-car-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se causale di magazzino a zero              *
      *                  *---------------------------------------------*
           if        w-tes-cau-car (1)    not  = zero
                     go to acc-cau-car-620.
      *                      *-----------------------------------------*
      *                      * Normalizzazione c/merce                 *
      *                      *-----------------------------------------*
           if        w-tes-cmc-car (1)    =    spaces
                     go to acc-cau-car-610.
           move      spaces               to   w-tes-cmc-car (1)      .
           move      spaces               to   w-tes-cmc-car-des (1)  .
           perform   vis-cmc-car-000      thru vis-cmc-car-999        .
           perform   vis-cmc-car-des-000  thru vis-cmc-car-des-999    .
       acc-cau-car-610.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     acc-cau-car-800.
       acc-cau-car-620.
      *                  *---------------------------------------------*
      *                  * Se causale di magazzino esistente           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Bufferizzazione valori per il c/merce   *
      *                      *-----------------------------------------*
           move      w-let-arc-zmc-tmc    to   w-tes-cau-car-tmc (1)  .
           move      w-let-arc-zmc-cmc    to   w-tes-cau-car-cmc (1)  .
           move      w-let-arc-zmc-vmc    to   w-tes-cau-car-vmc (1)  .
      *                      *-----------------------------------------*
      *                      * Trattamento c/merce                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se causale non di c/merce           *
      *                          *-------------------------------------*
           if        w-tes-cau-car-tmc (1)
                                          not  = 02
                     go to acc-cau-car-670.
           if        w-tes-cmc-car (1)    =    spaces
                     go to acc-cau-car-660.
           move      spaces               to   w-tes-cmc-car (1)      .
           move      spaces               to   w-tes-cmc-car-des (1)  .
           perform   vis-cmc-car-000      thru vis-cmc-car-999        .
           perform   vis-cmc-car-des-000  thru vis-cmc-car-des-999    .
       acc-cau-car-660.
           go to     acc-cau-car-800.
       acc-cau-car-670.
      *                          *-------------------------------------*
      *                          * Se causale di c/merce               *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se codice c/merce modificabile :*
      *                              * oltre                           *
      *                              *---------------------------------*
           if        w-tes-cau-car-vmc (1)
                                          =    "S"
                     go to  acc-cau-car-800.
      *                              *---------------------------------*
      *                              * Se codice c/merce non modifica- *
      *                              * bile                            *
      *                              *---------------------------------*
           if        w-tes-cau-car-cmc (1)
                                          =    w-tes-cmc-car (1)
                     go to  acc-cau-car-800.
           move      w-tes-cau-car-cmc (1)
                                          to   w-tes-cmc-car (1)      .
           move      w-tes-cmc-car (1)    to   w-let-arc-zmm-cod      .
           perform   let-arc-zmm-000      thru let-arc-zmm-999        .
           move      w-let-arc-zmm-des    to   w-tes-cmc-car-des (1)  .
           perform   vis-cmc-car-000      thru vis-cmc-car-999        .
           perform   vis-cmc-car-des-000  thru vis-cmc-car-des-999    .
       acc-cau-car-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cau-car-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cau-car-100.
       acc-cau-car-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Causale di magazzino di   *
      *    * carico                                                    *
      *    *-----------------------------------------------------------*
       vis-cau-car-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cau-car (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cau-car-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione causale di    *
      *    *                                 magazzino di carico       *
      *    *-----------------------------------------------------------*
       vis-cau-car-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-tes-cau-car-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-cau-car-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice merce in conto di ca- *
      *    * rico                                                      *
      *    *-----------------------------------------------------------*
       acc-cmc-car-000.
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
           if        w-tes-cau-car (1)    =    zero
                     go to acc-cmc-car-999.
      *                      *-----------------------------------------*
      *                      * Test su trattamento c/merce della cau-  *
      *                      * sale di magazzino                       *
      *                      *-----------------------------------------*
           if        w-tes-cau-car-tmc (1)
                                          =    02
                     go to acc-cmc-car-999.
      *                      *-----------------------------------------*
      *                      * Se codice c/merce non variabile : uscita*
      *                      *-----------------------------------------*
           if        w-tes-cau-car-vmc (1)
                                          =    "N"
                     go to acc-cmc-car-999.
      *                  *---------------------------------------------*
      *                  * Preparazione valore di default              *
      *                  *---------------------------------------------*
           if        w-tes-cmc-car (1)    =    spaces
                     move  w-tes-cau-car-cmc (1)
                                          to   w-tes-cmc-car (1)      .
       acc-cmc-car-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-zmm-ope      .
           move      w-tes-cmc-car (1)    to   w-cod-des-zmm-cod      .
           move      12                   to   w-cod-des-zmm-lin      .
           move      30                   to   w-cod-des-zmm-pos      .
           move      12                   to   w-cod-des-zmm-dln      .
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
       acc-cmc-car-110.
           perform   cod-des-zmm-cll-000  thru cod-des-zmm-cll-999    .
           if        w-cod-des-zmm-ope    =    "F+"
                     go to acc-cmc-car-115.
           if        w-cod-des-zmm-ope    =    "AC"
                     go to acc-cmc-car-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cmc-car-115.
           perform   cod-des-zmm-foi-000  thru cod-des-zmm-foi-999    .
           go to     acc-cmc-car-110.
       acc-cmc-car-120.
           move      w-cod-des-zmm-cod    to   v-alf                  .
       acc-cmc-car-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cmc-car-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cmc-car-999.
       acc-cmc-car-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-cmc-car (1)      .
       acc-cmc-car-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella [zmm]                       *
      *                  *---------------------------------------------*
           move      w-tes-cmc-car (1)    to   w-let-arc-zmm-cod      .
           perform   let-arc-zmm-000      thru let-arc-zmm-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zmm-des    to   w-tes-cmc-car-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-cmc-car-des-000  thru vis-cmc-car-des-999    .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : reimpostazione          *
      *                  *---------------------------------------------*
           if        w-let-arc-zmm-flg    not  = spaces
                     go to acc-cmc-car-100.
      *                  *---------------------------------------------*
      *                  * Valore a spaces ammesso, purche' il tratta- *
      *                  * mento merce in conto sia :                  *
      *                  * 01 : Puo' riferirsi ad un altro documento;  *
      *                  * altrimenti reimpostazione, a meno che non si*
      *                  * sia in Up                                   *
      *                  *---------------------------------------------*
           if        w-tes-cmc-car (1)    not  = spaces
                     go to acc-cmc-car-600.
           if        w-tes-cau-car-tmc (1)
                                          =    01
                     go to acc-cmc-car-600.
           if        v-key                =    "UP  "
                     go to acc-cmc-car-600
           else      go to acc-cmc-car-100.
       acc-cmc-car-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cmc-car-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cmc-car-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cmc-car-100.
       acc-cmc-car-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice merce in conto di  *
      *    * carico                                                    *
      *    *-----------------------------------------------------------*
       vis-cmc-car-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cmc-car (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cmc-car-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione per codice    *
      *    *                                 merce in conto di carico  *
      *    *-----------------------------------------------------------*
       vis-cmc-car-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-tes-cmc-car-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cmc-car-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Causale di magazzino di sca- *
      *    * rico componenti                                           *
      *    *-----------------------------------------------------------*
       acc-cau-sca-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cau-sca-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zmc-ope      .
           move      w-tes-cau-sca (1)    to   w-cod-mne-zmc-cod      .
           move      16                   to   w-cod-mne-zmc-lin      .
           move      30                   to   w-cod-mne-zmc-pos      .
           move      16                   to   w-cod-mne-zmc-dln      .
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
       acc-cau-sca-110.
           perform   cod-mne-zmc-cll-000  thru cod-mne-zmc-cll-999    .
           if        w-cod-mne-zmc-ope    =    "F+"
                     go to acc-cau-sca-115.
           if        w-cod-mne-zmc-ope    =    "AC"
                     go to acc-cau-sca-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cau-sca-115.
           perform   cod-mne-zmc-foi-000  thru cod-mne-zmc-foi-999    .
           go to     acc-cau-sca-110.
       acc-cau-sca-120.
           move      w-cod-mne-zmc-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cau-sca-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cau-sca-999.
       acc-cau-sca-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cau-sca (1)      .
       acc-cau-sca-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zmc]                      *
      *                  *---------------------------------------------*
           move      w-tes-cau-sca (1)    to   w-let-arc-zmc-cod      .
           perform   let-arc-zmc-000      thru let-arc-zmc-999        .
      *                  *---------------------------------------------*
      *                  * Trattamento descrizione                     *
      *                  *---------------------------------------------*
           move      w-let-arc-zmc-des    to   w-tes-cau-sca-des (1)  .
           perform   vis-cau-sca-des-000  thru vis-cau-sca-des-999    .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : reimpostazione          *
      *                  *---------------------------------------------*
           if        w-let-arc-zmc-flg    not  = spaces
                     go to acc-cau-sca-100.
      *                  *---------------------------------------------*
      *                  * Se causale di magazzino a zero : si saltano *
      *                  * ulteriori controlli                         *
      *                  *---------------------------------------------*
           if        w-tes-cau-sca (1)    =    zero
                     go to acc-cau-sca-600.
       acc-cau-sca-420.
      *                  *---------------------------------------------*
      *                  * Test su trattamento valore per la causale   *
      *                  *---------------------------------------------*
           if        w-let-arc-zmc-trv    =    "N"
                     go to acc-cau-sca-424.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Trattamento valore per la causale errato !        
      -              "     "              to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-cau-sca-100.
       acc-cau-sca-424.
      *                  *---------------------------------------------*
      *                  * Test su tipo movimento di magazzino         *
      *                  *---------------------------------------------*
           if        w-let-arc-zmc-tip    =    02
                     go to acc-cau-sca-426.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "La causale di magazzino non e' 'di scarico' !     
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                          *-------------------------------------*
      *                          * A reimpostazione                    *
      *                          *-------------------------------------*
           go to     acc-cau-sca-100.
       acc-cau-sca-426.
      *                  *---------------------------------------------*
      *                  * Controlli superati                          *
      *                  *---------------------------------------------*
           go to     acc-cau-sca-600.
       acc-cau-sca-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se causale di magazzino a zero              *
      *                  *---------------------------------------------*
           if        w-tes-cau-sca (1)    not  = zero
                     go to acc-cau-sca-620.
      *                      *-----------------------------------------*
      *                      * Normalizzazione c/merce                 *
      *                      *-----------------------------------------*
           if        w-tes-cmc-sca (1)    =    spaces
                     go to acc-cau-sca-610.
           move      spaces               to   w-tes-cmc-sca (1)      .
           move      spaces               to   w-tes-cmc-sca-des (1)  .
           perform   vis-cmc-sca-000      thru vis-cmc-sca-999        .
           perform   vis-cmc-sca-des-000  thru vis-cmc-sca-des-999    .
       acc-cau-sca-610.
      *                      *-----------------------------------------*
      *                      * Normalizzazione tipo esplosione distin- *
      *                      * ta base                                 *
      *                      *-----------------------------------------*
           if        w-tes-tde-dib (1)    =    zero
                     go to acc-cau-sca-615.
           move      zero                 to   w-tes-tde-dib (1)      .
           perform   vis-tde-dib-000      thru vis-tde-dib-999        .
       acc-cau-sca-615.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     acc-cau-sca-800.
       acc-cau-sca-620.
      *                  *---------------------------------------------*
      *                  * Se causale di magazzino esistente           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Bufferizzazione valori per il c/merce   *
      *                      *-----------------------------------------*
           move      w-let-arc-zmc-tmc    to   w-tes-cau-sca-tmc (1)  .
           move      w-let-arc-zmc-cmc    to   w-tes-cau-sca-cmc (1)  .
           move      w-let-arc-zmc-vmc    to   w-tes-cau-sca-vmc (1)  .
      *                      *-----------------------------------------*
      *                      * Trattamento c/merce                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se causale non di c/merce           *
      *                          *-------------------------------------*
           if        w-tes-cau-sca-tmc (1)
                                          not  = 02
                     go to acc-cau-sca-670.
           if        w-tes-cmc-sca (1)    =    spaces
                     go to acc-cau-sca-660.
           move      spaces               to   w-tes-cmc-sca (1)      .
           move      spaces               to   w-tes-cmc-sca-des (1)  .
           perform   vis-cmc-sca-000      thru vis-cmc-sca-999        .
           perform   vis-cmc-sca-des-000  thru vis-cmc-sca-des-999    .
       acc-cau-sca-660.
           go to     acc-cau-sca-800.
       acc-cau-sca-670.
      *                          *-------------------------------------*
      *                          * Se causale di c/merce               *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se codice c/merce modificabile :*
      *                              * oltre                           *
      *                              *---------------------------------*
           if        w-tes-cau-sca-vmc (1)
                                          =    "S"
                     go to  acc-cau-sca-800.
      *                              *---------------------------------*
      *                              * Se codice c/merce non modifica- *
      *                              * bile                            *
      *                              *---------------------------------*
           if        w-tes-cau-sca-cmc (1)
                                          =    w-tes-cmc-sca (1)
                     go to  acc-cau-sca-800.
           move      w-tes-cau-sca-cmc (1)
                                          to   w-tes-cmc-sca (1)      .
           move      w-tes-cmc-sca (1)    to   w-let-arc-zmm-cod      .
           perform   let-arc-zmm-000      thru let-arc-zmm-999        .
           move      w-let-arc-zmm-des    to   w-tes-cmc-sca-des (1)  .
           perform   vis-cmc-sca-000      thru vis-cmc-sca-999        .
           perform   vis-cmc-sca-des-000  thru vis-cmc-sca-des-999    .
       acc-cau-sca-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cau-sca-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cau-sca-100.
       acc-cau-sca-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Causale di magazzino di   *
      *    * scarico componenti                                        *
      *    *-----------------------------------------------------------*
       vis-cau-sca-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cau-sca (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cau-sca-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione causale di    *
      *    *                                 magazzino di scarico com- *
      *    *                                 ponenti                   *
      *    *-----------------------------------------------------------*
       vis-cau-sca-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-tes-cau-sca-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-cau-sca-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice merce in conto di     *
      *    * scarico componenti                                        *
      *    *-----------------------------------------------------------*
       acc-cmc-sca-000.
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
           if        w-tes-cau-sca (1)    =    zero
                     go to acc-cmc-sca-999.
      *                      *-----------------------------------------*
      *                      * Test su trattamento c/merce della cau-  *
      *                      * sale di magazzino                       *
      *                      *-----------------------------------------*
           if        w-tes-cau-sca-tmc (1)
                                          =    02
                     go to acc-cmc-sca-999.
      *                      *-----------------------------------------*
      *                      * Se codice c/merce non variabile : uscita*
      *                      *-----------------------------------------*
           if        w-tes-cau-sca-vmc (1)
                                          =    "N"
                     go to acc-cmc-sca-999.
      *                  *---------------------------------------------*
      *                  * Preparazione valore di default              *
      *                  *---------------------------------------------*
           if        w-tes-cmc-sca (1)    =    spaces
                     move  w-tes-cau-sca-cmc (1)
                                          to   w-tes-cmc-sca (1)      .
       acc-cmc-sca-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-zmm-ope      .
           move      w-tes-cmc-sca (1)    to   w-cod-des-zmm-cod      .
           move      17                   to   w-cod-des-zmm-lin      .
           move      30                   to   w-cod-des-zmm-pos      .
           move      17                   to   w-cod-des-zmm-dln      .
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
       acc-cmc-sca-110.
           perform   cod-des-zmm-cll-000  thru cod-des-zmm-cll-999    .
           if        w-cod-des-zmm-ope    =    "F+"
                     go to acc-cmc-sca-115.
           if        w-cod-des-zmm-ope    =    "AC"
                     go to acc-cmc-sca-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cmc-sca-115.
           perform   cod-des-zmm-foi-000  thru cod-des-zmm-foi-999    .
           go to     acc-cmc-sca-110.
       acc-cmc-sca-120.
           move      w-cod-des-zmm-cod    to   v-alf                  .
       acc-cmc-sca-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cmc-sca-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cmc-sca-999.
       acc-cmc-sca-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-cmc-sca (1)      .
       acc-cmc-sca-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella [zmm]                       *
      *                  *---------------------------------------------*
           move      w-tes-cmc-sca (1)    to   w-let-arc-zmm-cod      .
           perform   let-arc-zmm-000      thru let-arc-zmm-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zmm-des    to   w-tes-cmc-sca-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-cmc-sca-des-000  thru vis-cmc-sca-des-999    .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : reimpostazione          *
      *                  *---------------------------------------------*
           if        w-let-arc-zmm-flg    not  = spaces
                     go to acc-cmc-sca-100.
      *                  *---------------------------------------------*
      *                  * Valore a spaces ammesso, purche' il tratta- *
      *                  * mento merce in conto sia :                  *
      *                  * 01 : Puo' riferirsi ad un altro documento;  *
      *                  * altrimenti reimpostazione, a meno che non si*
      *                  * sia in Up                                   *
      *                  *---------------------------------------------*
           if        w-tes-cmc-sca (1)    not  = spaces
                     go to acc-cmc-sca-600.
           if        w-tes-cau-sca-tmc (1)
                                          =    01
                     go to acc-cmc-sca-600.
           if        v-key                =    "UP  "
                     go to acc-cmc-sca-600
           else      go to acc-cmc-sca-100.
       acc-cmc-sca-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cmc-sca-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cmc-sca-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cmc-sca-100.
       acc-cmc-sca-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice merce in conto di  *
      *    * scarico componenti                                        *
      *    *-----------------------------------------------------------*
       vis-cmc-sca-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cmc-sca (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cmc-sca-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione per codice    *
      *    *                                 merce in conto di scarico *
      *    *                                 componenti                *
      *    *-----------------------------------------------------------*
       vis-cmc-sca-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-tes-cmc-sca-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cmc-sca-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Tipo di esplosione distinta  *
      *    * base                                                      *
      *    *-----------------------------------------------------------*
       acc-tde-dib-000.
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
           if        w-tes-cau-sca (1)    =    zero
                     go to acc-tde-dib-999.
       acc-tde-dib-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tde-dib-lun    to   v-car                  .
           move      w-exp-tde-dib-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tde-dib-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-tde-dib (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-tde-dib-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-tde-dib-999.
       acc-tde-dib-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-tde-dib (1)      .
       acc-tde-dib-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : reimpostazione                    *
      *                  *---------------------------------------------*
           if        w-tes-tde-dib (1)    not  = zero
                     go to acc-tde-dib-600.
           if        v-key                =    "UP  "
                     go to acc-tde-dib-600
           else      go to acc-tde-dib-100.
       acc-tde-dib-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tde-dib-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-tde-dib-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-tde-dib-100.
       acc-tde-dib-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Tipo di esplosione di-    *
      *    * stinta base                                               *
      *    *-----------------------------------------------------------*
       vis-tde-dib-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tde-dib-lun    to   v-car                  .
           move      w-exp-tde-dib-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tde-dib-tbl    to   v-txt                  .
           move      w-tes-tde-dib (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tde-dib-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Si/No gestione consumi addi- *
      *    * zionali                                                   *
      *    *-----------------------------------------------------------*
       acc-snx-cad-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-snx-cad (1)    to   w-sav-snx-cad          .
       acc-snx-cad-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           if        w-tes-snx-cad (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-cad (1)    =    "N"
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
                     go to acc-snx-cad-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-cad-999.
       acc-snx-cad-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   w-tes-snx-cad (1)
           else if   v-num                =    02
                     move  "N"            to   w-tes-snx-cad (1)
           else      move  spaces         to   w-tes-snx-cad (1)      .
       acc-snx-cad-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se a zero : reimpostazione                  *
      *                  *---------------------------------------------*
           if        v-num                not  = zero
                     go to acc-snx-cad-600.
           if        v-key                =    "UP  "
                     go to acc-snx-cad-600
           else      go to acc-snx-cad-100.
       acc-snx-cad-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore attuale come precedente : oltre   *
      *                  *---------------------------------------------*
           if        w-tes-snx-cad (1)    =    w-sav-snx-cad
                     go to acc-snx-cad-800.
      *                  *---------------------------------------------*
      *                  * Se valore attuale 'Si' : oltre              *
      *                  *---------------------------------------------*
           if        w-tes-snx-cad (1)    =    "S"
                     go to acc-snx-cad-800.
      *                  *---------------------------------------------*
      *                  * Trattamento causale magazzino scarico con-  *
      *                  * sumi                                        *
      *                  *---------------------------------------------*
           if        w-tes-cau-cad (1)    not  = zero
                     move  zero           to   w-tes-cau-cad (1)
                     perform vis-cau-cad-000
                                          thru vis-cau-cad-999
                     move  spaces         to   w-tes-cau-cad-des (1)
                     perform vis-cau-cad-des-000
                                          thru vis-cau-cad-des-999    .
      *                  *---------------------------------------------*
      *                  * Trattamento conto merce per scarico consu-  *
      *                  * mi                                          *
      *                  *---------------------------------------------*
           if        w-tes-cmc-cad (1)    not  = spaces
                     move  spaces         to   w-tes-cmc-cad (1)
                     perform vis-cmc-cad-000
                                          thru vis-cmc-cad-999
                     move  spaces         to   w-tes-cmc-cad-des (1)
                     perform vis-cmc-cad-des-000
                                          thru vis-cmc-cad-des-999    .
       acc-snx-cad-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-cad-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-cad-100.
       acc-snx-cad-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Si/No gestione consumi    *
      *    * addizionali                                               *
      *    *-----------------------------------------------------------*
       vis-snx-cad-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           if        w-tes-snx-cad (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-cad (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-cad-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Causale di magazzino di sca- *
      *    * rico consumi                                              *
      *    *-----------------------------------------------------------*
       acc-cau-cad-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-snx-cad (1)    not  = "S"
                     go to acc-cau-cad-999.
       acc-cau-cad-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zmc-ope      .
           move      w-tes-cau-cad (1)    to   w-cod-mne-zmc-cod      .
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
       acc-cau-cad-110.
           perform   cod-mne-zmc-cll-000  thru cod-mne-zmc-cll-999    .
           if        w-cod-mne-zmc-ope    =    "F+"
                     go to acc-cau-cad-115.
           if        w-cod-mne-zmc-ope    =    "AC"
                     go to acc-cau-cad-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cau-cad-115.
           perform   cod-mne-zmc-foi-000  thru cod-mne-zmc-foi-999    .
           go to     acc-cau-cad-110.
       acc-cau-cad-120.
           move      w-cod-mne-zmc-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cau-cad-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cau-cad-999.
       acc-cau-cad-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cau-cad (1)      .
       acc-cau-cad-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zmc]                      *
      *                  *---------------------------------------------*
           move      w-tes-cau-cad (1)    to   w-let-arc-zmc-cod      .
           perform   let-arc-zmc-000      thru let-arc-zmc-999        .
      *                  *---------------------------------------------*
      *                  * Trattamento descrizione                     *
      *                  *---------------------------------------------*
           move      w-let-arc-zmc-des    to   w-tes-cau-cad-des (1)  .
           perform   vis-cau-cad-des-000  thru vis-cau-cad-des-999    .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : reimpostazione          *
      *                  *---------------------------------------------*
           if        w-let-arc-zmc-flg    not  = spaces
                     go to acc-cau-cad-100.
      *                  *---------------------------------------------*
      *                  * Se causale di magazzino a zero : si saltano *
      *                  * ulteriori controlli                         *
      *                  *---------------------------------------------*
           if        w-tes-cau-cad (1)    =    zero
                     go to acc-cau-cad-600.
       acc-cau-cad-420.
      *                  *---------------------------------------------*
      *                  * Test su trattamento valore per la causale   *
      *                  *---------------------------------------------*
           if        w-let-arc-zmc-trv    =    "N"
                     go to acc-cau-cad-424.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Trattamento valore per la causale errato !        
      -              "     "              to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-cau-cad-100.
       acc-cau-cad-424.
      *                  *---------------------------------------------*
      *                  * Test su tipo movimento di magazzino         *
      *                  *---------------------------------------------*
           if        w-let-arc-zmc-tip    =    02
                     go to acc-cau-cad-426.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "La causale di magazzino non e' 'di scarico' !     
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                          *-------------------------------------*
      *                          * A reimpostazione                    *
      *                          *-------------------------------------*
           go to     acc-cau-cad-100.
       acc-cau-cad-426.
      *                  *---------------------------------------------*
      *                  * Controlli superati                          *
      *                  *---------------------------------------------*
           go to     acc-cau-cad-600.
       acc-cau-cad-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se causale di magazzino a zero              *
      *                  *---------------------------------------------*
           if        w-tes-cau-cad (1)    not  = zero
                     go to acc-cau-cad-620.
      *                      *-----------------------------------------*
      *                      * Normalizzazione c/merce                 *
      *                      *-----------------------------------------*
           if        w-tes-cmc-cad (1)    =    spaces
                     go to acc-cau-cad-610.
           move      spaces               to   w-tes-cmc-cad (1)      .
           move      spaces               to   w-tes-cmc-cad-des (1)  .
           perform   vis-cmc-cad-000      thru vis-cmc-cad-999        .
           perform   vis-cmc-cad-des-000  thru vis-cmc-cad-des-999    .
       acc-cau-cad-610.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     acc-cau-cad-800.
       acc-cau-cad-620.
      *                  *---------------------------------------------*
      *                  * Se causale di magazzino esistente           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Bufferizzazione valori per il c/merce   *
      *                      *-----------------------------------------*
           move      w-let-arc-zmc-tmc    to   w-tes-cau-cad-tmc (1)  .
           move      w-let-arc-zmc-cmc    to   w-tes-cau-cad-cmc (1)  .
           move      w-let-arc-zmc-vmc    to   w-tes-cau-cad-vmc (1)  .
      *                      *-----------------------------------------*
      *                      * Trattamento c/merce                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se causale non di c/merce           *
      *                          *-------------------------------------*
           if        w-tes-cau-cad-tmc (1)
                                          not  = 02
                     go to acc-cau-cad-670.
           if        w-tes-cmc-cad (1)    =    spaces
                     go to acc-cau-cad-660.
           move      spaces               to   w-tes-cmc-cad (1)      .
           move      spaces               to   w-tes-cmc-cad-des (1)  .
           perform   vis-cmc-cad-000      thru vis-cmc-cad-999        .
           perform   vis-cmc-cad-des-000  thru vis-cmc-cad-des-999    .
       acc-cau-cad-660.
           go to     acc-cau-cad-800.
       acc-cau-cad-670.
      *                          *-------------------------------------*
      *                          * Se causale di c/merce               *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se codice c/merce modificabile :*
      *                              * oltre                           *
      *                              *---------------------------------*
           if        w-tes-cau-cad-vmc (1)
                                          =    "S"
                     go to  acc-cau-cad-800.
      *                              *---------------------------------*
      *                              * Se codice c/merce non modifica- *
      *                              * bile                            *
      *                              *---------------------------------*
           if        w-tes-cau-cad-cmc (1)
                                          =    w-tes-cmc-cad (1)
                     go to  acc-cau-cad-800.
           move      w-tes-cau-cad-cmc (1)
                                          to   w-tes-cmc-cad (1)      .
           move      w-tes-cmc-cad (1)    to   w-let-arc-zmm-cod      .
           perform   let-arc-zmm-000      thru let-arc-zmm-999        .
           move      w-let-arc-zmm-des    to   w-tes-cmc-cad-des (1)  .
           perform   vis-cmc-cad-000      thru vis-cmc-cad-999        .
           perform   vis-cmc-cad-des-000  thru vis-cmc-cad-des-999    .
       acc-cau-cad-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cau-cad-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cau-cad-100.
       acc-cau-cad-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Causale di magazzino di   *
      *    * scarico consumi                                           *
      *    *-----------------------------------------------------------*
       vis-cau-cad-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cau-cad (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cau-cad-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione causale di    *
      *    *                                 magazzino di scarico con- *
      *    *                                 sumi                      *
      *    *-----------------------------------------------------------*
       vis-cau-cad-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-tes-cau-cad-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-cau-cad-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice merce in conto di     *
      *    * scarico consumi                                           *
      *    *-----------------------------------------------------------*
       acc-cmc-cad-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se no gestione consumi addizionali :   *
      *                      * uscita                                  *
      *                      *-----------------------------------------*
           if        w-tes-snx-cad (1)    not  = "S"
                     go to acc-cmc-cad-999.
      *                      *-----------------------------------------*
      *                      * Se causale di magazzino non esistente : *
      *                      * uscita                                  *
      *                      *-----------------------------------------*
           if        w-tes-cau-cad (1)    =    zero
                     go to acc-cmc-cad-999.
      *                      *-----------------------------------------*
      *                      * Test su trattamento c/merce della cau-  *
      *                      * sale di magazzino                       *
      *                      *-----------------------------------------*
           if        w-tes-cau-cad-tmc (1)
                                          =    02
                     go to acc-cmc-cad-999.
      *                      *-----------------------------------------*
      *                      * Se codice c/merce non variabile : uscita*
      *                      *-----------------------------------------*
           if        w-tes-cau-cad-vmc (1)
                                          =    "N"
                     go to acc-cmc-cad-999.
      *                  *---------------------------------------------*
      *                  * Preparazione valore di default              *
      *                  *---------------------------------------------*
           if        w-tes-cmc-cad (1)    =    spaces
                     move  w-tes-cau-cad-cmc (1)
                                          to   w-tes-cmc-cad (1)      .
       acc-cmc-cad-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-zmm-ope      .
           move      w-tes-cmc-cad (1)    to   w-cod-des-zmm-cod      .
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
       acc-cmc-cad-110.
           perform   cod-des-zmm-cll-000  thru cod-des-zmm-cll-999    .
           if        w-cod-des-zmm-ope    =    "F+"
                     go to acc-cmc-cad-115.
           if        w-cod-des-zmm-ope    =    "AC"
                     go to acc-cmc-cad-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cmc-cad-115.
           perform   cod-des-zmm-foi-000  thru cod-des-zmm-foi-999    .
           go to     acc-cmc-cad-110.
       acc-cmc-cad-120.
           move      w-cod-des-zmm-cod    to   v-alf                  .
       acc-cmc-cad-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cmc-cad-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cmc-cad-999.
       acc-cmc-cad-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-cmc-cad (1)      .
       acc-cmc-cad-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella [zmm]                       *
      *                  *---------------------------------------------*
           move      w-tes-cmc-cad (1)    to   w-let-arc-zmm-cod      .
           perform   let-arc-zmm-000      thru let-arc-zmm-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zmm-des    to   w-tes-cmc-cad-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-cmc-cad-des-000  thru vis-cmc-cad-des-999    .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : reimpostazione          *
      *                  *---------------------------------------------*
           if        w-let-arc-zmm-flg    not  = spaces
                     go to acc-cmc-cad-100.
      *                  *---------------------------------------------*
      *                  * Valore a spaces ammesso, purche' il tratta- *
      *                  * mento merce in conto sia :                  *
      *                  * 01 : Puo' riferirsi ad un altro documento;  *
      *                  * altrimenti reimpostazione, a meno che non si*
      *                  * sia in Up                                   *
      *                  *---------------------------------------------*
           if        w-tes-cmc-cad (1)    not  = spaces
                     go to acc-cmc-cad-600.
           if        w-tes-cau-cad-tmc (1)
                                          =    01
                     go to acc-cmc-cad-600.
           if        v-key                =    "UP  "
                     go to acc-cmc-cad-600
           else      go to acc-cmc-cad-100.
       acc-cmc-cad-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cmc-cad-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cmc-cad-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cmc-cad-100.
       acc-cmc-cad-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice merce in conto di  *
      *    * scarico consumi                                           *
      *    *-----------------------------------------------------------*
       vis-cmc-cad-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cmc-cad (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cmc-cad-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione per codice    *
      *    *                                 merce in conto di scarico *
      *    *                                 consumi                   *
      *    *-----------------------------------------------------------*
       vis-cmc-cad-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-tes-cmc-cad-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cmc-cad-des-999.
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
           move      13                   to   v-lin                  .
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
           move      13                   to   v-lin                  .
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
           move      14                   to   v-lin                  .
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
      *    * Visualizzazione campo testata : Default tipo movimento a  *
      *    * fronte                                                    *
      *    *-----------------------------------------------------------*
       vis-sgl-num-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      14                   to   v-lin                  .
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
           move      spaces               to   v-edm                  .
           move      16                   to   v-lin                  .
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
      *                  * Trattamento chiusura commessa               *
      *                  *---------------------------------------------*
           if        w-tes-chi-com (1)    not  = spaces
                     move  spaces         to   w-tes-chi-com (1)
                     perform vis-chi-com-000
                                          thru vis-chi-com-999        .
      *                  *---------------------------------------------*
      *                  * Trattamento obbligatorieta' movimento a     *
      *                  * fronte                                      *
      *                  *---------------------------------------------*
           if        w-tes-obl-afd (1)    not  = zero
                     move  zero           to   w-tes-obl-afd (1)
                     perform vis-obl-afd-000
                                          thru vis-obl-afd-999        .
      *                  *---------------------------------------------*
      *                  * Trattamento default tipo movimento a fronte *
      *                  *---------------------------------------------*
           if        w-tes-def-tmf (1)    not  = spaces
                     move  spaces         to   w-tes-def-tmf (1)
                     perform vis-def-tmf-000
                                          thru vis-def-tmf-999
                     move  spaces         to   w-tes-def-tmf-des (1)
                     perform vis-def-tmf-des-000
                                          thru vis-def-tmf-des-999    .
      *                  *---------------------------------------------*
      *                  * Trattamento modificabilita' tipo movimento  *
      *                  * a fronte                                    *
      *                  *---------------------------------------------*
           if        w-tes-mod-tmf (1)    not  = spaces
                     move  spaces         to   w-tes-mod-tmf (1)
                     perform vis-mod-tmf-000
                                          thru vis-mod-tmf-999        .
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
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-mov-afd-tbl    to   v-txt                  .
           move      w-tes-mov-afd (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-mov-afd-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Si/No chiusura commessa      *
      *    *-----------------------------------------------------------*
       acc-chi-com-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-mov-afd (1)    =    01
                     go to acc-chi-com-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-chi-com (1)    to   w-sav-chi-com          .
       acc-chi-com-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           if        w-tes-chi-com (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-chi-com (1)    =    "N"
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
                     go to acc-chi-com-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-chi-com-999.
       acc-chi-com-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   w-tes-chi-com (1)
           else if   v-num                =    02
                     move  "N"            to   w-tes-chi-com (1)
           else      move  spaces         to   w-tes-chi-com (1)      .
       acc-chi-com-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se a zero : reimpostazione                  *
      *                  *---------------------------------------------*
           if        v-num                not  = zero
                     go to acc-chi-com-600.
           if        v-key                =    "UP  "
                     go to acc-chi-com-600
           else      go to acc-chi-com-100.
       acc-chi-com-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore attuale come precedente : oltre   *
      *                  *---------------------------------------------*
           if        w-tes-chi-com (1)    =    w-sav-chi-com
                     go to acc-chi-com-800.
      *                  *---------------------------------------------*
      *                  * Se valore attuale 'N' : oltre               *
      *                  *---------------------------------------------*
           if        w-tes-chi-com (1)    =    "N"
                     go to acc-chi-com-800.
      *                  *---------------------------------------------*
      *                  * Trattamento obbligatorieta' riferimenti     *
      *                  * commessa                                    *
      *                  *---------------------------------------------*
           if        w-tes-obl-afd (1)    not  = 01
                     move  01             to   w-tes-obl-afd (1)
                     perform vis-obl-afd-000
                                          thru vis-obl-afd-999        .
       acc-chi-com-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-chi-com-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-chi-com-100.
       acc-chi-com-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Si/No chiusura commessa   *
      *    *-----------------------------------------------------------*
       vis-chi-com-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           if        w-tes-chi-com (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-chi-com (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-chi-com-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Obbligatorieta' movimento a  *
      *    * fronte                                                    *
      *    *-----------------------------------------------------------*
       acc-obl-afd-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-mov-afd (1)    =    01
                     go to acc-obl-afd-999.
           if        w-tes-chi-com (1)    =    "S"
                     go to acc-obl-afd-999.
       acc-obl-afd-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-obl-afd-lun    to   v-car                  .
           move      w-exp-obl-afd-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-obl-afd-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-obl-afd (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-obl-afd-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-obl-afd-999.
       acc-obl-afd-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-obl-afd (1)      .
       acc-obl-afd-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : reimpostazione, a meno che non    *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        w-tes-obl-afd (1)    not  = zero
                     go to acc-obl-afd-600.
           if        v-key                =    "UP  "
                     go to acc-obl-afd-600
           else      go to acc-obl-afd-100.
       acc-obl-afd-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-obl-afd-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-obl-afd-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-obl-afd-100.
       acc-obl-afd-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Obbligatorieta' movimento *
      *    * a fronte                                                  *
      *    *-----------------------------------------------------------*
       vis-obl-afd-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-obl-afd-lun    to   v-car                  .
           move      w-exp-obl-afd-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-obl-afd-tbl    to   v-txt                  .
           move      w-tes-obl-afd (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-obl-afd-999.
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
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo movimento a *
      *                  * fronte                                      *
      *                  *---------------------------------------------*
           if        w-tes-mov-afd (1)    =    02
                     go to acc-def-tmf-120
           else      go to acc-def-tmf-999.
       acc-def-tmf-120.
      *                  *---------------------------------------------*
      *                  * Se movimento a fronte commessa di produzio- *
      *                  * ne                                          *
      *                  *---------------------------------------------*
           perform   acc-tmo-cdp-000      thru acc-tmo-cdp-999        .
           go to     acc-def-tmf-400.
       acc-def-tmf-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-def-tmf-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-def-tmf-800.
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
      *    * Visualizzazione campo testata : Descrizione default tipo  *
      *    * movimento a fronte                                        *
      *    *-----------------------------------------------------------*
       vis-def-tmf-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-tes-def-tmf-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-def-tmf-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Tipo movimento commessa di   *
      *    * produzione                                                *
      *    *-----------------------------------------------------------*
       acc-tmo-cdp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-def-tmf (1)    to   w-sav-def-tmf          .
       acc-tmo-cdp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-ycp-ope      .
           move      w-tes-def-tmf (1)    to   w-cod-des-ycp-cod      .
           move      20                   to   w-cod-des-ycp-lin      .
           move      30                   to   w-cod-des-ycp-pos      .
           move      20                   to   w-cod-des-ycp-dln      .
           move      37                   to   w-cod-des-ycp-dps      .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           perform   cod-des-ycp-cll-000  thru cod-des-ycp-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-ycp-foi-000  thru cod-des-ycp-foi-999    .
       acc-tmo-cdp-110.
           perform   cod-des-ycp-cll-000  thru cod-des-ycp-cll-999    .
           if        w-cod-des-ycp-ope    =    "F+"
                     go to acc-tmo-cdp-115.
           if        w-cod-des-ycp-ope    =    "AC"
                     go to acc-tmo-cdp-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-tmo-cdp-115.
           perform   cod-des-ycp-foi-000  thru cod-des-ycp-foi-999    .
           go to     acc-tmo-cdp-110.
       acc-tmo-cdp-120.
           move      w-cod-des-ycp-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-tmo-cdp-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-tmo-cdp-999.
       acc-tmo-cdp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-def-tmf (1)      .
       acc-tmo-cdp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [ycp]                      *
      *                  *---------------------------------------------*
           move      w-tes-def-tmf (1)    to   w-let-arc-ycp-cod      .
           perform   let-arc-ycp-000      thru let-arc-ycp-999        .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori associati al tipo    *
      *                  * movimento commessa di produzione            *
      *                  *---------------------------------------------*
           move      w-let-arc-ycp-des    to   w-tes-def-tmf-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-def-tmf-des-000  thru vis-def-tmf-des-999    .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-ycp-flg    not  = spaces
                     go to acc-tmo-cdp-100.
      *                  *---------------------------------------------*
      *                  * Se valore a spaces                          *
      *                  *---------------------------------------------*
           if        w-tes-def-tmf (1)    not  = spaces
                     go to acc-tmo-cdp-600.
      *                      *-----------------------------------------*
      *                      * Se valore obbligatorio: reimpostazione, *
      *                      * a meno che non si sia in Up             *
      *                      *-----------------------------------------*
           if        w-tes-obl-afd (1)    not  = 01
                     go to acc-tmo-cdp-600.
           if        v-key                =    "UP  "
                     go to acc-tmo-cdp-600
           else      go to acc-tmo-cdp-100.
       acc-tmo-cdp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore attuale come precedente : oltre   *
      *                  *---------------------------------------------*
           if        w-tes-def-tmf (1)    =    w-sav-def-tmf
                     go to acc-tmo-cdp-800.
      *                  *---------------------------------------------*
      *                  * Se valore attuale esistente : oltre         *
      *                  *---------------------------------------------*
           if        w-tes-def-tmf (1)    not  = spaces
                     go to acc-tmo-cdp-800.
      *                  *---------------------------------------------*
      *                  * Trattamento modificabilita' tipo movimento  *
      *                  * a fronte                                    *
      *                  *---------------------------------------------*
           if        w-tes-mod-tmf (1)    not  = spaces
                     move  spaces         to   w-tes-mod-tmf (1)
                     perform vis-mod-tmf-000
                                          thru vis-mod-tmf-999        .
       acc-tmo-cdp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-tmo-cdp-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-tmo-cdp-100.
       acc-tmo-cdp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Modificabilita' tipo movi-   *
      *    * mento a fronte                                            *
      *    *-----------------------------------------------------------*
       acc-mod-tmf-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-mov-afd (1)    =    01
                     go to acc-mod-tmf-999.
           if        w-tes-def-tmf (1)    =    spaces
                     go to acc-mod-tmf-999.
       acc-mod-tmf-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           if        w-tes-mod-tmf (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-mod-tmf (1)    =    "N"
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
                     go to acc-mod-tmf-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-mod-tmf-999.
       acc-mod-tmf-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   w-tes-mod-tmf (1)
           else if   v-num                =    02
                     move  "N"            to   w-tes-mod-tmf (1)
           else      move  spaces         to   w-tes-mod-tmf (1)      .
       acc-mod-tmf-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se a zero : reimpostazione                  *
      *                  *---------------------------------------------*
           if        v-num                not  = zero
                     go to acc-mod-tmf-600.
           if        v-key                =    "UP  "
                     go to acc-mod-tmf-600
           else      go to acc-mod-tmf-100.
       acc-mod-tmf-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-mod-tmf-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-mod-tmf-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-mod-tmf-100.
       acc-mod-tmf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Modificabilita' tipo mo-  *
      *    * vimento a fronte                                          *
      *    *-----------------------------------------------------------*
       vis-mod-tmf-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-gen-lun    to   v-car                  .
           move      w-exp-snx-gen-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-gen-tbl    to   v-txt                  .
           if        w-tes-mod-tmf (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-mod-tmf (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-mod-tmf-999.
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
           move      w-tes-cod-tmv        to   v-alf                  .
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
           move      w-tes-des-tmv (1)    to   v-alf                  .
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
      *                  * Lettura archivio [ada]                      *
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
      -              "!              "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     acc-cod-dpz-100.
       acc-cod-dpz-450.
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-ada-den    to   w-rig-cod-dpz-des (1)  .
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
       acc-cod-dsl-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-zmd-ope      .
           move      w-rig-cod-dpz (1)    to   w-cod-des-zmd-dpz      .
           move      w-rig-cod-dsl (1)    to   w-cod-des-zmd-cod      .
           move      21                   to   w-cod-des-zmd-lin      .
           move      21                   to   w-cod-des-zmd-pos      .
           move      21                   to   w-cod-des-zmd-dln      .
           move      30                   to   w-cod-des-zmd-dps      .
           move      spaces               to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           if        w-cnt-sts-imp-rig    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      "REMV"               to   v-pfk (06)             .
           perform   cod-des-zmd-cll-000  thru cod-des-zmd-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-zmd-foi-000  thru cod-des-zmd-foi-999    .
       acc-cod-dsl-110.
           perform   cod-des-zmd-cll-000  thru cod-des-zmd-cll-999    .
           if        w-cod-des-zmd-ope    =    "F+"
                     go to acc-cod-dsl-115.
           if        w-cod-des-zmd-ope    =    "AC"
                     go to acc-cod-dsl-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-dsl-115.
           perform   cod-des-zmd-foi-000  thru cod-des-zmd-foi-999    .
           go to     acc-cod-dsl-110.
       acc-cod-dsl-120.
           move      w-cod-des-zmd-cod    to   v-alf                  .
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
      *                  * Lettura tabella [zmd]                       *
      *                  *---------------------------------------------*
           move      w-rig-cod-dpz (1)    to   w-let-arc-zmd-dpz      .
           move      w-rig-cod-dsl (1)    to   w-let-arc-zmd-cod      .
           perform   let-arc-zmd-000      thru let-arc-zmd-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zmd-des    to   w-rig-cod-dsl-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-cod-dsl-des-000  thru vis-cod-dsl-des-999    .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : reimpostazione          *
      *                  *---------------------------------------------*
           if        w-let-arc-zmd-flg    not  = spaces
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
           if        w-tes-cod-tmv        =    spaces
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
      *              * Test su codice c/merce per carico               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cau-car (1)    =    zero
                     go to cnt-tdo-nok-010.
           if        w-tes-cau-car-tmc (1)
                                          =    01 or
                     w-tes-cau-car-tmc (1)
                                          =    02
                     go to cnt-tdo-nok-010.
      *                  *---------------------------------------------*
      *                  * Controllo                                   *
      *                  *---------------------------------------------*
           if        w-tes-cmc-car (1)    not  = spaces
                     go to cnt-tdo-nok-010.
           move      "Manca il codice c/merce per carico magazzino !    
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-010.
      *              *-------------------------------------------------*
      *              * Test su codice c/merce per scarico componenti   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cau-sca (1)    =    zero
                     go to cnt-tdo-nok-020.
           if        w-tes-cau-sca-tmc (1)
                                          =    01 or
                     w-tes-cau-sca-tmc (1)
                                          =    02
                     go to cnt-tdo-nok-020.
      *                  *---------------------------------------------*
      *                  * Controllo                                   *
      *                  *---------------------------------------------*
           if        w-tes-cmc-sca (1)    not  = spaces
                     go to cnt-tdo-nok-020.
           move      "Manca il codice c/merce per scarico componenti !  
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-020.
      *              *-------------------------------------------------*
      *              * Test su : Si/No consumi addizionali             *
      *              *-------------------------------------------------*
           if        w-tes-snx-cad (1)    =    "S" or
                     w-tes-snx-cad (1)    =    "N"
                     go to cnt-tdo-nok-030.
           move      "Manca la definizione sulla gestione consumi addizi
      -              "onali !        "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-030.
      *              *-------------------------------------------------*
      *              * Test su codice c/merce per scarico componenti   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-snx-cad (1)    not  = "S"
                     go to cnt-tdo-nok-040.
           if        w-tes-cau-cad (1)    =    zero
                     go to cnt-tdo-nok-040.
           if        w-tes-cau-cad-tmc (1)
                                          =    01 or
                     w-tes-cau-cad-tmc (1)
                                          =    02
                     go to cnt-tdo-nok-040.
      *                  *---------------------------------------------*
      *                  * Controllo                                   *
      *                  *---------------------------------------------*
           if        w-tes-cmc-cad (1)    not  = spaces
                     go to cnt-tdo-nok-040.
           move      "Manca il codice c/merce per scarico consumi !     
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-040.
      *              *-------------------------------------------------*
      *              * Test su : origine del documento                 *
      *              *-------------------------------------------------*
           if        w-tes-org-doc (1)    not  = zero
                     go to cnt-tdo-nok-050.
           move      "Manca il tipo origine del documento !             
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-050.
      *              *-------------------------------------------------*
      *              * Test su : Movimento a fronte di                 *
      *              *-------------------------------------------------*
           if        w-tes-mov-afd (1)    not  = zero
                     go to cnt-tdo-nok-060.
           move      "Manca la definizione se movimento a fronte !      
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-060.
      *              *-------------------------------------------------*
      *              * Test su : Si/No chiusura commessa               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-mov-afd (1)    =    01
                     go to cnt-tdo-nok-070.
      *                  *---------------------------------------------*
      *                  * Controllo                                   *
      *                  *---------------------------------------------*
           if        w-tes-chi-com (1)    =    "S" or
                     w-tes-chi-com (1)    =    "N"
                     go to cnt-tdo-nok-070.
           move      "Manca la definizione se movimento di pareggiamento
      -              " !             "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-070.
      *              *-------------------------------------------------*
      *              * Test su : Obbligatorieta' movimento a fronte    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-mov-afd (1)    =    01
                     go to cnt-tdo-nok-080.
      *                  *---------------------------------------------*
      *                  * Controllo                                   *
      *                  *---------------------------------------------*
           if        w-tes-obl-afd (1)    not  = zero
                     go to cnt-tdo-nok-080.
           move      "Manca la definizione se riferimenti obbligatori ! 
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-080.
      *              *-------------------------------------------------*
      *              * Test su : Default tipo movimento a fronte       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-mov-afd (1)    =    01
                     go to cnt-tdo-nok-090.
      *                  *---------------------------------------------*
      *                  * Controllo                                   *
      *                  *---------------------------------------------*
           if        w-tes-obl-afd (1)    not  = 01
                     go to cnt-tdo-nok-090.
           if        w-tes-def-tmf (1)    not  = spaces
                     go to cnt-tdo-nok-090.
           move      "Manca il tipo movimento a fronte da proporre !    
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-090.
      *              *-------------------------------------------------*
      *              * Test su : Obbligatorieta' movimento a fronte    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-mov-afd (1)    =    01
                     go to cnt-tdo-nok-100.
           if        w-tes-def-tmf (1)    =    spaces
                     go to cnt-tdo-nok-100.
      *                  *---------------------------------------------*
      *                  * Controllo                                   *
      *                  *---------------------------------------------*
           if        w-tes-mod-tmf (1)    =    "S" or
                     w-tes-mod-tmf (1)    =    "N"
                     go to cnt-tdo-nok-100.
           move      "Manca la definizione se tipo movimento a fronte mo
      -              "dificabile !   "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-100.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
       cnt-tdo-nok-500.
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
           move      spaces               to   w-tes-cod-tmv          .
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *-----------------------------------------------------------*
       nor-nok-tes-000.
           move      spaces               to   w-tes-des-tmv (1)      .
           move      spaces               to   w-tes-des-key (1)      .
           move      spaces               to   w-tes-pwd-tmv (1)      .
           move      zero                 to   w-tes-cau-car (1)      .
           move      spaces               to   w-tes-cau-car-des (1)  .
           move      zero                 to   w-tes-cau-car-tmc (1)  .
           move      spaces               to   w-tes-cau-car-cmc (1)  .
           move      spaces               to   w-tes-cau-car-vmc (1)  .
           move      spaces               to   w-tes-cmc-car (1)      .
           move      spaces               to   w-tes-cmc-car-des (1)  .
           move      zero                 to   w-tes-cau-sca (1)      .
           move      spaces               to   w-tes-cau-sca-des (1)  .
           move      zero                 to   w-tes-cau-sca-tmc (1)  .
           move      spaces               to   w-tes-cau-sca-cmc (1)  .
           move      spaces               to   w-tes-cau-sca-vmc (1)  .
           move      spaces               to   w-tes-cmc-sca (1)      .
           move      spaces               to   w-tes-cmc-sca-des (1)  .
           move      zero                 to   w-tes-tde-dib (1)      .
           move      spaces               to   w-tes-snx-cad (1)      .
           move      zero                 to   w-tes-cau-cad (1)      .
           move      spaces               to   w-tes-cau-cad-des (1)  .
           move      zero                 to   w-tes-cau-cad-tmc (1)  .
           move      spaces               to   w-tes-cau-cad-cmc (1)  .
           move      spaces               to   w-tes-cau-cad-vmc (1)  .
           move      spaces               to   w-tes-cmc-cad (1)      .
           move      spaces               to   w-tes-cmc-cad-des (1)  .
           move      zero                 to   w-tes-org-doc (1)      .
           move      zero                 to   w-tes-prv-doc (1)      .
           move      spaces               to   w-tes-sgl-num (1)      .
           move      zero                 to   w-tes-mov-afd (1)      .
           move      spaces               to   w-tes-chi-com (1)      .
           move      zero                 to   w-tes-obl-afd (1)      .
           move      spaces               to   w-tes-def-tmf (1)      .
           move      spaces               to   w-tes-def-tmf-des (1)  .
           move      spaces               to   w-tes-mod-tmf (1)      .
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
      *              * Lettura archivio [yvp]                          *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODTMV    "         to   f-key                  .
           move      w-tes-cod-tmv        to   rf-yvp-cod-tmv         .
           move      zero                 to   rf-yvp-cod-dpz         .
           move      "pgm/vdp/fls/ioc/obj/iofyvp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yvp                 .
           if        f-sts                =    e-not-err
                     go to rou-let-reg-100.
      *                  *---------------------------------------------*
      *                  * Se movimento non trovato                    *
      *                  *---------------------------------------------*
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
      *                          * record [yvp]                        *
      *                          *-------------------------------------*
           move      rf-yvp-des-tmv       to   w-tes-des-tmv (1)      .
           move      rf-yvp-des-key       to   w-tes-des-key (1)      .
           move      rf-yvp-pwd-tmv       to   w-tes-pwd-tmv (1)      .
           move      rf-yvp-cau-car       to   w-tes-cau-car (1)      .
           move      rf-yvp-cmc-car       to   w-tes-cmc-car (1)      .
           move      rf-yvp-cau-sca       to   w-tes-cau-sca (1)      .
           move      rf-yvp-cmc-sca       to   w-tes-cmc-sca (1)      .
           move      rf-yvp-tde-dib       to   w-tes-tde-dib (1)      .
           move      rf-yvp-snx-cad       to   w-tes-snx-cad (1)      .
           move      rf-yvp-cau-cad       to   w-tes-cau-cad (1)      .
           move      rf-yvp-cmc-cad       to   w-tes-cmc-cad (1)      .
           move      rf-yvp-org-doc       to   w-tes-org-doc (1)      .
           move      rf-yvp-prv-doc       to   w-tes-prv-doc (1)      .
           move      rf-yvp-sgl-num       to   w-tes-sgl-num (1)      .
           move      rf-yvp-mov-afd       to   w-tes-mov-afd (1)      .
           move      rf-yvp-chi-com       to   w-tes-chi-com (1)      .
           move      rf-yvp-obl-afd       to   w-tes-obl-afd (1)      .
           move      rf-yvp-def-tmf       to   w-tes-def-tmf (1)      .
           move      rf-yvp-mod-tmf       to   w-tes-mod-tmf (1)      .
           move      rf-yvp-alx-gen       to   w-tes-alx-gen (1)      .
      *                          *-------------------------------------*
      *                          * Valori contenuti indirettamente in  *
      *                          * record [yvp]                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura archivio [zmc]          *
      *                              *---------------------------------*
           move      w-tes-cau-car (1)    to   w-let-arc-zmc-cod      .
           perform   let-arc-zmc-000      thru let-arc-zmc-999        .
           move      w-let-arc-zmc-des    to   w-tes-cau-car-des (1)  .
           move      w-let-arc-zmc-tmc    to   w-tes-cau-car-tmc (1)  .
           move      w-let-arc-zmc-cmc    to   w-tes-cau-car-cmc (1)  .
           move      w-let-arc-zmc-vmc    to   w-tes-cau-car-vmc (1)  .
      *                              *---------------------------------*
      *                              * Lettura tabella [zmm]           *
      *                              *---------------------------------*
           move      w-tes-cmc-car (1)    to   w-let-arc-zmm-cod      .
           perform   let-arc-zmm-000      thru let-arc-zmm-999        .
           move      w-let-arc-zmm-des    to   w-tes-cmc-car-des (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [zmc]          *
      *                              *---------------------------------*
           move      w-tes-cau-sca (1)    to   w-let-arc-zmc-cod      .
           perform   let-arc-zmc-000      thru let-arc-zmc-999        .
           move      w-let-arc-zmc-des    to   w-tes-cau-sca-des (1)  .
           move      w-let-arc-zmc-tmc    to   w-tes-cau-sca-tmc (1)  .
           move      w-let-arc-zmc-cmc    to   w-tes-cau-sca-cmc (1)  .
           move      w-let-arc-zmc-vmc    to   w-tes-cau-sca-vmc (1)  .
      *                              *---------------------------------*
      *                              * Lettura tabella [zmm]           *
      *                              *---------------------------------*
           move      w-tes-cmc-sca (1)    to   w-let-arc-zmm-cod      .
           perform   let-arc-zmm-000      thru let-arc-zmm-999        .
           move      w-let-arc-zmm-des    to   w-tes-cmc-sca-des (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [zmc]          *
      *                              *---------------------------------*
           move      w-tes-cau-cad (1)    to   w-let-arc-zmc-cod      .
           perform   let-arc-zmc-000      thru let-arc-zmc-999        .
           move      w-let-arc-zmc-des    to   w-tes-cau-cad-des (1)  .
           move      w-let-arc-zmc-tmc    to   w-tes-cau-cad-tmc (1)  .
           move      w-let-arc-zmc-cmc    to   w-tes-cau-cad-cmc (1)  .
           move      w-let-arc-zmc-vmc    to   w-tes-cau-cad-vmc (1)  .
      *                              *---------------------------------*
      *                              * Lettura tabella [zmm]           *
      *                              *---------------------------------*
           move      w-tes-cmc-cad (1)    to   w-let-arc-zmm-cod      .
           perform   let-arc-zmm-000      thru let-arc-zmm-999        .
           move      w-let-arc-zmm-des    to   w-tes-cmc-cad-des (1)  .
      *                              *---------------------------------*
      *                              * Lettura default tipo movimento  *
      *                              * a fronte                        *
      *                              *---------------------------------*
           if        w-tes-mov-afd (1)    not  = 02
                     go to rou-let-reg-400.
           move      w-tes-def-tmf (1)    to   w-let-arc-ycp-cod      .
           perform   let-arc-ycp-000      thru let-arc-ycp-999        .
           move      w-let-arc-ycp-des    to   w-tes-def-tmf-des (1)  .
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
           move      w-tes-cod-tmv        to   rf-yvp-cod-tmv         .
           move      zero                 to   rf-yvp-cod-dpz         .
           move      "pgm/vdp/fls/ioc/obj/iofyvp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yvp                 .
      *                      *-----------------------------------------*
      *                      * Se errore di start : fine lettura       *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to  rou-let-reg-999.
       rou-let-reg-600.
      *                  *---------------------------------------------*
      *                  * Lettura sequenziale file [yvp]              *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/vdp/fls/ioc/obj/iofyvp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yvp                 .
      *                      *-----------------------------------------*
      *                      * Se at end : fine lettura                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to  rou-let-reg-999.
      *                  *---------------------------------------------*
      *                  * Se oltre il massimo : fine lettura          *
      *                  *---------------------------------------------*
           if        rf-yvp-cod-tmv       not  = w-tes-cod-tmv
                     go to  rou-let-reg-999.
      *                  *---------------------------------------------*
      *                  * Se record con codice dipendenza a zero : ri-*
      *                  * ciclo in lettura                            *
      *                  *---------------------------------------------*
           if        rf-yvp-cod-dpz       =    zero
                     go to rou-let-reg-600.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione riga                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Valori attuali                          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Valori contenuti direttamente in    *
      *                          * record [yvp]                        *
      *                          *-------------------------------------*
           move      rf-yvp-cod-dpz       to   w-rig-cod-dpz (1)      .
           move      rf-yvp-cod-dsl       to   w-rig-cod-dsl (1)      .
           move      rf-yvp-alx-dpz       to   w-rig-alx-dpz (1)      .
      *                          *-------------------------------------*
      *                          * Valori contenuti indirettamente in  *
      *                          * record [yvp]                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura archivio [ada]          *
      *                              *---------------------------------*
           move      w-rig-cod-dpz (1)    to   w-let-arc-ada-cod      .
           perform   let-arc-ada-000      thru let-arc-ada-999        .
           move      w-let-arc-ada-den    to   w-rig-cod-dpz-des (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [zmd]          *
      *                              *---------------------------------*
           move      w-rig-cod-dpz (1)    to   w-let-arc-zmd-dpz      .
           move      w-rig-cod-dsl (1)    to   w-let-arc-zmd-cod      .
           perform   let-arc-zmd-000      thru let-arc-zmd-999        .
           move      w-let-arc-zmd-des    to   w-rig-cod-dsl-des (1)  .
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
      *                  * Riciclo a lettura [yvp] successivo          *
      *                  *---------------------------------------------*
           go to     rou-let-reg-600.
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
      *              * Delete [yvp] relativo alla dipendenza           *
      *              *-------------------------------------------------*
           perform   del-rec-yvd-000      thru del-rec-yvd-999        .
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
      *              * Trattamento file [yvp] dipendenza               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-500.
       scr-mov-fil-400.
      *                      *-----------------------------------------*
      *                      * Write record [yvp] relativo alla dipen- *
      *                      * denza                                   *
      *                      *-----------------------------------------*
           perform   wrt-rec-yvd-000      thru wrt-rec-yvd-999        .
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
      *                          * Rewrite [yvp] relativo alla dipen-  *
      *                          * denza                               *
      *                          *-------------------------------------*
           perform   rew-rec-yvd-000      thru rew-rec-yvd-999        .
      *                          *-------------------------------------*
      *                          * Riciclo a lettura riga corpo suc-   *
      *                          * cessiva                             *
      *                          *-------------------------------------*
           go to     scr-mov-fil-300.
       scr-mov-fil-700.
      *              *-------------------------------------------------*
      *              * Trattamento file [yvp] generale                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-800.
      *                      *-----------------------------------------*
      *                      * Write record [yvp] generale             *
      *                      *-----------------------------------------*
           perform   wrt-rec-yvg-000      thru wrt-rec-yvg-999        .
           go to     scr-mov-fil-999.
       scr-mov-fil-800.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rewrite record [yvp] generale           *
      *                      *-----------------------------------------*
           perform   rew-rec-yvg-000      thru rew-rec-yvg-999        .
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
      *                  * Delete [yvp] relativo alla dipendenza       *
      *                  *---------------------------------------------*
           perform   del-rec-yvd-000      thru del-rec-yvd-999        .
      *                  *---------------------------------------------*
      *                  * Riciclo a lettura riga corpo successiva     *
      *                  *---------------------------------------------*
           go to     del-mov-fil-100.
       del-mov-fil-500.
      *              *-------------------------------------------------*
      *              * Cancellazione file [yvp] generale               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Delete record [yvp] generale                *
      *                  *---------------------------------------------*
           perform   del-rec-yvg-000      thru del-rec-yvg-999        .
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [yvp] relativo alla dipendenza        *
      *    *-----------------------------------------------------------*
       cmp-rec-yvd-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/vdp/fls/ioc/obj/iofyvp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yvp                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           move      w-tes-cod-tmv        to   rf-yvp-cod-tmv         .
           move      w-rig-cod-dpz (1)    to   rf-yvp-cod-dpz         .
           move      w-rig-cod-dsl (1)    to   rf-yvp-cod-dsl         .
           move      w-rig-alx-dpz (1)    to   rf-yvp-alx-dpz         .
       cmp-rec-yvd-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [yvp] relativo alla dipendenza           *
      *    *-----------------------------------------------------------*
       wrt-rec-yvd-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-yvd-000      thru cmp-rec-yvd-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/vdp/fls/ioc/obj/iofyvp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yvp                 .
       wrt-rec-yvd-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record righe [yvp] relativo alla dipendenza   *
      *    *-----------------------------------------------------------*
       rew-rec-yvd-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-yvd-000      thru cmp-rec-yvd-999        .
      *              *-------------------------------------------------*
      *              * Force Put record                                *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/vdp/fls/ioc/obj/iofyvp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yvp                 .
       rew-rec-yvd-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [yvp] relativo alla dipendenza       *
      *    *-----------------------------------------------------------*
       del-rec-yvd-000.
      *              *-------------------------------------------------*
      *              * Composizione chiave primaria                    *
      *              *-------------------------------------------------*
           move      w-tes-cod-tmv        to   rf-yvp-cod-tmv         .
           move      w-rig-cod-dpz (2)    to   rf-yvp-cod-dpz         .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/vdp/fls/ioc/obj/iofyvp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yvp                 .
       del-rec-yvd-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [yvp] generale                        *
      *    *-----------------------------------------------------------*
       cmp-rec-yvg-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/vdp/fls/ioc/obj/iofyvp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yvp                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           move      w-tes-cod-tmv        to   rf-yvp-cod-tmv         .
           move      zero                 to   rf-yvp-cod-dpz         .
           move      w-tes-des-tmv (1)    to   rf-yvp-des-tmv         .
           move      w-tes-des-key (1)    to   rf-yvp-des-key         .
           move      w-tes-pwd-tmv (1)    to   rf-yvp-pwd-tmv         .
           move      w-tes-cau-car (1)    to   rf-yvp-cau-car         .
           move      w-tes-cmc-car (1)    to   rf-yvp-cmc-car         .
           move      w-tes-cau-sca (1)    to   rf-yvp-cau-sca         .
           move      w-tes-cmc-sca (1)    to   rf-yvp-cmc-sca         .
           move      w-tes-tde-dib (1)    to   rf-yvp-tde-dib         .
           move      w-tes-snx-cad (1)    to   rf-yvp-snx-cad         .
           move      w-tes-cau-cad (1)    to   rf-yvp-cau-cad         .
           move      w-tes-cmc-cad (1)    to   rf-yvp-cmc-cad         .
           move      w-tes-org-doc (1)    to   rf-yvp-org-doc         .
           move      w-tes-prv-doc (1)    to   rf-yvp-prv-doc         .
           move      w-tes-sgl-num (1)    to   rf-yvp-sgl-num         .
           move      w-tes-mov-afd (1)    to   rf-yvp-mov-afd         .
           move      w-tes-chi-com (1)    to   rf-yvp-chi-com         .
           move      w-tes-obl-afd (1)    to   rf-yvp-obl-afd         .
           move      w-tes-def-tmf (1)    to   rf-yvp-def-tmf         .
           move      w-tes-mod-tmf (1)    to   rf-yvp-mod-tmf         .
           move      w-tes-alx-gen (1)    to   rf-yvp-alx-gen         .
       cmp-rec-yvg-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [yvp] generale                           *
      *    *-----------------------------------------------------------*
       wrt-rec-yvg-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-yvg-000      thru cmp-rec-yvg-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/vdp/fls/ioc/obj/iofyvp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yvp                 .
       wrt-rec-yvg-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [yvp] generale                         *
      *    *-----------------------------------------------------------*
       rew-rec-yvg-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-yvg-000      thru cmp-rec-yvg-999        .
      *              *-------------------------------------------------*
      *              * Force Put record                                *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/vdp/fls/ioc/obj/iofyvp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yvp                 .
       rew-rec-yvg-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [yvp] generale                       *
      *    *-----------------------------------------------------------*
       del-rec-yvg-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-yvg-000      thru cmp-rec-yvg-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/vdp/fls/ioc/obj/iofyvp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yvp                 .
       del-rec-yvg-999.
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
      *    * Box per messaggio di errore esteso, su tre righe          *
      *    *-----------------------------------------------------------*
       box-msg-e03-000.
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
           move      10                   to   v-lin                  .
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
           move      11                   to   v-lin                  .
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
           move      12                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-m02    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Linea 03                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-m03    to   v-alf                  .
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
       box-msg-e03-999.
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
           move      rf-ada-cod-mne       to   w-let-arc-ada-den      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-ada-999.
       let-arc-ada-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-ada-flg      .
           move      all   "."            to   w-let-arc-ada-den      .
           go to     let-arc-ada-999.
       let-arc-ada-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-ada-den      .
       let-arc-ada-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zmc]                         *
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
           move      rf-zmc-tip-mdm       to   w-let-arc-zmc-tip      .
           move      rf-zmc-trt-mic       to   w-let-arc-zmc-tmc      .
           move      rf-zmc-tip-mic       to   w-let-arc-zmc-tcm      .
           move      rf-zmc-cod-mic       to   w-let-arc-zmc-cmc      .
           move      rf-zmc-snv-mic       to   w-let-arc-zmc-vmc      .
           move      rf-zmc-def-tar       to   w-let-arc-zmc-dta      .
           move      rf-zmc-snv-tar       to   w-let-arc-zmc-vta      .
           move      rf-zmc-lst-tar       to   w-let-arc-zmc-lta      .
           move      rf-zmc-def-tco       to   w-let-arc-zmc-dtm      .
           move      rf-zmc-snv-tco       to   w-let-arc-zmc-vtm      .
           move      rf-zmc-lst-tco       to   w-let-arc-zmc-ltm      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zmc-999.
       let-arc-zmc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zmc-flg      .
           move      all "."              to   w-let-arc-zmc-des      .
           go to     let-arc-zmc-600.
       let-arc-zmc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zmc-des      .
       let-arc-zmc-600.
           move      spaces               to   w-let-arc-zmc-trv      .
           move      zero                 to   w-let-arc-zmc-tip      .
           move      zero                 to   w-let-arc-zmc-tmc      .
           move      spaces               to   w-let-arc-zmc-tcm      .
           move      spaces               to   w-let-arc-zmc-cmc      .
           move      spaces               to   w-let-arc-zmc-vmc      .
           move      spaces               to   w-let-arc-zmc-dta      .
           move      spaces               to   w-let-arc-zmc-vta      .
           move      spaces               to   w-let-arc-zmc-lta      .
           move      spaces               to   w-let-arc-zmc-dtm      .
           move      spaces               to   w-let-arc-zmc-vtm      .
           move      spaces               to   w-let-arc-zmc-ltm      .
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
      *    * Routine lettura tabella [zmd]                             *
      *    *-----------------------------------------------------------*
       let-arc-zmd-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zmd-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice dislocazione a Spaces            *
      *              *-------------------------------------------------*
           if        w-let-arc-zmd-cod    =    spaces
                     go to let-arc-zmd-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODDSL    "         to   f-key                  .
           move      w-let-arc-zmd-dpz    to   rf-zmd-cod-dpz         .
           move      w-let-arc-zmd-cod    to   rf-zmd-cod-dsl         .
           move      "pgm/mag/fls/ioc/obj/iofzmd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zmd                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zmd-400.
       let-arc-zmd-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zmd-des-dsl       to   w-let-arc-zmd-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zmd-999.
       let-arc-zmd-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zmd-flg      .
           move      all   "."            to   w-let-arc-zmd-des      .
           go to     let-arc-zmd-999.
       let-arc-zmd-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zmd-des      .
       let-arc-zmd-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [ycp]                         *
      *    *-----------------------------------------------------------*
       let-arc-ycp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-ycp-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spazi                          *
      *              *-------------------------------------------------*
           if        w-let-arc-ycp-cod    =    spaces
                     go to let-arc-ycp-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODTCP"             to   f-key                  .
           move      w-let-arc-ycp-cod    to   rf-ycp-cod-tcp         .
           move      "pgm/cdp/fls/ioc/obj/iofycp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ycp                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-ycp-400.
       let-arc-ycp-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-ycp-des-tcp       to   w-let-arc-ycp-des      .
           move      rf-ycp-vld-dpz       to   w-let-arc-ycp-vld      .
           move      rf-ycp-cod-dpz       to   w-let-arc-ycp-dpz      .
           move      rf-ycp-org-doc       to   w-let-arc-ycp-ord      .
           move      rf-ycp-prv-doc       to   w-let-arc-ycp-prd      .
           move      rf-ycp-sgl-num       to   w-let-arc-ycp-sgl      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-ycp-999.
       let-arc-ycp-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-ycp-flg      .
           move      all   "."            to   w-let-arc-ycp-des      .
           go to     let-arc-ycp-600.
       let-arc-ycp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-ycp-des      .
       let-arc-ycp-600.
           move      spaces               to   w-let-arc-ycp-des      .
           move      zero                 to   w-let-arc-ycp-vld      .
           move      zero                 to   w-let-arc-ycp-dpz      .
           move      zero                 to   w-let-arc-ycp-ord      .
           move      zero                 to   w-let-arc-ycp-prd      .
           move      spaces               to   w-let-arc-ycp-sgl      .
       let-arc-ycp-999.
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
           copy      "pgm/vdp/prg/cpy/acdeyvp0.acs"                   .

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
           copy      "pgm/mag/prg/cpy/acdezmd0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per accettazione codice dipendenza dell'a-    *
      *    * zienda                                                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/acoddpz0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione tipo movimento per commes- *
      *    * se di produzione                                          *
      *    *-----------------------------------------------------------*
           copy      "pgm/cdp/prg/cpy/acdeycp0.acs"                   .
