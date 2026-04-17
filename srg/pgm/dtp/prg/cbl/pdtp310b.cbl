       Identification Division.
       Program-Id.                                 pdtp310b           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dtp                 *
      *                                Settore:    ges                 *
      *                                   Fase:    dtp310              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 03/12/93    *
      *                       Ultima revisione:    NdK del 08/02/06    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Operazioni speciali su distinte base        *
      *                                                                *
      *                    Sottoprogramma per esecuzione funzione di : *
      *                                                                *
      *                    Eliminazione di un componente da tutte le   *
      *                    distinte base in cui il componente da eli-  *
      *                    minare compare                              *
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

      *================================================================*
       Input-Output Section.
      *================================================================*

       File-Control.

      *    *===========================================================*
      *    * File Control [srt]                                        *
      *    *-----------------------------------------------------------*
           select  srt       assign       to sort                     .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [srt]                                    *
      *    *-----------------------------------------------------------*
       sd  srt.
      *    *-----------------------------------------------------------*
      *    * Sort record per ordinamento righe di distinte base        *
      *    *-----------------------------------------------------------*
       01  srt-rec.
      *        *-------------------------------------------------------*
      *        * Chiave di ordinamento                                 *
      *        *-------------------------------------------------------*
           05  srt-key.
      *            *---------------------------------------------------*
      *            * Tipo di ordinamento per l'assieme, come letto     *
      *            * dalla riga di distinta base, e poi trasformato    *
      *            *   - A : Prodotto finito                           *
      *            *   - B : Semilavorato                              *
      *            *   - C : Sub-distinta virtuale                     *
      *            *   - Z : Tipo assieme non riconosciuto             *
      *            *---------------------------------------------------*
               10  srt-ord-tip-ass        pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Segnale di anagrafica assieme esistente oppure no *
      *            *   - Spaces : Esistente                            *
      *            *   - #      : Non esistente                        *
      *            *---------------------------------------------------*
               10  srt-ord-fae-ass        pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Codice alfanumerico di ordinamento per l'assieme, *
      *            * che corrisponde al codice alfanumerico dell'as-   *
      *            * sieme in caso di anagrafica esistente, oppure     *
      *            * viene posto pari al codice numerico tra parentesi *
      *            *---------------------------------------------------*
               10  srt-ord-alf-ass        pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Codice numerico di ordinamento per l'assieme, che *
      *            * corrisponde sempre al suo codice numerico         *
      *            *---------------------------------------------------*
               10  srt-ord-num-ass        pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  srt-dat.
      *            *---------------------------------------------------*
      *            * Tipo magazzino interessato per l'assieme, come    *
      *            * letto dalla riga di distinta base                 *
      *            *---------------------------------------------------*
               10  srt-lgr-tpm-ass        pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Codice numerico magazzino per l'assieme, come     *
      *            * letto dalla riga di distinta base                 *
      *            *---------------------------------------------------*
               10  srt-lgr-nrm-ass        pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Numero progressivo elemento nella distinta, come  *
      *            * letto dalla riga di distinta base                 *
      *            *---------------------------------------------------*
               10  srt-lgr-num-prg        pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Tipo magazzino interessato per il componente, co- *
      *            * me letto dalla riga di distinta base              *
      *            *---------------------------------------------------*
               10  srt-lgr-tpm-cpt        pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Codice numerico magazzino per il componente, come *
      *            * letto dalla riga di distinta base                 *
      *            *---------------------------------------------------*
               10  srt-lgr-nrm-cpt        pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice alfanumerico magazzino per il componente,  *
      *            * come letto dalla riga di distinta base            *
      *            *---------------------------------------------------*
               10  srt-lgr-afm-cpt        pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Unita' di misura per la produzione per il compo-  *
      *            * nente, come letto dalla riga di distinta base     *
      *            *---------------------------------------------------*
               10  srt-lgr-umi-prd        pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Numero decimali per la quantita' di impiego per   *
      *            * il componente, come letto dalla riga di distinta  *
      *            * base                                              *
      *            *---------------------------------------------------*
               10  srt-lgr-dec-qta        pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Quantita' di impiego, moltiplicatore, come letto  *
      *            * dalla riga di distinta base                       *
      *            *---------------------------------------------------*
               10  srt-lgr-qta-ipm        pic  9(08)v9(03)            .
      *            *---------------------------------------------------*
      *            * Quantita' di impiego, divisore, come letto dalla  *
      *            * riga di distinta base                             *
      *            *---------------------------------------------------*
               10  srt-lgr-qta-ipd        pic  9(08)v9(03)            .
      *            *---------------------------------------------------*
      *            * Note relative al componente, come letto dalla ri- *
      *            * ga di distinta base                               *
      *            *---------------------------------------------------*
               10  srt-lgr-not-cpt        pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Area libera per espansioni speciali, come letto   *
      *            * dalla riga di distinta base                       *
      *            *---------------------------------------------------*
               10  srt-lgr-alx-exp.
                   15  filler  occurs 20  pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Descrizione per l'assieme, da anagrafica assieme  *
      *            *---------------------------------------------------*
               10  srt-ass-des-ass        pic  x(40)                  .

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
                     "dtp"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "ges"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "dtp310"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pdtp310b"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "ELIMINAZIONE COMPONENTI IN DISTINTE BASE"       .

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
      *            * Per routine pre-exe-pgm-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-pre-exe-pgm      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine rou-opn-fls-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-rou-opn-fls      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine rou-car-ini-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-rou-car-ini      pic  x(01)                  .
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
      *        * [lgt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dtp/fls/rec/rflgt"                          .
      *        *-------------------------------------------------------*
      *        * [lgr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dtp/fls/rec/rflgr"                          .
      *        *-------------------------------------------------------*
      *        * [lgv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dtp/fls/rec/rflgv"                          .

      *    *===========================================================*
      *    * Work-area per bufferizzazione testata                     *
      *    *-----------------------------------------------------------*
       01  w-tes.
      *        *-------------------------------------------------------*
      *        * Flag di primo passaggio in impostazione campi chiave  *
      *        *-------------------------------------------------------*
           05  w-tes-flg-uno              pic  x(01) value spaces     .
      *        *-------------------------------------------------------*
      *        * Componente da eliminare, tipo                         *
      *        *  - M : Materia prima                                  *
      *        *  - S : Semilavorato                                   *
      *        *  - D : Subdistinta virtuale                           *
      *        *-------------------------------------------------------*
           05  w-tes-cde-tip              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Componente da eliminare, codice numerico              *
      *        *-------------------------------------------------------*
           05  w-tes-cde-num              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Componente da eliminare, codice alfanumerico          *
      *        *-------------------------------------------------------*
           05  w-tes-cde-alf              pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Componente da eliminare, descrizione                  *
      *        *-------------------------------------------------------*
           05  w-tes-cde-des              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Componente da eliminare, unita' di misura             *
      *        *-------------------------------------------------------*
           05  w-tes-cde-umi              pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Componente da eliminare, decimali quantita'           *
      *        *-------------------------------------------------------*
           05  w-tes-cde-dec              pic  9(01)                  .

      *    *===========================================================*
      *    * Work-area per bufferizzazione riga corpo                  *
      *    *-----------------------------------------------------------*
       01  w-rig.
      *        *-------------------------------------------------------*
      *        * Numero progressivo per la riga                        *
      *        *-------------------------------------------------------*
           05  w-rig-num-prg              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Dati provenienti dalla riga di distinta base          *
      *        *-------------------------------------------------------*
           05  w-rig-lgr.
      *            *---------------------------------------------------*
      *            * Tipo magazzino interessato per l'assieme, come    *
      *            * letto dalla riga di distinta base                 *
      *            *---------------------------------------------------*
               10  w-rig-lgr-tpm-ass      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Codice numerico magazzino per l'assieme, come     *
      *            * letto dalla riga di distinta base                 *
      *            *---------------------------------------------------*
               10  w-rig-lgr-nrm-ass      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Numero progressivo elemento nella distinta, come  *
      *            * letto dalla riga di distinta base                 *
      *            *---------------------------------------------------*
               10  w-rig-lgr-num-prg      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Tipo magazzino interessato per il componente, co- *
      *            * me letto dalla riga di distinta base              *
      *            *---------------------------------------------------*
               10  w-rig-lgr-tpm-cpt      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Codice numerico magazzino per il componente, come *
      *            * letto dalla riga di distinta base                 *
      *            *---------------------------------------------------*
               10  w-rig-lgr-nrm-cpt      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice alfanumerico magazzino per il componente,  *
      *            * come letto dalla riga di distinta base            *
      *            *---------------------------------------------------*
               10  w-rig-lgr-afm-cpt      pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Unita' di misura per la produzione per il compo-  *
      *            * nente, come letto dalla riga di distinta base     *
      *            *---------------------------------------------------*
               10  w-rig-lgr-umi-prd      pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Numero decimali per la quantita' di impiego per   *
      *            * il componente, come letto dalla riga di distinta  *
      *            * base                                              *
      *            *---------------------------------------------------*
               10  w-rig-lgr-dec-qta      pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Quantita' di impiego, moltiplicatore, come letto  *
      *            * dalla riga di distinta base                       *
      *            *---------------------------------------------------*
               10  w-rig-lgr-qta-ipm      pic  9(08)v9(03)            .
      *            *---------------------------------------------------*
      *            * Quantita' di impiego, divisore, come letto dalla  *
      *            * riga di distinta base                             *
      *            *---------------------------------------------------*
               10  w-rig-lgr-qta-ipd      pic  9(08)v9(03)            .
      *            *---------------------------------------------------*
      *            * Note relative al componente, come letto dalla ri- *
      *            * ga di distinta base                               *
      *            *---------------------------------------------------*
               10  w-rig-lgr-not-cpt      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Area libera per espansioni speciali, come letto   *
      *            * dalla riga di distinta base                       *
      *            *---------------------------------------------------*
               10  w-rig-lgr-alx-exp.
                   15  filler  occurs 20  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Dati provenienti dall'anagrafica assieme              *
      *        *-------------------------------------------------------*
           05  w-rig-ass.
      *            *---------------------------------------------------*
      *            * Codice alfanumerico magazzino per l'assieme, da   *
      *            * anagrafica assieme                                *
      *            *---------------------------------------------------*
               10  w-rig-ass-afm-ass      pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Descrizione per l'assieme, da anagrafica assieme  *
      *            *---------------------------------------------------*
               10  w-rig-ass-des-ass      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Dati da accettazione                                  *
      *        *-------------------------------------------------------*
           05  w-rig-acc.
      *            *---------------------------------------------------*
      *            * Si/No eliminazione                                *
      *            *  - S : Si                                         *
      *            *  - N : No                                         *
      *            *---------------------------------------------------*
               10  w-rig-acc-snx-eli      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No eliminazione modificabile                   *
      *            *  - S : Si                                         *
      *            *  - N : No                                         *
      *            *---------------------------------------------------*
               10  w-rig-acc-snx-emo      pic  x(01)                  .

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
      *                * - Numero linea                                *
      *                * - Parentesi quadra aperta                     *
      *                * - Tipo assieme                                *
      *                * - Parentesi quadra chiusa                     *
      *                * - Codice assieme                              *
      *                * - Descrizione assieme                         *
      *                * - Literal per si/no sostituzione              *
      *                *-----------------------------------------------*
                   15  w-lin-imm-num-lin  pic  x(05)                  .
                   15  filler             pic  x(02)                  .
                   15  w-lin-imm-pqu-ape  pic  x(01)                  .
                   15  w-lin-imm-tip-ass  pic  x(01)                  .
                   15  w-lin-imm-pqu-chi  pic  x(01)                  .
                   15  filler             pic  x(03)                  .
                   15  w-lin-imm-cod-ass  pic  x(14)                  .
                   15  filler             pic  x(02)                  .
                   15  w-lin-imm-des-ass  pic  x(40)                  .
                   15  filler             pic  x(02)                  .
                   15  w-lin-imm-lit-snx  pic  x(09)                  .

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
      *        * Work per Let su archivio [lgv]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-lgv.
               10  w-let-arc-lgv-flg      pic  x(01)                  .
               10  w-let-arc-lgv-num      pic  9(07)                  .
               10  w-let-arc-lgv-alf      pic  x(14)                  .
               10  w-let-arc-lgv-des      pic  x(40)                  .
               10  w-let-arc-lgv-umi      pic  x(03)                  .
               10  w-let-arc-lgv-deq      pic  9(01)                  .
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

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Salvataggio per tipo componente da eliminare          *
      *        *-------------------------------------------------------*
           05  w-sav-cde-tip              pic  x(01)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo tipo componente da eliminare          *
      *        *-------------------------------------------------------*
           05  w-exp-cde-tip.
               10  w-exp-cde-tip-num      pic  9(02)       value 3    .
               10  w-exp-cde-tip-lun      pic  9(02)       value 40   .
               10  w-exp-cde-tip-tbl.
                   15  filler             pic  x(40) value
                            "Materia prima                           ".
                   15  filler             pic  x(40) value
                            "Semilavorato                            ".
                   15  filler             pic  x(40) value
                            "Sub-distinta virtuale                   ".

      *    *===========================================================*
      *    * Work per determinazione numero linea a video per tratta-  *
      *    * mento del corpo direttamente nelle righe di scroll        *
      *    *-----------------------------------------------------------*
       01  w-num-lin-scr.
      *        *-------------------------------------------------------*
      *        * Numero riga corpo di riferimento                      *
      *        *-------------------------------------------------------*
           05  w-num-lin-scr-rig          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero linea determinato                              *
      *        *-------------------------------------------------------*
           05  w-num-lin-scr-lin          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Work di comodo per determinazione                     *
      *        *-------------------------------------------------------*
           05  w-num-lin-scr-w01          pic  9(05)                  .
           05  w-num-lin-scr-w02          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero facciata determinato                           *
      *        *-------------------------------------------------------*
           05  w-num-lin-scr-nfa          pic  9(05)                  .

      *    *===========================================================*
      *    * Work per il caricamento iniziale delle righe di distinta  *
      *    * base da trattare                                          *
      *    *-----------------------------------------------------------*
       01  w-car-rig-dba.
      *        *-------------------------------------------------------*
      *        * Flag di uscita                                        *
      *        *-------------------------------------------------------*
           05  w-car-rig-dba-flg          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Messaggio d'errore se errori in caricamento           *
      *        *-------------------------------------------------------*
           05  w-car-rig-dba-err          pic  x(65)                  .
      *        *-------------------------------------------------------*
      *        * Numero di elementi caricati                           *
      *        *-------------------------------------------------------*
           05  w-car-rig-dba-nel          pic  9(07)                  .

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
      *    * Link-area per accettazione codice prodotto 'dcp'          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acoddcp0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice semilavorato 'dps'      *
      *    *-----------------------------------------------------------*
           copy      "pgm/dps/prg/cpy/acoddps0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice legame virtuale         *
      *    *-----------------------------------------------------------*
           copy      "pgm/dtp/prg/cpy/acodlgv0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice materia prima           *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/acoddpm0.acl"                   .

      *    *===========================================================*
      *    * Work per subroutine di esplosione scalare distinta base   *
      *    *-----------------------------------------------------------*
       01  w-esp-scl-dtp.
      *        *-------------------------------------------------------*
      *        * Parametri in input                                    *
      *        *-------------------------------------------------------*
           05  w-esp-scl-dtp-inp.
      *            *---------------------------------------------------*
      *            * Funzione da eseguire                              *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-fun      pic  x(05)                  .
      *            *---------------------------------------------------*
      *            * Tipo magazzino per l'assieme da esplodere         *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-tde      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Codice numerico magazzino da esplodere            *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-nde      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice alfanumerico magazzino da esplodere        *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-ade      pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Quantita' da esplodere per l'assieme              *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-qde      pic s9(08)v9(03)            .
      *        *-------------------------------------------------------*
      *        * Parametri in output                                   *
      *        *-------------------------------------------------------*
           05  w-esp-scl-dtp-out.
      *            *---------------------------------------------------*
      *            * Status di uscita dalle funzioni                   *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-sts      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Tipo di elemento trovato nella scansione          *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-tip      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Codice numerico elemento trovato                  *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-num      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice alfanumerico elemento trovato              *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-alf      pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Livello di profondita' elemento trovato           *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-liv      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Quantita' totale relativa all'elemento trovato    *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-qta      pic s9(08)v9(03)            .
      *            *---------------------------------------------------*
      *            * Flag di anagrafica elemento esistente             *
      *            *  - Spaces : Si                                    *
      *            *  - #      : No                                    *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-ana      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Descrizione da anagrafica elemento                *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-des      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Unita' di misura da anagrafica elemento           *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-umi      pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Numero decimali quantita' da anagrafica elemento  *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-dec      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Area di lavoro                                        *
      *        *-------------------------------------------------------*
           05  w-esp-scl-dtp-war.
      *            *---------------------------------------------------*
      *            * Livello di profondita' interno                    *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-wlp      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Salvataggio livello di profondita' interno attu-  *
      *            * ale                                               *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-wls      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Segnale di prossima operazione da eseguire        *
      *            *  - S : Start sul livello di profondita' attuale   *
      *            *  - U : Incremento di un livello e quindi 'start'  *
      *            *        sul livello di profondita' aumentato       *
      *            *  - N : Read Next sul livello di profondita' attu- *
      *            *        ale                                        *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-wpo      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Tabella per ogni livello, max 50                  *
      *            *---------------------------------------------------*
               10  w-esp-scl-dtp-wtb occurs 50.
      *                *-----------------------------------------------*
      *                * Tipo di elemento                              *
      *                *  - 01 : Prodotto finito                       *
      *                *  - 02 : Semilavorato                          *
      *                *  - 03 : Materia prima                         *
      *                *  - 99 : Subdistinta virtuale                  *
      *                *-----------------------------------------------*
                   15  w-esp-scl-dtp-wti  pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Codice numerico elemento                      *
      *                *-----------------------------------------------*
                   15  w-esp-scl-dtp-wnu  pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Codice alfanumerico elemento                  *
      *                *-----------------------------------------------*
                   15  w-esp-scl-dtp-wal  pic  x(14)                  .
      *                *-----------------------------------------------*
      *                * Numero progressivo di riga distinta           *
      *                *-----------------------------------------------*
                   15  w-esp-scl-dtp-wrg  pic  9(05)                  .
      *                *-----------------------------------------------*
      *                * Quantita' relativa all'elemento               *
      *                *-----------------------------------------------*
                   15  w-esp-scl-dtp-wqt  pic s9(08)v9(03)            .
      *                *-----------------------------------------------*
      *                * Elemento anagraficamente esistente            *
      *                *  - Spaces : Si                                *
      *                *  - #      : No                                *
      *                *-----------------------------------------------*
                   15  w-esp-scl-dtp-wes  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Descrizione anagrafica per l'elemento         *
      *                *-----------------------------------------------*
                   15  w-esp-scl-dtp-wde  pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Unita' di misura per l'elemento               *
      *                *-----------------------------------------------*
                   15  w-esp-scl-dtp-wum  pic  x(03)                  .
      *                *-----------------------------------------------*
      *                * Numero decimali per quantita'                 *
      *                *-----------------------------------------------*
                   15  w-esp-scl-dtp-wnd  pic  9(01)                  .

      *    *===========================================================*
      *    * Work per subroutine di implosione scalare distinta base   *
      *    *-----------------------------------------------------------*
       01  w-imp-scl-dtp.
      *        *-------------------------------------------------------*
      *        * Parametri in input                                    *
      *        *-------------------------------------------------------*
           05  w-imp-scl-dtp-inp.
      *            *---------------------------------------------------*
      *            * Funzione da eseguire                              *
      *            *---------------------------------------------------*
               10  w-imp-scl-dtp-fun      pic  x(05)                  .
      *            *---------------------------------------------------*
      *            * Tipo magazzino per il componente da implodere     *
      *            *---------------------------------------------------*
               10  w-imp-scl-dtp-tdi      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Codice numerico magazzino per il componente da    *
      *            * implodere                                         *
      *            *---------------------------------------------------*
               10  w-imp-scl-dtp-ndi      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Quantita' da implodere per il componente          *
      *            *---------------------------------------------------*
               10  w-imp-scl-dtp-qdi      pic s9(08)v9(03)            .
      *        *-------------------------------------------------------*
      *        * Parametri in output                                   *
      *        *-------------------------------------------------------*
           05  w-imp-scl-dtp-out.
      *            *---------------------------------------------------*
      *            * Status di uscita dalle funzioni                   *
      *            *---------------------------------------------------*
               10  w-imp-scl-dtp-sts      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Tipo di componente trovato nella scansione        *
      *            *---------------------------------------------------*
               10  w-imp-scl-dtp-tco      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Codice numerico del componente trovato nella      *
      *            * scansione                                         *
      *            *---------------------------------------------------*
               10  w-imp-scl-dtp-nco      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Tipo di assieme trovato nella scansione           *
      *            *---------------------------------------------------*
               10  w-imp-scl-dtp-tas      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Codice numerico dell'assieme trovato nella        *
      *            * scansione                                         *
      *            *---------------------------------------------------*
               10  w-imp-scl-dtp-nas      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Livello di profondita'                            *
      *            *---------------------------------------------------*
               10  w-imp-scl-dtp-liv      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Quantita' producibile relativa all'assieme tro-   *
      *            * vato                                              *
      *            *---------------------------------------------------*
               10  w-imp-scl-dtp-qta      pic s9(08)v9(03)            .
      *            *---------------------------------------------------*
      *            * Flag di anagrafica esistente per l'assieme        *
      *            *  - Spaces : Si                                    *
      *            *  - #      : No                                    *
      *            *---------------------------------------------------*
               10  w-imp-scl-dtp-ana      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Descrizione da anagrafica assieme                 *
      *            *---------------------------------------------------*
               10  w-imp-scl-dtp-des      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Unita' di misura da assieme anagrafica assieme    *
      *            *---------------------------------------------------*
               10  w-imp-scl-dtp-umi      pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Numero decimali quantita' da anagrafica assieme   *
      *            *---------------------------------------------------*
               10  w-imp-scl-dtp-dec      pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Unita' di misura da anagrafica componente         *
      *            * te                                                *
      *            *---------------------------------------------------*
               10  w-imp-scl-dtp-cum      pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Numero decimali quantita' da anagrafica componen- *
      *            * te                                                *
      *            *---------------------------------------------------*
               10  w-imp-scl-dtp-cnd      pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Quantita' di utilizzo, moltiplicatore             *
      *            *---------------------------------------------------*
               10  w-imp-scl-dtp-cmu      pic s9(08)v9(03)            .
      *            *---------------------------------------------------*
      *            * Quantita' di utilizzo, divisore                   *
      *            *---------------------------------------------------*
               10  w-imp-scl-dtp-cdu      pic s9(08)v9(03)            .
      *        *-------------------------------------------------------*
      *        * Area di lavoro                                        *
      *        *-------------------------------------------------------*
           05  w-imp-scl-dtp-war.
      *            *---------------------------------------------------*
      *            * Livello di profondita' interno                    *
      *            *---------------------------------------------------*
               10  w-imp-scl-dtp-wlp      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Comodo per livello di profondita' interno attuale *
      *            * aumentato di 1                                    *
      *            *---------------------------------------------------*
               10  w-imp-scl-dtp-wls      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Segnale di prossima operazione da eseguire        *
      *            *  - S : Start sul livello di profondita' attuale   *
      *            *  - U : Incremento di un livello e quindi 'start'  *
      *            *        sul livello di profondita' aumentato       *
      *            *  - N : Read Next sul livello di profondita' attu- *
      *            *        ale                                        *
      *            *---------------------------------------------------*
               10  w-imp-scl-dtp-wpo      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Ultimo tipo componente letto                      *
      *            *---------------------------------------------------*
               10  w-imp-scl-dtp-utc      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Ultimo codice numerico componente letto           *
      *            *---------------------------------------------------*
               10  w-imp-scl-dtp-ucc      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Ultima unita' di misura componente letta          *
      *            *---------------------------------------------------*
               10  w-imp-scl-dtp-uuc      pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Ultimo numero decimali componente letto           *
      *            *---------------------------------------------------*
               10  w-imp-scl-dtp-udc      pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Tabella per ogni livello, max 50                  *
      *            *---------------------------------------------------*
               10  w-imp-scl-dtp-wtb occurs 50.
      *                *-----------------------------------------------*
      *                * Tipo di componente                            *
      *                *-----------------------------------------------*
                   15  w-imp-scl-dtp-wtc  pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Codice numerico del componente                *
      *                *-----------------------------------------------*
                   15  w-imp-scl-dtp-wnc  pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Tipo di assieme                               *
      *                *-----------------------------------------------*
                   15  w-imp-scl-dtp-wta  pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Codice numerico dell'assieme                  *
      *                *-----------------------------------------------*
                   15  w-imp-scl-dtp-wna  pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Numero riga in distinta                       *
      *                *-----------------------------------------------*
                   15  w-imp-scl-dtp-wrg  pic  9(05)                  .
      *                *-----------------------------------------------*
      *                * Quantita' producibile relativa all'assieme    *
      *                *-----------------------------------------------*
                   15  w-imp-scl-dtp-wqt  pic s9(08)v9(03)            .
      *                *-----------------------------------------------*
      *                * Assieme anagraficamente esistente             *
      *                *  - Spaces : Si                                *
      *                *  - #      : No                                *
      *                *-----------------------------------------------*
                   15  w-imp-scl-dtp-wes  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Descrizione anagrafica per l'assieme          *
      *                *-----------------------------------------------*
                   15  w-imp-scl-dtp-wde  pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Unita' di misura per l'assieme                *
      *                *-----------------------------------------------*
                   15  w-imp-scl-dtp-wum  pic  x(03)                  .
      *                *-----------------------------------------------*
      *                * Numero decimali per l'assieme                 *
      *                *-----------------------------------------------*
                   15  w-imp-scl-dtp-wnd  pic  9(01)                  .
      *                *-----------------------------------------------*
      *                * Sigla unita' di misura per il componente      *
      *                *-----------------------------------------------*
                   15  w-imp-scl-dtp-wuc  pic  x(03)                  .
      *                *-----------------------------------------------*
      *                * Numero decimali per il componente             *
      *                *-----------------------------------------------*
                   15  w-imp-scl-dtp-wdc  pic  9(01)                  .
      *                *-----------------------------------------------*
      *                * Quantita' di utilizzo, moltiplicatore         *
      *                *-----------------------------------------------*
                   15  w-imp-scl-dtp-wcm  pic s9(08)v9(03)            .
      *                *-----------------------------------------------*
      *                * Quantita' di utilizzo, divisore               *
      *                *-----------------------------------------------*
                   15  w-imp-scl-dtp-wcd  pic s9(08)v9(03)            .

      *    *===========================================================*
      *    * Work-area per Ctl                                         *
      *    *-----------------------------------------------------------*
       01  w-ctl.
      *        *-------------------------------------------------------*
      *        * Work-area per bufferizzazione della distinta base su  *
      *        * cui eseguire il controllo di non-svuotamento, per un  *
      *        * massimo di un elemento                                *
      *        *-------------------------------------------------------*
           05  w-ctl-nsb.
      *            *---------------------------------------------------*
      *            * Tipo magazzino dell'assieme da bufferizzare       *
      *            *---------------------------------------------------*
               10  w-ctl-nsb-tpm-ass      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Codice numerico magazzino dell'assieme da buf-    *
      *            * ferizzare                                         *
      *            *---------------------------------------------------*
               10  w-ctl-nsb-nrm-ass      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice alfanumerico magazzino dell'assieme da     *
      *            * bufferizzare                                      *
      *            *---------------------------------------------------*
               10  w-ctl-nsb-afm-ass      pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Numero di elementi caricati nel buffer, cioe' nu- *
      *            * mero di componenti della distinta base            *
      *            *---------------------------------------------------*
               10  w-ctl-nsb-num-cpt      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Indice per la scansione degli  elementi caricati  *
      *            * nel buffer, cioe' per la scansione dei componenti *
      *            *---------------------------------------------------*
               10  w-ctl-nsb-inx-cpt      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Buffer per gli elementi caricati nel buffer, cioe'*
      *            * buffer per i componenti della distinta base       *
      *            *---------------------------------------------------*
               10  w-ctl-nsb-buf-cpt.
      *                *-----------------------------------------------*
      *                * Massimo numero di elementi caricabili         *
      *                *-----------------------------------------------*
                   15  w-ctl-nsb-max-ele  pic  9(05)       value   1  .
      *                *-----------------------------------------------*
      *                * Elementi nel buffer                           *
      *                *-----------------------------------------------*
                   15  w-ctl-nsb-buf-ele  occurs   1.
      *                    *-------------------------------------------*
      *                    * Numero progressivo del componente all'in- *
      *                    * terno della distinta                      *
      *                    *-------------------------------------------*
                       20  w-ctl-nsb-prg-cpt
                                          pic  9(05)                  .
      *                    *-------------------------------------------*
      *                    * Tipo magazzino del componente             *
      *                    *-------------------------------------------*
                       20  w-ctl-nsb-tpm-cpt
                                          pic  9(02)                  .
      *                    *-------------------------------------------*
      *                    * Codice numerico magazzino del componente  *
      *                    *-------------------------------------------*
                       20  w-ctl-nsb-nrm-cpt
                                          pic  9(07)                  .
      *                    *-------------------------------------------*
      *                    * Codice alfanumerico magazzino del compo-  *
      *                    * nente                                     *
      *                    *-------------------------------------------*
                       20  w-ctl-nsb-afm-cpt
                                          pic  x(14)                  .

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
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
           perform   pre-exe-pgm-000      thru pre-exe-pgm-999        .
           if        w-cnt-pre-exe-pgm    not  = spaces
                     go to main-999.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-rou-opn-fls      .
           perform   rou-opn-fls-000      thru rou-opn-fls-999        .
           if        w-cnt-rou-opn-fls    not  = spaces
                     go to main-800.
       main-200.
      *              *-------------------------------------------------*
      *              * Accettazione campi chiave                       *
      *              *-------------------------------------------------*
           perform   acc-key-reg-000      thru acc-key-reg-999        .
      *              *-------------------------------------------------*
      *              * Se tipo uscita "E" : fine programma             *
      *              *-------------------------------------------------*
           if        w-cnt-tus-acc-key    =    "E"
                     go to main-800.
      *              *-------------------------------------------------*
      *              * Accettazione campi non chiave                   *
      *              *-------------------------------------------------*
           perform   acc-nok-reg-000      thru acc-nok-reg-999        .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo di sucita         *
      *              *-------------------------------------------------*
           if        w-cnt-tus-acc-nok    =    "E"
                     go to main-400.
       main-300.
      *              *-------------------------------------------------*
      *              * Se uscita per conferma                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Routine post-conferma                       *
      *                  *---------------------------------------------*
           perform   pos-cnf-ins-000      thru pos-cnf-ins-999        .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     main-800.
       main-400.
      *              *-------------------------------------------------*
      *              * Se uscita per non conferma                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Routine post-exit                           *
      *                  *---------------------------------------------*
           perform   pos-exi-ins-000      thru pos-exi-ins-999        .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     main-800.
       main-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
           perform   rou-cls-fls-000      thru rou-cls-fls-999        .
       main-900.
      *              *-------------------------------------------------*
      *              * Esecuzione routine post-esecuzione programma    *
      *              *-------------------------------------------------*
           perform   pos-exe-pgm-000      thru pos-exe-pgm-999        .
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
      *              * Tasto di funzione Prsc : sempre disabilitato se *
      *              * in impostazione testata, altrimenti inalterato  *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    "T"
                     move  spaces         to   v-pfk (07)             .
      *              *-------------------------------------------------*
      *              * Tasto di funzione Nxsc : sempre disabilitato se *
      *              * in impostazione testata, altrimenti inalterato  *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    "T"
                     move  spaces         to   v-pfk (08)             .
      *              *-------------------------------------------------*
      *              * Tasto di funzione Delt : sempre disabilitato    *
      *              *-------------------------------------------------*
           move      spaces               to   v-pfk (19)             .
      *              *-------------------------------------------------*
      *              * Tasto di funzione Do   : sempre disabilitato se *
      *              * in impostazione testata, altrimenti inalterato  *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    "T"
                     move  spaces         to   v-pfk (05)             .
      *              *-------------------------------------------------*
      *              * Tasti di funzione Insr : sempre disabilitato se *
      *              * in impostazione primo campo riga corpo, altri-  *
      *              * menti inalterato                                *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    "C"
                     move  spaces         to   v-pfk (04)             .
      *              *-------------------------------------------------*
      *              * Tasti di funzione Remv : sempre disabilitato se *
      *              * in impostazione primo campo riga corpo, altri-  *
      *              * menti inalterato                                *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    "C"
                     move  spaces         to   v-pfk (06)             .
      *              *-------------------------------------------------*
      *              * Esecuzione accettazione                         *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       exe-acc-cmp-999.
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
      *    * Routine pre-esecuzione programma                          *
      *    *-----------------------------------------------------------*
       pre-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Visualizzazione titolo programma                *
      *              *-------------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
           move      "#"                  to   w-cnt-sts-vis-tit      .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * Open sottoprogramma gestione file relative di   *
      *              * supporto                                        *
      *              *-------------------------------------------------*
           move      "OP"                 to   w-rlt-sup-ope          .
           perform   cll-rlt-sup-000      thru cll-rlt-sup-999        .
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
           move      "OP"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
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
      *              * [lgt]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgt                 .
      *              *-------------------------------------------------*
      *              * [lgr]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgr                 .
      *              *-------------------------------------------------*
      *              * [lgv]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgv                 .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice prodotto 'dcp'  *
      *              *-------------------------------------------------*
           perform   cod-cod-dcp-opn-000  thru cod-cod-dcp-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice semilavorato    *
      *              *-------------------------------------------------*
           perform   cod-cod-dps-opn-000  thru cod-cod-dps-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice sub-distinta    *
      *              *-------------------------------------------------*
           perform   cod-cod-lgv-opn-000  thru cod-cod-lgv-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice materia prima   *
      *              *-------------------------------------------------*
           perform   cod-cod-dpm-opn-000  thru cod-cod-dpm-opn-999    .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * Close sottoprogramma gestione file relative di  *
      *              * supporto                                        *
      *              *-------------------------------------------------*
           move      "CL"                 to   w-rlt-sup-ope          .
           perform   cll-rlt-sup-000      thru cll-rlt-sup-999        .
      *              *-------------------------------------------------*
      *              * Cancellazione dello stesso sottoprogramma       *
      *              *-------------------------------------------------*
           perform   cnc-sub-cat-000      thru cnc-sub-cat-999        .
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
           move      "CL"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
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
      *              * [lgt]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgt                 .
      *              *-------------------------------------------------*
      *              * [lgr]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgr                 .
      *              *-------------------------------------------------*
      *              * [lgv]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgv                 .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice prodotto 'dcp' *
      *              *-------------------------------------------------*
           perform   cod-cod-dcp-cls-000  thru cod-cod-dcp-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice semilavorato   *
      *              *-------------------------------------------------*
           perform   cod-cod-dps-cls-000  thru cod-cod-dps-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice sub-distinta   *
      *              *-------------------------------------------------*
           perform   cod-cod-lgv-cls-000  thru cod-cod-lgv-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice materia prima  *
      *              *-------------------------------------------------*
           perform   cod-cod-dpm-cls-000  thru cod-cod-dpm-cls-999    .
       rou-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per gestione file relative di     *
      *    * supporto                                                  *
      *    *-----------------------------------------------------------*
       cll-rlt-sup-000.
           move      "pgm/dtp/prg/obj/pdtp3102"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using w-rlt-sup              .
       cll-rlt-sup-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione sottoprogramma per gestione file relative   *
      *    * di supporto                                               *
      *    *-----------------------------------------------------------*
       cnc-sub-cat-000.
           move      "pgm/dtp/prg/obj/pdtp3102"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
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
                                               w-cnt-sts-imp-pie      .
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
      *                  * Flag di primo passaggio in impostazione dei *
      *                  * campi chiave                                *
      *                  *---------------------------------------------*
           perform   acc-flg-uno-000      thru acc-flg-uno-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
       acc-key-reg-200.
      *                  *---------------------------------------------*
      *                  * A controllo finale                          *
      *                  *---------------------------------------------*
           go to     acc-key-reg-900.
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
                     go to acc-key-reg-200.
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
       pmt-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Flag di primo passaggio in imposta-  *
      *    *                      zione campi chiave                   *
      *    *-----------------------------------------------------------*
       acc-flg-uno-000.
      *              *-------------------------------------------------*
      *              * Se si e' al primo passaggio si pone in On il    *
      *              * flag di primo passaggio e si esce con Ok.       *
      *              * Altrimenti si esce per Exit                     *
      *              *-------------------------------------------------*
           if        w-tes-flg-uno        =    spaces
                     move  "#"            to   w-tes-flg-uno
                     move  "S"            to   w-cnt-tus-acc-key
           else      move  "E"            to   w-cnt-tus-acc-key       .
       acc-flg-uno-999.
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
      *              * Assestamento status di impostazione per testa-  *
      *              * ta e corpo                                      *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-tes      .
           move      all   "#"            to   w-cnt-sts-imp-pte      .
           move      all   "#"            to   w-cnt-sts-ing-pte      .
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
           if        w-cnt-sts-imp-ptx
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
      *                  * Video in On                                 *
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
       acc-nok-reg-240.
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
                     w-cnt-tus-acc-cor    =    "E"
                     move  w-cnt-tus-acc-cor
                                          to   w-cnt-tus-acc-nok
                     go to acc-nok-reg-999.
      *                  *---------------------------------------------*
      *                  * Se tipo uscita "-"                          *
      *                  *---------------------------------------------*
           if        w-cnt-tus-acc-cor    =    "-"
                     go to acc-nok-reg-240.
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
      *                  * Messaggio ed accettazione                   *
      *                  *---------------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           if        w-cnt-sts-imp-tes    not  = spaces and
                     w-cnt-sts-imp-cor    not  = spaces and
                     w-cnt-sts-imp-pie    not  = spaces
                     move  "Conferma esecuzione (S/N/E) ?"
                                          to   v-not
           else      move  "Conferma esecuzione (N/E) ?"
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
      *                      * Nr riga corpo da accettare : max        *
      *                      *-----------------------------------------*
           move      w-rlt-sup-max        to   w-cnt-cor-nrg-dac      .
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
       acc-tes-reg-010.
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
      *              *-------------------------------------------------*
      *              * Pagina 1                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Componente da eliminare, tipo               *
      *                  *---------------------------------------------*
           perform   acc-cde-tip-000      thru acc-cde-tip-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-110.
      *                  *---------------------------------------------*
      *                  * Componente da eliminare, codice             *
      *                  *---------------------------------------------*
           perform   acc-cde-cod-000      thru acc-cde-cod-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-100.
       acc-tes-reg-190.
      *                  *---------------------------------------------*
      *                  * Presa visione per la pagina                 *
      *                  *---------------------------------------------*
           perform   acc-pre-vpg-000      thru acc-pre-vpg-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-100.
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
                     depending            on   w-cnt-sts-imp-npt      .
           go to     vis-tes-reg-999.
       vis-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Pagina 1                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Componente da eliminare, tipo               *
      *                  *---------------------------------------------*
           perform   vis-cde-tip-000      thru vis-cde-tip-999        .
      *                  *---------------------------------------------*
      *                  * Componente da eliminare, codice             *
      *                  *---------------------------------------------*
           perform   vis-cde-cod-000      thru vis-cde-cod-999        .
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
      *              * Erase linee impegnate dalla testata             *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      04                   to   v-lin                  .
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
      *              * Pagina 1                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Componente da eliminare, titolo             *
      *                  *---------------------------------------------*
           perform   pmt-cde-tit-000      thru pmt-cde-tit-999        .
      *                  *---------------------------------------------*
      *                  * Componente da eliminare, tipo               *
      *                  *---------------------------------------------*
           perform   pmt-cde-tip-000      thru pmt-cde-tip-999        .
      *                  *---------------------------------------------*
      *                  * Componente da eliminare, codice             *
      *                  *---------------------------------------------*
           perform   pmt-cde-cod-000      thru pmt-cde-cod-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-tes-reg-999.
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Prompt per componente da eliminare, titolo                *
      *    *-----------------------------------------------------------*
       pmt-cde-tit-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "          ----------------------------------------
      -              "--------------------          "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                      Componente che deve essere e
      -              "liminato                      "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "          ----------------------------------------
      -              "--------------------          "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cde-tit-999.
           exit.

      *    *===========================================================*
      *    * Prompt per componente da eliminare, tipo                  *
      *    *-----------------------------------------------------------*
       pmt-cde-tip-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo di componente     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cde-tip-999.
           exit.

      *    *===========================================================*
      *    * Prompt per componente da eliminare, codice                *
      *    *-----------------------------------------------------------*
       pmt-cde-cod-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           if        w-tes-cde-tip        =    "M"
                     move  "Codice materia prima   :"
                                          to   v-alf
           else if   w-tes-cde-tip        =    "S"
                     move  "Codice semilavorato    :"
                                          to   v-alf
           else if   w-tes-cde-tip        =    "D"
                     move  "Codice sub-distinta    :"
                                          to   v-alf
           else      move  "Codice                 :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cde-cod-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Componente da eliminare, tipo              *
      *    *-----------------------------------------------------------*
       acc-cde-tip-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cde-tip-025.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-cde-tip        to   w-sav-cde-tip          .
       acc-cde-tip-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-cde-tip-lun    to   v-car                  .
           move      w-exp-cde-tip-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      26                   to   v-pos                  .
           move      w-exp-cde-tip-tbl    to   v-txt                  .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-tes-cde-tip        =    "M"
                     move  01             to   v-num
           else if   w-tes-cde-tip        =    "S"
                     move  02             to   v-num
           else if   w-tes-cde-tip        =    "D"
                     move  03             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cde-tip-999.
       acc-cde-tip-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "M"            to   w-tes-cde-tip
           else if   v-num                =    02
                     move  "S"            to   w-tes-cde-tip
           else if   v-num                =    03
                     move  "D"            to   w-tes-cde-tip
           else      move  spaces         to   w-tes-cde-tip          .
       acc-cde-tip-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cde-tip-425.
      *                  *---------------------------------------------*
      *                  * Valore a spaces non ammesso, a meno che non *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        w-tes-cde-tip        not  = spaces
                     go to acc-cde-tip-450.
           if        v-key                =    "UP  "
                     go to acc-cde-tip-450
           else      go to acc-cde-tip-600.
       acc-cde-tip-450.
      *                  *---------------------------------------------*
      *                  * Fine controlli                              *
      *                  *---------------------------------------------*
           go to     acc-cde-tip-600.
       acc-cde-tip-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cde-tip-620.
      *                  *---------------------------------------------*
      *                  * Se valore immutato rispetto al valore pre-  *
      *                  * cedente : nessuna dipendenza                *
      *                  *---------------------------------------------*
           if        w-tes-cde-tip        =    w-sav-cde-tip
                     go to acc-cde-tip-800.
       acc-cde-tip-640.
      *                  *---------------------------------------------*
      *                  * Se valore precedente indeterminato : solo   *
      *                  * visualizzazione prompt per codice compo-    *
      *                  * nente                                       *
      *                  *---------------------------------------------*
           if        w-sav-cde-tip        not  = spaces
                     go to acc-cde-tip-660.
           perform   pmt-cde-cod-000      thru pmt-cde-cod-999        .
           go to     acc-cde-tip-800.
       acc-cde-tip-660.
      *                  *---------------------------------------------*
      *                  * Normalizzazione per codice componente       *
      *                  *---------------------------------------------*
           move      zero                 to   w-tes-cde-num          .
           move      spaces               to   w-tes-cde-alf          .
           move      spaces               to   w-tes-cde-des          .
           move      spaces               to   w-tes-cde-umi          .
           move      zero                 to   w-tes-cde-dec          .
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt per codice componen- *
      *                  * te                                          *
      *                  *---------------------------------------------*
           perform   pmt-cde-cod-000      thru pmt-cde-cod-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazioni per codice componente       *
      *                  *---------------------------------------------*
           perform   vis-cde-cod-000      thru vis-cde-cod-999        .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cde-tip-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-cde-tip-999.
       acc-cde-tip-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazzione : Componente da eliminare, tipo          *
      *    *-----------------------------------------------------------*
       vis-cde-tip-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-cde-tip-lun    to   v-car                  .
           move      w-exp-cde-tip-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      26                   to   v-pos                  .
           move      w-exp-cde-tip-tbl    to   v-txt                  .
           if        w-tes-cde-tip        =    "M"
                     move  01             to   v-num
           else if   w-tes-cde-tip        =    "S"
                     move  02             to   v-num
           else if   w-tes-cde-tip        =    "D"
                     move  03             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cde-tip-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Componente da eliminare, codice            *
      *    *-----------------------------------------------------------*
       acc-cde-cod-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo componente        *
      *              *-------------------------------------------------*
           if        w-tes-cde-tip        =    "M"
                     go to acc-cde-cod-100
           else if   w-tes-cde-tip        =    "S"
                     go to acc-cde-cod-200
           else if   w-tes-cde-tip        =    "D"
                     go to acc-cde-cod-300
           else      go to acc-cde-cod-900.
       acc-cde-cod-100.
      *              *-------------------------------------------------*
      *              * Se tipo componente : Materia prima              *
      *              *-------------------------------------------------*
           perform   acc-cde-dpm-000      thru acc-cde-dpm-999        .
           go to     acc-cde-cod-999.
       acc-cde-cod-200.
      *              *-------------------------------------------------*
      *              * Se tipo componente : Semilavorato               *
      *              *-------------------------------------------------*
           perform   acc-cde-dps-000      thru acc-cde-dps-999        .
           go to     acc-cde-cod-999.
       acc-cde-cod-300.
      *              *-------------------------------------------------*
      *              * Se tipo componente : Subdistinta virtuale       *
      *              *-------------------------------------------------*
           perform   acc-cde-lgv-000      thru acc-cde-lgv-999        .
           go to     acc-cde-cod-999.
       acc-cde-cod-900.
      *              *-------------------------------------------------*
      *              * Se tipo distinta indeterminato                  *
      *              *-------------------------------------------------*
           go to     acc-cde-cod-999.
       acc-cde-cod-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Componente da eliminare, codice materia    *
      *    *                prima                                      *
      *    *-----------------------------------------------------------*
       acc-cde-dpm-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cde-dpm-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-dpm-ope      .
           move      "A"                  to   w-cod-cod-dpm-tac      .
           move      w-tes-cde-num        to   w-cod-cod-dpm-num      .
           move      w-tes-cde-alf        to   w-cod-cod-dpm-alf      .
           move      11                   to   w-cod-cod-dpm-lin      .
           move      26                   to   w-cod-cod-dpm-pos      .
           move      11                   to   w-cod-cod-dpm-dln      .
           move      41                   to   w-cod-cod-dpm-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           perform   cod-cod-dpm-cll-000  thru cod-cod-dpm-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-dpm-foi-000  thru cod-cod-dpm-foi-999    .
       acc-cde-dpm-110.
           perform   cod-cod-dpm-cll-000  thru cod-cod-dpm-cll-999    .
           if        w-cod-cod-dpm-ope    =    "F+"
                     go to acc-cde-dpm-115.
           if        w-cod-cod-dpm-ope    =    "AC"
                     go to acc-cde-dpm-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cde-dpm-115.
           perform   cod-cod-dpm-foi-000  thru cod-cod-dpm-foi-999    .
           go to     acc-cde-dpm-110.
       acc-cde-dpm-120.
           move      w-cod-cod-dpm-num    to   v-num                  .
           move      w-cod-cod-dpm-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cde-dpm-999.
       acc-cde-dpm-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cde-num          .
           move      v-alf                to   w-tes-cde-alf          .
       acc-cde-dpm-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cde-dpm-420.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [dpm]                      *
      *                  *---------------------------------------------*
           move      w-tes-cde-num        to   w-let-arc-dpm-num      .
           perform   let-arc-dpm-000      thru let-arc-dpm-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione, unita' di mi-   *
      *                  * sura, e decimali quantita', relativi al     *
      *                  * componente                                  *
      *                  *---------------------------------------------*
           move      w-let-arc-dpm-des    to   w-tes-cde-des          .
           move      w-let-arc-dpm-umi    to   w-tes-cde-umi          .
           move      w-let-arc-dpm-deq    to   w-tes-cde-dec          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-cde-des-000      thru vis-cde-des-999        .
      *                  *---------------------------------------------*
      *                  * Se valore non trovato : reimpostazione      *
      *                  *---------------------------------------------*
           if        w-let-arc-dpm-flg    not  = spaces
                     go to acc-cde-dpm-100.
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione a meno  *
      *                  * che non sia stato premuto 'Up'              *
      *                  *---------------------------------------------*
           if        w-tes-cde-alf        not  = spaces
                     go to acc-cde-dpm-440.
           if        v-key                =    "UP  "
                     go to acc-cde-dpm-600
           else      go to acc-cde-dpm-100.
       acc-cde-dpm-440.
      *                  *---------------------------------------------*
      *                  * Fine controlli                              *
      *                  *---------------------------------------------*
           go to     acc-cde-dpm-600.
       acc-cde-dpm-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cde-dpm-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-cde-dpm-999.
       acc-cde-dpm-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Componente da eliminare, codice semila-    *
      *    *                vorato                                     *
      *    *-----------------------------------------------------------*
       acc-cde-dps-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cde-dps-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-dps-ope      .
           move      "A"                  to   w-cod-cod-dps-tac      .
           move      w-tes-cde-num        to   w-cod-cod-dps-num      .
           move      w-tes-cde-alf        to   w-cod-cod-dps-alf      .
           move      11                   to   w-cod-cod-dps-lin      .
           move      26                   to   w-cod-cod-dps-pos      .
           move      11                   to   w-cod-cod-dps-dln      .
           move      41                   to   w-cod-cod-dps-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           perform   cod-cod-dps-cll-000  thru cod-cod-dps-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-dps-foi-000  thru cod-cod-dps-foi-999    .
       acc-cde-dps-110.
           perform   cod-cod-dps-cll-000  thru cod-cod-dps-cll-999    .
           if        w-cod-cod-dps-ope    =    "F+"
                     go to acc-cde-dps-115.
           if        w-cod-cod-dps-ope    =    "AC"
                     go to acc-cde-dps-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cde-dps-115.
           perform   cod-cod-dps-foi-000  thru cod-cod-dps-foi-999    .
           go to     acc-cde-dps-110.
       acc-cde-dps-120.
           move      w-cod-cod-dps-num    to   v-num                  .
           move      w-cod-cod-dps-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cde-dps-999.
       acc-cde-dps-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cde-num          .
           move      v-alf                to   w-tes-cde-alf          .
       acc-cde-dps-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cde-dps-420.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [dps]                      *
      *                  *---------------------------------------------*
           move      w-tes-cde-num        to   w-let-arc-dps-num      .
           perform   let-arc-dps-000      thru let-arc-dps-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione, unita' di mi-   *
      *                  * sura, e decimali quantita', relativi al     *
      *                  * componente                                  *
      *                  *---------------------------------------------*
           move      w-let-arc-dps-des    to   w-tes-cde-des          .
           move      w-let-arc-dps-umi    to   w-tes-cde-umi          .
           move      w-let-arc-dps-deq    to   w-tes-cde-dec          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-cde-des-000      thru vis-cde-des-999        .
      *                  *---------------------------------------------*
      *                  * Se valore non trovato : reimpostazione      *
      *                  *---------------------------------------------*
           if        w-let-arc-dps-flg    not  = spaces
                     go to acc-cde-dps-100.
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione a meno  *
      *                  * che non sia stato premuto 'Up'              *
      *                  *---------------------------------------------*
           if        w-tes-cde-alf        not  = spaces
                     go to acc-cde-dps-440.
           if        v-key                =    "UP  "
                     go to acc-cde-dps-600
           else      go to acc-cde-dps-100.
       acc-cde-dps-440.
      *                  *---------------------------------------------*
      *                  * Fine controlli                              *
      *                  *---------------------------------------------*
           go to     acc-cde-dps-600.
       acc-cde-dps-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cde-dps-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-cde-dps-999.
       acc-cde-dps-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Componente da eliminare, codice sub-       *
      *    *                distinta virtuale                          *
      *    *-----------------------------------------------------------*
       acc-cde-lgv-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cde-lgv-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-lgv-ope      .
           move      "A"                  to   w-cod-cod-lgv-tac      .
           move      w-tes-cde-num        to   w-cod-cod-lgv-num      .
           move      w-tes-cde-alf        to   w-cod-cod-lgv-alf      .
           move      11                   to   w-cod-cod-lgv-lin      .
           move      26                   to   w-cod-cod-lgv-pos      .
           move      11                   to   w-cod-cod-lgv-dln      .
           move      41                   to   w-cod-cod-lgv-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           perform   cod-cod-lgv-cll-000  thru cod-cod-lgv-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-lgv-foi-000  thru cod-cod-lgv-foi-999    .
       acc-cde-lgv-110.
           perform   cod-cod-lgv-cll-000  thru cod-cod-lgv-cll-999    .
           if        w-cod-cod-lgv-ope    =    "F+"
                     go to acc-cde-lgv-115.
           if        w-cod-cod-lgv-ope    =    "AC"
                     go to acc-cde-lgv-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cde-lgv-115.
           perform   cod-cod-lgv-foi-000  thru cod-cod-lgv-foi-999    .
           go to     acc-cde-lgv-110.
       acc-cde-lgv-120.
           move      w-cod-cod-lgv-num    to   v-num                  .
           move      w-cod-cod-lgv-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cde-lgv-999.
       acc-cde-lgv-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cde-num          .
           move      v-alf                to   w-tes-cde-alf          .
       acc-cde-lgv-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cde-lgv-420.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [lgv]                      *
      *                  *---------------------------------------------*
           move      w-tes-cde-num        to   w-let-arc-lgv-num      .
           perform   let-arc-lgv-000      thru let-arc-lgv-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione, unita' di mi-   *
      *                  * sura, e decimali quantita', relativi al     *
      *                  * componente                                  *
      *                  *---------------------------------------------*
           move      w-let-arc-lgv-des    to   w-tes-cde-des          .
           move      w-let-arc-lgv-umi    to   w-tes-cde-umi          .
           move      w-let-arc-lgv-deq    to   w-tes-cde-dec          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-cde-des-000      thru vis-cde-des-999        .
      *                  *---------------------------------------------*
      *                  * Se valore non trovato : reimpostazione      *
      *                  *---------------------------------------------*
           if        w-let-arc-lgv-flg    not  = spaces
                     go to acc-cde-lgv-100.
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione a meno  *
      *                  * che non sia stato premuto 'Up'              *
      *                  *---------------------------------------------*
           if        w-tes-cde-alf        not  = spaces
                     go to acc-cde-lgv-440.
           if        v-key                =    "UP  "
                     go to acc-cde-lgv-600
           else      go to acc-cde-lgv-100.
       acc-cde-lgv-440.
      *                  *---------------------------------------------*
      *                  * Fine controlli                              *
      *                  *---------------------------------------------*
           go to     acc-cde-lgv-600.
       acc-cde-lgv-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cde-lgv-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-cde-lgv-999.
       acc-cde-lgv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Componente da eliminare, codice compo-  *
      *    *                   nente                                   *
      *    *-----------------------------------------------------------*
       vis-cde-cod-000.
      *              *-------------------------------------------------*
      *              * Visualizzazione codice alfanumerico             *
      *              *-------------------------------------------------*
           perform   vis-cde-alf-000      thru vis-cde-alf-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione descrizione                     *
      *              *-------------------------------------------------*
           perform   vis-cde-des-000      thru vis-cde-des-999        .
       vis-cde-cod-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Componente da eliminare, codice alfa-   *
      *    *                   numerico                                *
      *    *-----------------------------------------------------------*
       vis-cde-alf-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      26                   to   v-pos                  .
           move      w-tes-cde-alf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cde-alf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Componente da eliminare, descrizione    *
      *    *-----------------------------------------------------------*
       vis-cde-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cde-des        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cde-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Presa visione per pagina     *
      *    *-----------------------------------------------------------*
       acc-pre-vpg-000.
      *              *-------------------------------------------------*
      *              * Se esiste anche una sola pagina attiva succes-  *
      *              * siva alla pagina attuale : uscita con status    *
      *              * di uscita a spaces                              *
      *              *-------------------------------------------------*
       acc-pre-vpg-010.
      *                  *---------------------------------------------*
      *                  * Salvataggio numero pagina attuale           *
      *                  *---------------------------------------------*
           move      w-cnt-sts-imp-npt    to   w-cnt-sts-imp-svp      .
       acc-pre-vpg-020.
      *                  *---------------------------------------------*
      *                  * Se all'ultima pagina : ripristino pagina    *
      *                  * salvata e proseguimento per controllo glo-  *
      *                  * bale su impostazioni                        *
      *                  *---------------------------------------------*
           if        w-cnt-sts-imp-npt    not  < w-cnt-sts-imp-mpt
                     move  w-cnt-sts-imp-svp
                                          to   w-cnt-sts-imp-npt
                     go to acc-pre-vpg-050.
      *                  *---------------------------------------------*
      *                  * Incremento numero pagina                    *
      *                  *---------------------------------------------*
           add       1                    to   w-cnt-sts-imp-npt      .
      *                  *---------------------------------------------*
      *                  * Test se pagina da trattare                  *
      *                  *---------------------------------------------*
           perform   snp-tes-reg-000      thru snp-tes-reg-999        .
      *                  *---------------------------------------------*
      *                  * Se si : ripristino pagina salvata ed usci-  *
      *                  * ta con status a spaces                      *
      *                  *---------------------------------------------*
           if        w-cnt-sts-imp-snp    =    spaces
                     move  spaces         to   w-cnt-tus-acc-tes
                     move  spaces         to   v-key
                     go to acc-pre-vpg-999.
      *                  *---------------------------------------------*
      *                  * Se no : a re-incremento                     *
      *                  *---------------------------------------------*
           go to     acc-pre-vpg-020.
       acc-pre-vpg-050.
      *              *-------------------------------------------------*
      *              * Controllo globale impostazioni e, se non supe-  *
      *              * rato, uscita come per Up                        *
      *              *-------------------------------------------------*
           perform   cnt-tdo-nok-000      thru cnt-tdo-nok-999        .
           if        w-cnt-tdo-nok-flg    not  = spaces
                     move  spaces         to   w-cnt-tus-acc-tes
                     move  "UP  "         to   v-key
                     go to acc-pre-vpg-999.
       acc-pre-vpg-100.
      *              *-------------------------------------------------*
      *              * Accettazione conferma da parte dell'utente      *
      *              *-------------------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      "#SAV"               to   v-not                  .
           move      spaces               to   v-alf                  .
           move      "SNE"                to   v-msk                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-pre-vpg-150.
      *              *-------------------------------------------------*
      *              * Preparazione function key se necessario         *
      *              *-------------------------------------------------*
           if        v-key                not  = spaces
                     go to acc-pre-vpg-200.
           if        v-alf                =    "S"
                     move   "DO  "        to   v-key
           else if   v-alf                =    "E"
                     move   "EXIT"        to   v-key
           else if   v-alf                =    "N"
                     move   "UP  "        to   v-key                  .
       acc-pre-vpg-200.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda della function key         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-pre-vpg-250
           else if   v-key                =    "UP  "
                     go to acc-pre-vpg-300
           else if   v-key                =    "DO  "
                     go to acc-pre-vpg-400
           else      go to acc-pre-vpg-100.
       acc-pre-vpg-250.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           move      "E"                  to   w-cnt-tus-acc-tes      .
           move      "EXIT"               to   v-key                  .
           go to     acc-pre-vpg-999.
       acc-pre-vpg-300.
      *              *-------------------------------------------------*
      *              * Se Up                                           *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tus-acc-tes      .
           move      "UP  "               to   v-key                  .
           go to     acc-pre-vpg-999.
       acc-pre-vpg-400.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
       acc-pre-vpg-450.
      *                  *---------------------------------------------*
      *                  * Richiamo routine di caricamento iniziale    *
      *                  * degli elementi da trattare, e deviazione    *
      *                  * a seconda dello status di uscita            *
      *                  *---------------------------------------------*
           perform   rou-car-ini-000      thru rou-car-ini-999        .
           if        w-cnt-rou-car-ini    =    spaces
                     go to acc-pre-vpg-500
           else      go to acc-pre-vpg-550.
       acc-pre-vpg-500.
      *                  *---------------------------------------------*
      *                  * Se esito della routine Ok                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Status di uscita a continuazione        *
      *                      *-----------------------------------------*
           move      spaces               to   w-cnt-tus-acc-tes      .
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     acc-pre-vpg-999.
       acc-pre-vpg-550.
      *                  *---------------------------------------------*
      *                  * Se esito della routine ad errore            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Status di uscita a : errore             *
      *                      *-----------------------------------------*
           move      "E"                  to   w-cnt-tus-acc-tes      .
           move      "EXIT"               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     acc-pre-vpg-999.
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
           if        w-cnt-cor-nrg-dac    =    zero       or
                     w-cnt-cor-nrg-dac    >    w-rlt-sup-max
                     move  "+"            to   w-cnt-tus-acc-cor
                     go to acc-cor-reg-990.
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
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
      *              * Video in On                                     *
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
      *              *-------------------------------------------------*
      *              * Flag di status impostione riga attuale          *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-rig      .
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
      *                  * Accettazione Si/No eliminazione             *
      *                  *---------------------------------------------*
           perform   acc-snx-eli-000      thru acc-snx-eli-999        .
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
       acc-cor-reg-800.
      *              *-------------------------------------------------*
      *              * Controllo finale su riga impostata              *
      *              *-------------------------------------------------*
           perform   cnt-tdo-rig-000      thru cnt-tdo-rig-999        .
           if        w-cnt-tdo-rig-flg    not  = spaces
                     move  spaces         to   w-cnt-tdo-rig-flg
                     move  "UP  "         to   v-key
                     go to acc-cor-reg-100.
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
      *                      * Visualizzazione numero riga             *
      *                      *-----------------------------------------*
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
      *                      * Update riga su file relative di suppor- *
      *                      * to                                      *
      *                      *-----------------------------------------*
           move      "UP"                 to   w-rlt-sup-ope          .
           move      w-cnt-cor-nrg-dac    to   w-rlt-sup-prg          .
           move      w-rig                to   w-rlt-sup-buf          .
           perform   cll-rlt-sup-000      thru cll-rlt-sup-999        .
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
      *                      * Visualizzazione del numero riga         *
      *                      *-----------------------------------------*
           move      "N"                  to   w-cnt-cor-tvi-rig      .
           move      w-cnt-cor-nrg-dac    to   w-cnt-cor-nrg-dav      .
           perform   vis-lin-cor-000      thru vis-lin-cor-999        .
      *                      *-----------------------------------------*
      *                      * Se si e' sulla riga numero 1            *
      *                      *-----------------------------------------*
           if        w-cnt-cor-nrg-dac    =    1
                     go to acc-cor-reg-000.
      *                      *-----------------------------------------*
      *                      * Se non si e' sulla riga numero 1        *
      *                      *-----------------------------------------*
           subtract  1                    from w-cnt-cor-nrg-dac      .
           go to     acc-cor-reg-000.
       acc-cor-reg-940.
      *                  *---------------------------------------------*
      *                  * Se Down da primo campo di impostazione riga *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Visualizzazione del numero riga         *
      *                      *-----------------------------------------*
           move      "N"                  to   w-cnt-cor-tvi-rig      .
           move      w-cnt-cor-nrg-dac    to   w-cnt-cor-nrg-dav      .
           perform   vis-lin-cor-000      thru vis-lin-cor-999        .
      *                      *-----------------------------------------*
      *                      * Incremento del numero riga              *
      *                      *-----------------------------------------*
           add       1                    to   w-cnt-cor-nrg-dac      .
           go to     acc-cor-reg-000.
       acc-cor-reg-945.
      *                  *---------------------------------------------*
      *                  * Se Do da primo campo di impostazione riga   *
      *                  *---------------------------------------------*
           move      "S"                  to   w-cnt-tus-acc-cor      .
           go to     acc-cor-reg-990.
       acc-cor-reg-965.
      *                  *---------------------------------------------*
      *                  * Se Tab da primo campo di impostazione riga  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Visualizzazione del numero riga         *
      *                      *-----------------------------------------*
           move      "N"                  to   w-cnt-cor-tvi-rig      .
           move      w-cnt-cor-nrg-dac    to   w-cnt-cor-nrg-dav      .
           perform   vis-lin-cor-000      thru vis-lin-cor-999        .
      *                      *-----------------------------------------*
      *                      * Ad accettazione riga nr. max            *
      *                      *-----------------------------------------*
           move      w-rlt-sup-max        to   w-cnt-cor-nrg-dac      .
           go to     acc-cor-reg-000.
       acc-cor-reg-970.
      *                  *---------------------------------------------*
      *                  * Se Back da primo campo di impostazione riga *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Visualizzazione del numero riga         *
      *                      *-----------------------------------------*
           move      "N"                  to   w-cnt-cor-tvi-rig      .
           move      w-cnt-cor-nrg-dac    to   w-cnt-cor-nrg-dav      .
           perform   vis-lin-cor-000      thru vis-lin-cor-999        .
      *                      *-----------------------------------------*
      *                      * Ad accettazione riga nr. 1              *
      *                      *-----------------------------------------*
           move      1                    to   w-cnt-cor-nrg-dac      .
           go to     acc-cor-reg-000.
       acc-cor-reg-975.
      *                  *---------------------------------------------*
      *                  * Se Nxsc da primo campo di impostazione riga *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Visualizzazione del numero riga         *
      *                      *-----------------------------------------*
           move      "N"                  to   w-cnt-cor-tvi-rig      .
           move      w-cnt-cor-nrg-dac    to   w-cnt-cor-nrg-dav      .
           perform   vis-lin-cor-000      thru vis-lin-cor-999        .
      *                      *-----------------------------------------*
      *                      * Determinazione numero d'ordine della    *
      *                      * prima riga appartenente alla pagina     *
      *                      * attuale                                 *
      *                      *-----------------------------------------*
           move      w-cnt-cor-nrg-dac    to   w-cnt-wrk-ctr-001      .
           subtract  1                    from w-cnt-wrk-ctr-001      .
           divide    w-lin-num-lin-vis    into w-cnt-wrk-ctr-001      .
           multiply  w-lin-num-lin-vis    by   w-cnt-wrk-ctr-001      .
           add       1                    to   w-cnt-wrk-ctr-001      .
      *                      *-----------------------------------------*
      *                      * Determinazione numero d'ordine dell'ul- *
      *                      * tima riga appartenente alla pagina      *
      *                      * attuale                                 *
      *                      *-----------------------------------------*
           move      w-cnt-wrk-ctr-001    to   w-cnt-wrk-ctr-002      .
           add       w-lin-num-lin-vis    to   w-cnt-wrk-ctr-002      .
           subtract  1                    from w-cnt-wrk-ctr-002      .
      *                      *-----------------------------------------*
      *                      * Determinazione numero d'ordine della    *
      *                      * prima riga appartenente alla pagina     *
      *                      * successiva a quella attuale             *
      *                      *-----------------------------------------*
           move      w-cnt-wrk-ctr-002    to   w-cnt-wrk-ctr-003      .
           add       1                    to   w-cnt-wrk-ctr-003      .
      *                      *-----------------------------------------*
      *                      * Se il numero d'ordine della prima riga  *
      *                      * appartenente alla pagina successiva a   *
      *                      * quella attuale e' maggiore del max :    *
      *                      * tipo uscita "+" e si esce               *
      *                      *-----------------------------------------*
           if        w-cnt-wrk-ctr-003    >    w-rlt-sup-max
                     move    "+"          to   w-cnt-tus-acc-cor
                     go to acc-cor-reg-990.
      *                      *-----------------------------------------*
      *                      * Altrimenti si va' a quella riga         *
      *                      *-----------------------------------------*
           move      w-cnt-wrk-ctr-003    to   w-cnt-cor-nrg-dac      .
           go to     acc-cor-reg-000.
       acc-cor-reg-980.
      *                  *---------------------------------------------*
      *                  * Se Prsc da primo campo di impostazione riga *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Visualizzazione del numero riga         *
      *                      *-----------------------------------------*
           move      "N"                  to   w-cnt-cor-tvi-rig      .
           move      w-cnt-cor-nrg-dac    to   w-cnt-cor-nrg-dav      .
           perform   vis-lin-cor-000      thru vis-lin-cor-999        .
      *                      *-----------------------------------------*
      *                      * Determinazione numero d'ordine della    *
      *                      * prima riga appartenente alla pagina     *
      *                      * attuale                                 *
      *                      *-----------------------------------------*
           move      w-cnt-cor-nrg-dac    to   w-cnt-wrk-ctr-001      .
           subtract  1                    from w-cnt-wrk-ctr-001      .
           divide    w-lin-num-lin-vis    into w-cnt-wrk-ctr-001      .
           multiply  w-lin-num-lin-vis    by   w-cnt-wrk-ctr-001      .
           add       1                    to   w-cnt-wrk-ctr-001      .
      *                      *-----------------------------------------*
      *                      * Determinazione numero d'ordine dell'ul- *
      *                      * tima riga appartenente alla pagina      *
      *                      * attuale                                 *
      *                      *-----------------------------------------*
           move      w-cnt-wrk-ctr-001    to   w-cnt-wrk-ctr-002      .
           add       w-lin-num-lin-vis    to   w-cnt-wrk-ctr-002      .
           subtract  1                    from w-cnt-wrk-ctr-002      .
      *                      *-----------------------------------------*
      *                      * Determinazione numero d'ordine della    *
      *                      * prima riga appartenente alla pagina     *
      *                      * precedente rispetto a quella attuale    *
      *                      *-----------------------------------------*
           move      w-cnt-wrk-ctr-001    to   w-cnt-wrk-ctr-003      .
           subtract  w-lin-num-lin-vis    from w-cnt-wrk-ctr-003      .
      *                      *-----------------------------------------*
      *                      * Se si e' sulla prima pagina si va' al-  *
      *                      * la stessa linea in cui si e' gia'       *
      *                      *-----------------------------------------*
           if        w-cnt-wrk-ctr-001    =    1
                     go to acc-cor-reg-000.
      *                      *-----------------------------------------*
      *                      * Altrimenti si va' alla prima riga della *
      *                      * pagina precedente                       *
      *                      *-----------------------------------------*
           move      w-cnt-wrk-ctr-003    to   w-cnt-cor-nrg-dac      .
           go to     acc-cor-reg-000.
       acc-cor-reg-985.
      *                  *---------------------------------------------*
      *                  * Se Slct da primo campo di impostazione riga *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Visualizzazione del numero riga         *
      *                      *-----------------------------------------*
           move      "N"                  to   w-cnt-cor-tvi-rig      .
           move      w-cnt-cor-nrg-dac    to   w-cnt-cor-nrg-dav      .
           perform   vis-lin-cor-000      thru vis-lin-cor-999        .
      *                      *-----------------------------------------*
      *                      * Ad accettazione nr riga selezionato     *
      *                      *-----------------------------------------*
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
      *                  *---------------------------------------------*
      *                  * Se visualizzazione indicatore linea         *
      *                  *---------------------------------------------*
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
      *                  *---------------------------------------------*
      *                  * Se visualizzazione numero linea a spaces    *
      *                  *---------------------------------------------*
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
           move      w-cnt-cor-nrg-dav    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-lin-imm-num-lin      .
           move      05                   to   v-car                  .
           go to     vis-lin-cor-900.
       vis-lin-cor-400.
      *                  *---------------------------------------------*
      *                  * Se visualizzazione riga intera              *
      *                  *---------------------------------------------*
       vis-lin-cor-420.
      *                      *-----------------------------------------*
      *                      * Editing Numero d'ordine riga            *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      spaces               to   v-edm                  .
           move      w-cnt-cor-nrg-dav    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-lin-imm-num-lin      .
       vis-lin-cor-440.
      *                      *-----------------------------------------*
      *                      * Parentesi quadra aperta                 *
      *                      *-----------------------------------------*
           move      "["                  to   w-lin-imm-pqu-ape      .
       vis-lin-cor-460.
      *                      *-----------------------------------------*
      *                      * Tipo assieme                            *
      *                      *-----------------------------------------*
           if        w-rig-lgr-tpm-ass    =    01
                     move  "P"            to   w-lin-imm-tip-ass
           else if   w-rig-lgr-tpm-ass    =    02
                     move  "S"            to   w-lin-imm-tip-ass
           else if   w-rig-lgr-tpm-ass    =    03
                     move  "M"            to   w-lin-imm-tip-ass
           else if   w-rig-lgr-tpm-ass    =    04
                     move  "V"            to   w-lin-imm-tip-ass
           else if   w-rig-lgr-tpm-ass    =    99
                     move  "D"            to   w-lin-imm-tip-ass
           else      move  "?"            to   w-lin-imm-tip-ass      .
       vis-lin-cor-480.
      *                      *-----------------------------------------*
      *                      * Parentesi quadra chiusa                 *
      *                      *-----------------------------------------*
           move      "]"                  to   w-lin-imm-pqu-chi      .
       vis-lin-cor-500.
      *                      *-----------------------------------------*
      *                      * Codice assieme                          *
      *                      *-----------------------------------------*
           move      w-rig-ass-afm-ass    to   w-lin-imm-cod-ass      .
       vis-lin-cor-520.
      *                      *-----------------------------------------*
      *                      * Descrizione assieme                     *
      *                      *-----------------------------------------*
           move      w-rig-ass-des-ass    to   w-lin-imm-des-ass      .
       vis-lin-cor-540.
      *                      *-----------------------------------------*
      *                      * Si/No eliminazione                      *
      *                      *-----------------------------------------*
           if        w-rig-acc-snx-eli    =    "N"
                     go to vis-lin-cor-544.
       vis-lin-cor-542.
           if        w-rig-acc-snx-emo    =    "N"
                     move  "Si ***   "    to   w-lin-imm-lit-snx
           else      move  "Si       "    to   w-lin-imm-lit-snx      .
           go to     vis-lin-cor-546.
       vis-lin-cor-544.
           if        w-rig-acc-snx-emo    =    "N"
                     move  "   *** No"    to   w-lin-imm-lit-snx
           else      move  "       No"    to   w-lin-imm-lit-snx      .
           go to     vis-lin-cor-546.
       vis-lin-cor-546.
           go to     vis-lin-cor-800.
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
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cor-reg-100.
      *              *-------------------------------------------------*
      *              * Intestazione                                    *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "        Distinte base in cui il componente da elim
      -              "inare viene utilizzato        "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "--------------------------------------------------
      -              "------------------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      " N.R   Tipo      Codice                    Descriz
      -              "ione                 Eliminaz."
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "-----  ----  --------------  ---------------------
      -              "-------------------  ---------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cor-reg-200.
      *              *-------------------------------------------------*
      *              * Note operative in basso                         *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "--------------------------------------------------
      -              "------------------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Note : Tasto funzione [1] = Si eliminazione    Ret
      -              "urn = Criterio di eliminazione"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "       Tasto funzione [2] = No eliminazione       
      -              "      inalterato              "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cor-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione riga corpo di accettazione in w-rig       *
      *    *-----------------------------------------------------------*
       vis-rig-cor-000.
       vis-rig-cor-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per riga corpo di accettazione    *
      *    *-----------------------------------------------------------*
       pmt-rig-cor-000.
       pmt-rig-cor-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Si/No eliminazione componente              *
      *    *-----------------------------------------------------------*
       acc-snx-eli-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-snx-eli-025.
      *                  *---------------------------------------------*
      *                  * Determinazione numero linea a video, e nu-  *
      *                  * mero facciata di impostazione corpo, per    *
      *                  * trattamento del corpo direttamente nelle    *
      *                  * righe di scroll                             *
      *                  *---------------------------------------------*
           move      w-cnt-cor-nrg-dac    to   w-num-lin-scr-rig      .
           perform   num-lin-scr-000      thru num-lin-scr-999        .
       acc-snx-eli-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Parametri generici                          *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      w-num-lin-scr-lin    to   v-lin                  .
           move      12                   to   v-pos                  .
      *                  *---------------------------------------------*
      *                  * Tasti funzione                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Up   : sempre ammesso, a meno che non   *
      *                      *        si sia alla prima riga           *
      *                      *-----------------------------------------*
           if        w-cnt-cor-nrg-dac    =    1
                     move  spaces         to   v-pfk (01)
           else      move  "UP  "         to   v-pfk (01)             .
      *                      *-----------------------------------------*
      *                      * Down : sempre ammesso                   *
      *                      *-----------------------------------------*
           move      "DOWN"               to   v-pfk (02)             .
      *                      *-----------------------------------------*
      *                      * Find : mai ammesso, in quanto il campo  *
      *                      * non ammette il Find                     *
      *                      *-----------------------------------------*
           move      spaces               to   v-pfk (03)             .
      *                      *-----------------------------------------*
      *                      * Insr : mai ammesso                      *
      *                      *-----------------------------------------*
           move      spaces               to   v-pfk (04)             .
      *                      *-----------------------------------------*
      *                      * Do   : sempre ammesso                   *
      *                      *-----------------------------------------*
           move      "DO  "               to   v-pfk (05)             .
      *                      *-----------------------------------------*
      *                      * Remv : mai ammesso                      *
      *                      *-----------------------------------------*
           move      spaces               to   v-pfk (06)             .
      *                      *-----------------------------------------*
      *                      * Prsc : sempre ammesso, a meno che non   *
      *                      *        si sia alla prima facciata del   *
      *                      *        corpo                            *
      *                      *-----------------------------------------*
           if        w-num-lin-scr-nfa    =    1
                     move  spaces         to   v-pfk (07)
           else      move  "PRSC"         to   v-pfk (07)             .
      *                      *-----------------------------------------*
      *                      * Nxsc : sempre ammesso                   *
      *                      *-----------------------------------------*
           move      "NXSC"               to   v-pfk (08)             .
      *                      *-----------------------------------------*
      *                      * Back : sempre ammesso, a meno che non   *
      *                      *        si sia gia' sulla prima riga     *
      *                      *-----------------------------------------*
           if        w-cnt-cor-nrg-dac    =    1
                     move  spaces         to   v-pfk (09)
           else      move  "BACK"         to   v-pfk (09)             .
      *                      *-----------------------------------------*
      *                      * Tab  : sempre ammesso, a meno che non   *
      *                      *        si sia gia' sull'ultima riga     *
      *                      *-----------------------------------------*
           if        w-cnt-cor-nrg-dac    not  < w-rlt-sup-max
                     move  spaces         to   v-pfk (10)
           else      move  "TAB "         to   v-pfk (10)             .
      *                      *-----------------------------------------*
      *                      * Slct : mai ammesso, in quanto il campo  *
      *                      *        non puo' accettare valori        *
      *                      *-----------------------------------------*
           move      spaces               to   v-pfk (11)             .
      *                      *-----------------------------------------*
      *                      * Expd : mai ammesso                      *
      *                      *-----------------------------------------*
           move      spaces               to   v-pfk (12)             .
      *                      *-----------------------------------------*
      *                      * Pf1  : sempre ammesso, a meno che il    *
      *                      *        valore sia non modificabile      *
      *                      *-----------------------------------------*
           if        w-rig-acc-snx-emo    =    "N"
                     move  spaces         to   v-pfk (15)
           else      move  "[1] "         to   v-pfk (15)             .
      *                      *-----------------------------------------*
      *                      * Pf2  : sempre ammesso, a meno che il    *
      *                      *        valore sia non modificabile      *
      *                      *-----------------------------------------*
           if        w-rig-acc-snx-emo    =    "N"
                     move  spaces         to   v-pfk (16)
           else      move  "[2] "         to   v-pfk (16)             .
      *                      *-----------------------------------------*
      *                      * Richiamo subroutine di accettazione     *
      *                      *-----------------------------------------*
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-snx-eli-200.
      *              *-------------------------------------------------*
      *              * Se Return  o Pf1 o Pf2                          *
      *              *-------------------------------------------------*
           if        v-key                =    spaces or
                     v-key                =    "[1] " or
                     v-key                =    "[2] "
                     go to acc-snx-eli-400.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-rig
                     go to acc-snx-eli-999.
      *              *-------------------------------------------------*
      *              * Se Up                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "UP  "
                     move  "U"            to   w-cnt-tus-acc-rig
                     go to acc-snx-eli-999.
      *              *-------------------------------------------------*
      *              * Se Down                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DOWN"
                     move  "D"            to   w-cnt-tus-acc-rig
                     go to acc-snx-eli-999.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-rig
                             go to acc-snx-eli-999
                     else    go to acc-snx-eli-100.
      *              *-------------------------------------------------*
      *              * Se Tab                                          *
      *              *-------------------------------------------------*
           if        v-key                =    "TAB "
                     move  "T"            to   w-cnt-tus-acc-rig
                     go to acc-snx-eli-999.
      *              *-------------------------------------------------*
      *              * Se Back                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "BACK"
                     move  "B"            to   w-cnt-tus-acc-rig
                     go to acc-snx-eli-999.
      *              *-------------------------------------------------*
      *              * Se Nxsc                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "NXSC"
                     move  "N"            to   w-cnt-tus-acc-rig
                     go to acc-snx-eli-999.
      *              *-------------------------------------------------*
      *              * Se Prsc                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "PRSC"
                     move  "P"            to   w-cnt-tus-acc-rig
                     go to acc-snx-eli-999.
       acc-snx-eli-400.
      *              *-------------------------------------------------*
      *              * Se Return  o Pf1 o Pf2 o Pf3                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione della function-key   *
      *                  * impostata                                   *
      *                  *---------------------------------------------*
           if        v-key                =    spaces
                     go to acc-snx-eli-425
           else if   v-key                =    "[1] "
                     go to acc-snx-eli-450
           else if   v-key                =    "[2] "
                     go to acc-snx-eli-475
           else      go to acc-snx-eli-100.
       acc-snx-eli-425.
      *                  *---------------------------------------------*
      *                  * Se impostata function-key Return            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-snx-eli-600.
       acc-snx-eli-450.
      *                  *---------------------------------------------*
      *                  * Se impostata function-key Pf1               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Eliminazione : Si                       *
      *                      *-----------------------------------------*
           move      "S"                  to   w-rig-acc-snx-eli      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-snx-eli-600.
       acc-snx-eli-475.
      *                  *---------------------------------------------*
      *                  * Se impostata function-key Pf2               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Eliminazione : No                       *
      *                      *-----------------------------------------*
           move      "N"                  to   w-rig-acc-snx-eli      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-snx-eli-600.
       acc-snx-eli-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-eli-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
       acc-snx-eli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Si/No eliminazione componente           *
      *    *-----------------------------------------------------------*
       vis-snx-eli-000.
      *              *-------------------------------------------------*
      *              * Determinazione numero linea a video, e numero   *
      *              * facciata di impostazione corpo, per trattamen-  *
      *              * to del corpo direttamente nelle righe di scroll *
      *              *-------------------------------------------------*
           move      w-cnt-cor-nrg-dac    to   w-num-lin-scr-rig      .
           perform   num-lin-scr-000      thru num-lin-scr-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      w-num-lin-scr-lin    to   v-lin                  .
           move      72                   to   v-pos                  .
           if        w-rig-acc-snx-eli    =    "N"
                     if    w-rig-acc-snx-emo
                                          =    "N"
                           move  "   *** No"
                                          to   w-lin-imm-lit-snx
                     else  move  "       No"
                                          to   w-lin-imm-lit-snx
           else      if    w-rig-acc-snx-emo
                                          =    "N"
                           move  "Si ***   "
                                          to   w-lin-imm-lit-snx
                     else  move  "Si       "
                                          to   w-lin-imm-lit-snx      .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-eli-999.
           exit.

      *    *===========================================================*
      *    * Determinazione numero linea a video, e numero facciata    *
      *    * del corpo,  per trattamento del corpo direttamente nelle  *
      *    * righe di scroll                                           *
      *    *-----------------------------------------------------------*
       num-lin-scr-000.
      *              *-------------------------------------------------*
      *              * Determinazione numero linea                     *
      *              *-------------------------------------------------*
           move      w-num-lin-scr-rig    to   w-num-lin-scr-w01      .
           subtract  1                    from w-num-lin-scr-w01      .
           divide    w-lin-num-lin-vis    into w-num-lin-scr-w01
                                        giving w-num-lin-scr-w02
                                     remainder w-num-lin-scr-lin      .
           multiply  w-lin-num-lin-prc    by   w-num-lin-scr-lin      .
           add       w-lin-pri-lin-vid    to   w-num-lin-scr-lin      .
      *              *-------------------------------------------------*
      *              * Determinazione numero facciata 1..n             *
      *              *-------------------------------------------------*
           move      w-num-lin-scr-rig    to   w-num-lin-scr-w01      .
           divide    w-lin-num-lin-vis    into w-num-lin-scr-w01
                                        giving w-num-lin-scr-nfa
                                     remainder w-num-lin-scr-w02      .
           if        w-num-lin-scr-w02    >    zero
                     add   1              to  w-num-lin-scr-nfa       .
       num-lin-scr-999.
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
      *    * Messaggio centrale circondato da un box                   *
      *    *-----------------------------------------------------------*
       msg-cnt-box-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del valore del messaggio   *
      *              *-------------------------------------------------*
           if        w-err-box-err-msg    =    spaces
                     go to msg-cnt-box-600.
       msg-cnt-box-200.
      *              *-------------------------------------------------*
      *              * Se valore del messaggio diverso da spaces       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio immagine video                  *
      *                  *---------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Box                                         *
      *                  *---------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      12                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      14                   to   v-lto                  .
           move      77                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Messaggio nel box                           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      w-err-box-err-msg    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     msg-cnt-box-999.
       msg-cnt-box-600.
      *              *-------------------------------------------------*
      *              * Se valore del messaggio a spaces                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripristino immagine video                   *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     msg-cnt-box-999.
       msg-cnt-box-999.
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
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo impostazione      *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    "T"
                     go to cnt-tdo-nok-100
           else if   w-cnt-mfu-tip-imp    =    "C"
                     go to cnt-tdo-nok-500
           else      go to cnt-tdo-nok-999.
       cnt-tdo-nok-100.
      *              *-------------------------------------------------*
      *              * Se in impostazione : Testata                    *
      *              *-------------------------------------------------*
       cnt-tdo-nok-125.
      *                  *---------------------------------------------*
      *                  * Controllo su tipo componente da eliminare   *
      *                  *---------------------------------------------*
           if        w-tes-cde-tip        =    "M" or
                     w-tes-cde-tip        =    "S" or
                     w-tes-cde-tip        =    "D"
                     go to cnt-tdo-nok-150
           else if   w-tes-cde-tip        =    spaces
                     go to cnt-tdo-nok-127.
       cnt-tdo-nok-126.
           move      "Tipo componente da eliminare errato !             
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-127.
           move      "Manca il tipo componente da eliminare !           
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-150.
      *                  *---------------------------------------------*
      *                  * Controllo su codice componente da eliminare *
      *                  *---------------------------------------------*
           if        w-tes-cde-num        not  = zero
                     go to cnt-tdo-nok-175.
       cnt-tdo-nok-151.
           move      "Manca il codice componente da eliminare !         
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-175.
      *                  *---------------------------------------------*
      *                  * Fine controlli : uscita                     *
      *                  *---------------------------------------------*
           go to     cnt-tdo-nok-999.
       cnt-tdo-nok-500.
      *              *-------------------------------------------------*
      *              * Se in impostazione : Corpo                      *
      *              *-------------------------------------------------*
       cnt-tdo-nok-525.
      *                  *---------------------------------------------*
      *                  * Nessun controllo : uscita                   *
      *                  *---------------------------------------------*
           go to     cnt-tdo-nok-999.
       cnt-tdo-nok-900.
      *              *-------------------------------------------------*
      *              * Uscita per errore                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Emissione messaggio d'errore                *
      *                  *---------------------------------------------*
           if        w-err-box-err-m02    not  = spaces
                     perform box-msg-e02-000
                                          thru box-msg-e02-999
           else      perform box-msg-err-000
                                          thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-tdo-nok-flg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cnt-tdo-nok-999.
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
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *-----------------------------------------------------------*
       nor-nok-tes-000.
      *              *-------------------------------------------------*
      *              * Componente da eliminare                         *
      *              *-------------------------------------------------*
           move      spaces               to   w-tes-cde-tip          .
           move      zero                 to   w-tes-cde-num          .
           move      spaces               to   w-tes-cde-alf          .
           move      spaces               to   w-tes-cde-des          .
           move      spaces               to   w-tes-cde-umi          .
           move      zero                 to   w-tes-cde-dec          .
       nor-nok-tes-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave riga corpo, ad esclusione *
      *    * del numero progressivo riga                               *
      *    *-----------------------------------------------------------*
       nor-nok-rig-000.
      *              *-------------------------------------------------*
      *              * Dati provenienti dalla riga di distinta base    *
      *              *-------------------------------------------------*
           move      zero                 to   w-rig-lgr-tpm-ass      .
           move      zero                 to   w-rig-lgr-nrm-ass      .
           move      zero                 to   w-rig-lgr-num-prg      .
           move      zero                 to   w-rig-lgr-tpm-cpt      .
           move      zero                 to   w-rig-lgr-nrm-cpt      .
           move      spaces               to   w-rig-lgr-afm-cpt      .
           move      spaces               to   w-rig-lgr-umi-prd      .
           move      zero                 to   w-rig-lgr-dec-qta      .
           move      zero                 to   w-rig-lgr-qta-ipm      .
           move      zero                 to   w-rig-lgr-qta-ipd      .
           move      spaces               to   w-rig-lgr-not-cpt      .
           move      spaces               to   w-rig-lgr-alx-exp      .
      *              *-------------------------------------------------*
      *              * Dati provenienti dall'anagrafica assieme        *
      *              *-------------------------------------------------*
           move      spaces               to   w-rig-ass-afm-ass      .
           move      spaces               to   w-rig-ass-des-ass      .
      *              *-------------------------------------------------*
      *              * Dati da accettazione                            *
      *              *-------------------------------------------------*
           move      spaces               to   w-rig-acc-snx-eli      .
           move      spaces               to   w-rig-acc-snx-emo      .
       nor-nok-rig-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave piede                     *
      *    *-----------------------------------------------------------*
       nor-nok-pie-000.
       nor-nok-pie-999.
           exit.

      *    *===========================================================*
      *    * Routine di caricamento iniziale degli elementi da tratta- *
      *    * re                                                        *
      *    *-----------------------------------------------------------*
       rou-car-ini-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-rou-car-ini      .
      *              *-------------------------------------------------*
      *              * Caricamento delle righe di distinta base da     *
      *              * trattare                                        *
      *              *-------------------------------------------------*
           perform   car-rig-dba-000      thru car-rig-dba-999        .
      *              *-------------------------------------------------*
      *              * Se errore nel caricamento si pone lo status di  *
      *              * uscita ad errore e si esce                      *
      *              *-------------------------------------------------*
           if        w-car-rig-dba-flg    not  = spaces
                     move  "#"            to   w-cnt-rou-car-ini
                     go to rou-car-ini-999.
       rou-car-ini-999.
           exit.

      *    *===========================================================*
      *    * Caricamento delle righe di distinta base da trattare      *
      *    *-----------------------------------------------------------*
       car-rig-dba-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-car-rig-dba-flg      .
      *              *-------------------------------------------------*
      *              * Normalizzazione messaggio di errore             *
      *              *-------------------------------------------------*
           move      spaces               to   w-car-rig-dba-err      .
       car-rig-dba-100.
      *              *-------------------------------------------------*
      *              * Messaggio di caricamento in esecuzione          *
      *              *-------------------------------------------------*
           move      "                 Caricamento dati in esecuzione   
      -              "               "    to   w-err-box-err-msg      .
           perform   msg-cnt-box-000      thru msg-cnt-box-999        .
       car-rig-dba-200.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero di elementi caricati : zero          *
      *                  *---------------------------------------------*
           move      zero                 to   w-car-rig-dba-nel      .
       car-rig-dba-300.
      *              *-------------------------------------------------*
      *              * Sort                                            *
      *              *-------------------------------------------------*
           sort      srt                  on   ascending srt-key
                     input  procedure     is   srt-inp-prc-000
                                          thru srt-inp-prc-999
                     output procedure     is   srt-out-prc-000
                                          thru srt-out-prc-999        .
      *              *-------------------------------------------------*
      *              * Cancellazione messaggio di caricamento in ese-  *
      *              * cuzione                                         *
      *              *-------------------------------------------------*
           move      spaces               to   w-err-box-err-msg      .
           perform   msg-cnt-box-000      thru msg-cnt-box-999        .
       car-rig-dba-400.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda dell'esito del caricamento *
      *              *-------------------------------------------------*
           if        w-car-rig-dba-flg    =    "#"
                     go to car-rig-dba-500
           else if   w-car-rig-dba-flg    =    "F"
                     go to car-rig-dba-550
           else      go to car-rig-dba-600.
       car-rig-dba-500.
      *              *-------------------------------------------------*
      *              * Se flag di uscita a : errore bloccante          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-car-rig-dba-flg      .
      *                  *---------------------------------------------*
      *                  * Se messaggio di errore a spaces : uscita    *
      *                  *---------------------------------------------*
           if        w-car-rig-dba-err    =    spaces
                     go to car-rig-dba-999.
      *                  *---------------------------------------------*
      *                  * Visualizzazione messaggio di errore         *
      *                  *---------------------------------------------*
           move      w-car-rig-dba-err    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     car-rig-dba-999.
       car-rig-dba-550.
      *              *-------------------------------------------------*
      *              * Se flag di uscita a : errore non bloccante      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita a Ok                         *
      *                  *---------------------------------------------*
           move      spaces               to   w-car-rig-dba-flg      .
      *                  *---------------------------------------------*
      *                  * Se messaggio di errore a spaces : uscita    *
      *                  *---------------------------------------------*
           if        w-car-rig-dba-err    =    spaces
                     go to car-rig-dba-999.
      *                  *---------------------------------------------*
      *                  * Visualizzazione messaggio di anomalia       *
      *                  *---------------------------------------------*
           move      w-car-rig-dba-err    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     car-rig-dba-999.
       car-rig-dba-600.
      *              *-------------------------------------------------*
      *              * Se caricamento senza errori                     *
      *              *-------------------------------------------------*
       car-rig-dba-650.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del numero di elementi *
      *                  * caricati                                    *
      *                  *---------------------------------------------*
           if        w-car-rig-dba-nel    =    zero
                     go to car-rig-dba-700
           else      go to car-rig-dba-750.
       car-rig-dba-700.
      *                  *---------------------------------------------*
      *                  * Se numero di elementi caricati a zero       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita ad errore                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-car-rig-dba-flg      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione messaggio di errore     *
      *                      *-----------------------------------------*
           move      "Il componente da eliminare non e' mai utilizzato i
      -              "               "    to   w-err-box-err-msg      .
           move      "n alcuna distinta !                               
      -              "               "    to   w-err-box-err-m02      .
           perform   box-msg-e02-000      thru box-msg-e02-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     car-rig-dba-999.
       car-rig-dba-750.
      *                  *---------------------------------------------*
      *                  * Se numero di elementi caricati maggiore di  *
      *                  * zero                                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita a Ok                     *
      *                      *-----------------------------------------*
           move      spaces               to   w-car-rig-dba-flg      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     car-rig-dba-999.
       car-rig-dba-999.
           exit.

      *    *===========================================================*
      *    * Input procedure per selezione e sort righe di distinta da *
      *    * trattare                                                  *
      *    *-----------------------------------------------------------*
       srt-inp-prc-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione subroutine di implosione sca-  *
      *              * lare distinta base                              *
      *              *-------------------------------------------------*
           move      "E-INI"              to   w-imp-scl-dtp-fun      .
           if        w-tes-cde-tip        =    "M"
                     move  03             to   w-imp-scl-dtp-tdi
           else if   w-tes-cde-tip        =    "S"
                     move  02             to   w-imp-scl-dtp-tdi
           else if   w-tes-cde-tip        =    "D"
                     move  99             to   w-imp-scl-dtp-tdi
           else      move  zero           to   w-imp-scl-dtp-tdi      .
           move      w-tes-cde-num        to   w-imp-scl-dtp-ndi      .
           move      1,000                to   w-imp-scl-dtp-qdi      .
           perform   imp-scl-dtp-000      thru imp-scl-dtp-999        .
       srt-inp-prc-100.
      *              *-------------------------------------------------*
      *              * Lettura elemento successivo da implosione li-   *
      *              * neare distinta base                             *
      *              *-------------------------------------------------*
           move      "E-SKP"              to   w-imp-scl-dtp-fun      .
           perform   imp-scl-dtp-000      thru imp-scl-dtp-999        .
      *              *-------------------------------------------------*
      *              * Se fine scansione distinta : uscita             *
      *              *-------------------------------------------------*
           if        w-imp-scl-dtp-sts    not  = spaces
                     go to srt-inp-prc-999.
       srt-inp-prc-200.
      *              *-------------------------------------------------*
      *              * Controllo se l'elemento ottenuto dall'implosio- *
      *              * ne neare della distinta e' selezionabile        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo assieme non riconosciuto si ignora  *
      *                  * l'elemento                                  *
      *                  *---------------------------------------------*
           if        w-imp-scl-dtp-tas    not  = 01 and
                     w-imp-scl-dtp-tas    not  = 02 and
                     w-imp-scl-dtp-tas    not  = 99
                     go to srt-inp-prc-800.
      *                  *---------------------------------------------*
      *                  * Se anagrafica elemento non esistente si i-  *
      *                  * gnora l'elemento                            *
      *                  *---------------------------------------------*
           if        w-imp-scl-dtp-ana    not  = spaces
                     go to srt-inp-prc-800.
       srt-inp-prc-400.
      *              *-------------------------------------------------*
      *              * Composizione del record per il sort             *
      *              *-------------------------------------------------*
       srt-inp-prc-405.
      *                  *---------------------------------------------*
      *                  * Normalizzazione iniziale                    *
      *                  *---------------------------------------------*
           move      spaces               to   srt-rec                .
       srt-inp-prc-410.
      *                  *---------------------------------------------*
      *                  * Tipo di ordinamento                         *
      *                  *---------------------------------------------*
           if        w-imp-scl-dtp-tas    =    01
                     move  "A"            to   srt-ord-tip-ass
           else if   w-imp-scl-dtp-tas    =    02
                     move  "B"            to   srt-ord-tip-ass
           else if   w-imp-scl-dtp-tas    =    03
                     move  "C"            to   srt-ord-tip-ass
           else if   w-imp-scl-dtp-tas    =    99
                     move  "D"            to   srt-ord-tip-ass
           else      move  "Z"            to   srt-ord-tip-ass        .
       srt-inp-prc-415.
      *                  *---------------------------------------------*
      *                  * Segnale di anagrafica esistente             *
      *                  *---------------------------------------------*
           move      w-imp-scl-dtp-ana    to   srt-ord-fae-ass        .
       srt-inp-prc-420.
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico di ordinamento per l'-  *
      *                  * assieme, che corrisponde al codice alfanu-  *
      *                  * merico dell'assieme in caso di anagrafica   *
      *                  * esistente, oppure viene posto pari al co-   *
      *                  * dice numerico tra parentesi                 *
      *                  *---------------------------------------------*
           if        w-imp-scl-dtp-ana    =    spaces
                     go to srt-inp-prc-421
           else      go to srt-inp-prc-422.
       srt-inp-prc-421.
           if        w-imp-scl-dtp-tas    =    01
                     move  rf-dcp-alf-pro to   srt-ord-alf-ass
                     go to srt-inp-prc-425
           else if   w-imp-scl-dtp-tas    =    02
                     move  rf-dps-alf-sem to   srt-ord-alf-ass
                     go to srt-inp-prc-425
           else if   w-imp-scl-dtp-tas    =    03
                     move  rf-dpm-alf-map to   srt-ord-alf-ass
                     go to srt-inp-prc-425
           else if   w-imp-scl-dtp-tas    =    99
                     move  rf-lgv-alf-lgv to   srt-ord-alf-ass
                     go to srt-inp-prc-425
           else      go to srt-inp-prc-422.
       srt-inp-prc-422.
           move      spaces               to   srt-ord-alf-ass        .
           string    "("
                                delimited by   size
                     w-imp-scl-dtp-nas
                                delimited by   size
                     ")"
                                delimited by   size
                                          into srt-ord-alf-ass        .
       srt-inp-prc-425.
      *                  *---------------------------------------------*
      *                  * Codice numerico di ordinamento per l'assie- *
      *                  * me                                          *
      *                  *---------------------------------------------*
           move      w-imp-scl-dtp-nas    to   srt-ord-num-ass        .
       srt-inp-prc-430.
      *                  *---------------------------------------------*
      *                  * Tipo magazzino per l'assieme                *
      *                  *---------------------------------------------*
           move      w-imp-scl-dtp-tas    to   srt-lgr-tpm-ass        .
       srt-inp-prc-435.
      *                  *---------------------------------------------*
      *                  * Codice numerico magazzino per l'assieme     *
      *                  *---------------------------------------------*
           move      w-imp-scl-dtp-nas    to   srt-lgr-nrm-ass        .
       srt-inp-prc-440.
      *                  *---------------------------------------------*
      *                  * Numero progressivo elemento nella distinta  *
      *                  *---------------------------------------------*
           move      rf-lgr-num-prg       to   srt-lgr-num-prg        .
       srt-inp-prc-445.
      *                  *---------------------------------------------*
      *                  * Tipo magazzino per il componente            *
      *                  *---------------------------------------------*
           move      rf-lgr-tpm-cpt       to   srt-lgr-tpm-cpt        .
       srt-inp-prc-450.
      *                  *---------------------------------------------*
      *                  * Codice numerico per il componente           *
      *                  *---------------------------------------------*
           move      rf-lgr-nrm-cpt       to   srt-lgr-nrm-cpt        .
       srt-inp-prc-455.
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico per il componente       *
      *                  *---------------------------------------------*
           move      rf-lgr-afm-cpt       to   srt-lgr-afm-cpt        .
       srt-inp-prc-460.
      *                  *---------------------------------------------*
      *                  * Unita' di misura per la produzione per il   *
      *                  * componente                                  *
      *                  *---------------------------------------------*
           move      rf-lgr-umi-prd       to   srt-lgr-umi-prd        .
       srt-inp-prc-465.
      *                  *---------------------------------------------*
      *                  * Numero decimali per la quantita' per il     *
      *                  * componente                                  *
      *                  *---------------------------------------------*
           move      rf-lgr-dec-qta       to   srt-lgr-dec-qta        .
       srt-inp-prc-470.
      *                  *---------------------------------------------*
      *                  * Quantita' di impiego, moltiplicatore        *
      *                  *---------------------------------------------*
           move      rf-lgr-qta-ipm       to   srt-lgr-qta-ipm        .
       srt-inp-prc-475.
      *                  *---------------------------------------------*
      *                  * Quantita' di impiego, divisore              *
      *                  *---------------------------------------------*
           move      rf-lgr-qta-ipd       to   srt-lgr-qta-ipd        .
       srt-inp-prc-480.
      *                  *---------------------------------------------*
      *                  * Area libera per espansioni speciali         *
      *                  *---------------------------------------------*
           move      rf-lgr-alx-exp       to   srt-lgr-alx-exp        .
       srt-inp-prc-485.
      *                  *---------------------------------------------*
      *                  * Descrizione per l'assieme                   *
      *                  *---------------------------------------------*
           if        w-imp-scl-dtp-ana    =    spaces
                     go to srt-inp-prc-486
           else      go to srt-inp-prc-487.
       srt-inp-prc-486.
           if        w-imp-scl-dtp-tas    =    01
                     move  rf-dcp-des-pro to   srt-ass-des-ass
                     go to srt-inp-prc-490
           else if   w-imp-scl-dtp-tas    =    02
                     move  rf-dps-des-sem to   srt-ass-des-ass
                     go to srt-inp-prc-490
           else if   w-imp-scl-dtp-tas    =    03
                     move  rf-dpm-des-map to   srt-ass-des-ass
                     go to srt-inp-prc-490
           else if   w-imp-scl-dtp-tas    =    99
                     move  rf-lgv-des-lgv to   srt-ass-des-ass
                     go to srt-inp-prc-490
           else      go to srt-inp-prc-487.
       srt-inp-prc-487.
           move      all   "."            to   srt-ass-des-ass        .
       srt-inp-prc-490.
      *                  *---------------------------------------------*
      *                  * Fine composizione                           *
      *                  *---------------------------------------------*
           go to     srt-inp-prc-600.
       srt-inp-prc-600.
      *              *-------------------------------------------------*
      *              * Rilascio del record al sort                     *
      *              *-------------------------------------------------*
           release   srt-rec                                          .
       srt-inp-prc-700.
      *              *-------------------------------------------------*
      *              * Incremento numero di elementi caricati          *
      *              *-------------------------------------------------*
           add       1                    to   w-car-rig-dba-nel      .
       srt-inp-prc-800.
      *              *-------------------------------------------------*
      *              * Riciclo alla lettura dell'elemento successivo   *
      *              *-------------------------------------------------*
           go to     srt-inp-prc-100.
       srt-inp-prc-999.
           exit.

      *    *===========================================================*
      *    * Output procedure per selezione e sort righe di distinta   *
      *    * da trattare                                               *
      *    *-----------------------------------------------------------*
       srt-out-prc-000.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale archivio sortato [srt]      *
      *              *-------------------------------------------------*
           return    srt    at end
                            go to srt-out-prc-999.
       srt-out-prc-100.
      *              *-------------------------------------------------*
      *              * Se e' gia stato raggiunto il massimo numero di  *
      *              * elementi che possono essere trattati : si esce  *
      *              * con flag di anomalia                            *
      *              *-------------------------------------------------*
           if        w-rlt-sup-max        not  < 99999
                     go to srt-out-prc-900.
       srt-out-prc-200.
      *              *-------------------------------------------------*
      *              * Composizione area w-rig                         *
      *              *-------------------------------------------------*
       srt-out-prc-205.
      *                  *---------------------------------------------*
      *                  * Normalizzazione iniziale                    *
      *                  *---------------------------------------------*
           move      spaces               to   w-rig                  .
       srt-out-prc-210.
      *                  *---------------------------------------------*
      *                  * Numero progressivo per la riga              *
      *                  *---------------------------------------------*
           move      w-rlt-sup-max        to   w-rig-num-prg          .
           add       1                    to   w-rig-num-prg          .
       srt-out-prc-215.
      *                  *---------------------------------------------*
      *                  * Tipo magazzino per l'assieme                *
      *                  *---------------------------------------------*
           move      srt-lgr-tpm-ass      to   w-rig-lgr-tpm-ass      .
       srt-out-prc-220.
      *                  *---------------------------------------------*
      *                  * Codice numerico magazzino per l'assieme     *
      *                  *---------------------------------------------*
           move      srt-lgr-nrm-ass      to   w-rig-lgr-nrm-ass      .
       srt-out-prc-225.
      *                  *---------------------------------------------*
      *                  * Numero progressivo elemento nella distinta  *
      *                  *---------------------------------------------*
           move      srt-lgr-num-prg      to   w-rig-lgr-num-prg      .
       srt-out-prc-230.
      *                  *---------------------------------------------*
      *                  * Tipo magazzino per il componente            *
      *                  *---------------------------------------------*
           move      srt-lgr-tpm-cpt      to   w-rig-lgr-tpm-cpt      .
       srt-out-prc-235.
      *                  *---------------------------------------------*
      *                  * Codice numerico magazzino per il componente *
      *                  *---------------------------------------------*
           move      srt-lgr-nrm-cpt      to   w-rig-lgr-nrm-cpt      .
       srt-out-prc-240.
      *                  *---------------------------------------------*
      *                  * Codice numerico magazzino per il componente *
      *                  *---------------------------------------------*
           move      srt-lgr-afm-cpt      to   w-rig-lgr-afm-cpt      .
       srt-out-prc-245.
      *                  *---------------------------------------------*
      *                  * Unita' di misura per la produzione per il   *
      *                  * componente                                  *
      *                  *---------------------------------------------*
           move      srt-lgr-umi-prd      to   w-rig-lgr-umi-prd      .
       srt-out-prc-250.
      *                  *---------------------------------------------*
      *                  * Numero decimali per la quantita' di impiego *
      *                  * per il componente                           *
      *                  *---------------------------------------------*
           move      srt-lgr-dec-qta      to   w-rig-lgr-dec-qta      .
       srt-out-prc-255.
      *                  *---------------------------------------------*
      *                  * Quantita' di impiego, moltiplicatore        *
      *                  *---------------------------------------------*
           move      srt-lgr-qta-ipm      to   w-rig-lgr-qta-ipm      .
       srt-out-prc-260.
      *                  *---------------------------------------------*
      *                  * Quantita' di impiego, divisore              *
      *                  *---------------------------------------------*
           move      srt-lgr-qta-ipd      to   w-rig-lgr-qta-ipd      .
       srt-out-prc-265.
      *                  *---------------------------------------------*
      *                  * Note relative al componente                 *
      *                  *---------------------------------------------*
           move      srt-lgr-not-cpt      to   w-rig-lgr-not-cpt      .
       srt-out-prc-270.
      *                  *---------------------------------------------*
      *                  * Area libera per espansioni speciali         *
      *                  *---------------------------------------------*
           move      srt-lgr-alx-exp      to   w-rig-lgr-alx-exp      .
       srt-out-prc-275.
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico magazzino per l'assieme *
      *                  *---------------------------------------------*
           move      srt-ord-alf-ass      to   w-rig-ass-afm-ass      .
       srt-out-prc-280.
      *                  *---------------------------------------------*
      *                  * Descrizione per l'assieme                   *
      *                  *---------------------------------------------*
           move      srt-ass-des-ass      to   w-rig-ass-des-ass      .
       srt-out-prc-285.
      *                  *---------------------------------------------*
      *                  * Si/No eliminazione da effettuare            *
      *                  *---------------------------------------------*
           move      "S"                  to   w-rig-acc-snx-eli      .
       srt-out-prc-290.
      *                  *---------------------------------------------*
      *                  * Si/No eliminazione modificabile             *
      *                  *---------------------------------------------*
           move      "S"                  to   w-rig-acc-snx-emo      .
       srt-out-prc-400.
      *              *-------------------------------------------------*
      *              * Put su file relative di appoggio                *
      *              *-------------------------------------------------*
           move      "PT"                 to   w-rlt-sup-ope          .
           move      w-rig                to   w-rlt-sup-buf          .
           perform   cll-rlt-sup-000      thru cll-rlt-sup-999        .
       srt-out-prc-800.
      *              *-------------------------------------------------*
      *              * Riciclo a lettura elemento successivo da sort   *
      *              *-------------------------------------------------*
           go to     srt-out-prc-000.
       srt-out-prc-900.
      *              *-------------------------------------------------*
      *              * Uscita per superamento del massimo numero di e- *
      *              * lementi che possono essere trattati             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad anomalia                  *
      *                  *---------------------------------------------*
           move      "F"                  to   w-car-rig-dba-flg      .
      *                  *---------------------------------------------*
      *                  * Messaggio per l' anomalia                   *
      *                  *---------------------------------------------*
           move      "ATTENZIONE : Superati i 99999 elementi da trattare
      -              " !             "    to   w-car-rig-dba-err      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     srt-out-prc-999.
       srt-out-prc-999.
           exit.

      *    *===========================================================*
      *    * Routine post-exit su inserimento                          *
      *    *-----------------------------------------------------------*
       pos-exi-ins-000.
       pos-exi-ins-999.
           exit.

      *    *===========================================================*
      *    * Routine post-conferma di inserimento                      *
      *    *-----------------------------------------------------------*
       pos-cnf-ins-000.
      *              *-------------------------------------------------*
      *              * Routine di scaricamento finale degli elementi   *
      *              * trattati                                        *
      *              *-------------------------------------------------*
           perform   rou-sca-fin-000      thru rou-sca-fin-999        .
       pos-cnf-ins-999.
           exit.

      *    *===========================================================*
      *    * Routine di scaricamento finale degli elementi trattati    *
      *    * re                                                        *
      *    *-----------------------------------------------------------*
       rou-sca-fin-000.
      *              *-------------------------------------------------*
      *              * Messaggio di aggiornamento dati in esecuzione   *
      *              *-------------------------------------------------*
           perform   sub-vis-agd-000      thru sub-vis-agd-999        .
      *              *-------------------------------------------------*
      *              * Numero record da trattare a zero                *
      *              *-------------------------------------------------*
           move      zero                 to   w-rlt-sup-prg          .
       rou-sca-fin-100.
      *              *-------------------------------------------------*
      *              * Incremento numero record da trattare            *
      *              *-------------------------------------------------*
           add       1                    to   w-rlt-sup-prg          .
      *              *-------------------------------------------------*
      *              * Se oltr il max : fine scaricamento              *
      *              *-------------------------------------------------*
           if        w-rlt-sup-prg        >    w-rlt-sup-max
                     go to rou-sca-fin-900.
      *              *-------------------------------------------------*
      *              * Read da file relative di appoggio               *
      *              *-------------------------------------------------*
           move      "RD"                 to   w-rlt-sup-ope          .
           perform   cll-rlt-sup-000      thru cll-rlt-sup-999        .
      *              *-------------------------------------------------*
      *              * Se errore in lettura si ignora il record e si   *
      *              * ricicla per il trattamento del record successi- *
      *              * vo                                              *
      *              *-------------------------------------------------*
           if        w-rlt-sup-exs        not  = spaces
                     go to rou-sca-fin-800.
      *              *-------------------------------------------------*
      *              * Record letto in area di ridefinizione 'w-rig'   *
      *              *-------------------------------------------------*
           move      w-rlt-sup-buf        to   w-rig                  .
       rou-sca-fin-200.
      *              *-------------------------------------------------*
      *              * Se l'aggiornamento del record non deve essere   *
      *              * eseguito : riciclo al trattamento del record    *
      *              * successivo                                      *
      *              *-------------------------------------------------*
           if        w-rig-acc-snx-eli    not  = "S"
                     go to rou-sca-fin-800.
       rou-sca-fin-300.
      *              *-------------------------------------------------*
      *              * Controllo di non svuotamento della distinta ba- *
      *              * se                                              *
      *              *-------------------------------------------------*
       rou-sca-fin-310.
      *                  *---------------------------------------------*
      *                  * Caricamento del buffer, per un massimo di   *
      *                  * un elemento diverso da quello da eliminare  *
      *                  *---------------------------------------------*
           perform   buf-per-cns-000      thru buf-per-cns-999        .
      *                  *---------------------------------------------*
      *                  * Se il numero di elementi caricati nel buf-  *
      *                  * fer, diversi da quello da eliminare, non e' *
      *                  * a zero : si prosegue                        *
      *                  *---------------------------------------------*
           if        w-ctl-nsb-num-cpt    >    zero
                     go to rou-sca-fin-400.
       rou-sca-fin-320.
      *                  *---------------------------------------------*
      *                  * Cancellazione messaggio di aggiornamento    *
      *                  * dati in esecuzione                          *
      *                  *---------------------------------------------*
           perform   sub-fvi-agd-000      thru sub-fvi-agd-999        .
       rou-sca-fin-330.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore per svuotamento della   *
      *                  * distinta base                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Salvataggio immagine video              *
      *                      *-----------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Video in Off                            *
      *                      *-----------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Box                                     *
      *                      *-----------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      08                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      18                   to   v-lto                  .
           move      75                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Messaggio nel box                       *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      68                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      07                   to   v-pos                  .
           move      " Attenzione :                                     
      -              "                  " to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      68                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      07                   to   v-pos                  .
           move      " L'eliminazione del componente [X] xxxxxxxxxxxxxx 
      -              "                  " to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      39                   to   v-pos                  .
           move      w-tes-cde-tip        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      42                   to   v-pos                  .
           move      w-tes-cde-alf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      68                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      07                   to   v-pos                  .
           move      "    non puo' avvenire per la distinta [X] xxxxxxxx
      -              "xxxxxx            " to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      46                   to   v-pos                  .
           if        w-rig-lgr-tpm-ass    =    01
                     move  "P"            to   v-alf
           else if   w-rig-lgr-tpm-ass    =    02
                     move  "S"            to   v-alf
           else if   w-rig-lgr-tpm-ass    =    03
                     move  "M"            to   v-alf
           else if   w-rig-lgr-tpm-ass    =    99
                     move  "D"            to   v-alf
           else      move  "?"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      49                   to   v-pos                  .
           move      w-rig-ass-afm-ass    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      68                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      07                   to   v-pos                  .
           move      "    in quanto e' l'unico componente della distinta
      -              ".                 " to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      68                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      07                   to   v-pos                  .
           move      "                            Premere un tasto per p
      -              "resa visione [ ]  " to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Video in On                             *
      *                      *-----------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Accettazione carattere di presa visione *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      17                   to   v-lin                  .
           move      71                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Ripristino immagine video               *
      *                      *-----------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       rou-sca-fin-340.
      *                  *---------------------------------------------*
      *                  * Ripristino messaggio di aggiornamento       *
      *                  * dati in esecuzione                          *
      *                  *---------------------------------------------*
           perform   sub-vis-agd-000      thru sub-vis-agd-999        .
       rou-sca-fin-350.
      *                  *---------------------------------------------*
      *                  * Continuazione per ignorare il record e con- *
      *                  * tinuare da quello successivo                *
      *                  *---------------------------------------------*
           go to     rou-sca-fin-800.
       rou-sca-fin-400.
      *              *-------------------------------------------------*
      *              * Lettura con lock della riga di distinta base    *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "TNMASS    "         to   f-key                  .
           move      w-rig-lgr-tpm-ass    to   rf-lgr-tpm-ass         .
           move      w-rig-lgr-nrm-ass    to   rf-lgr-nrm-ass         .
           move      w-rig-lgr-num-prg    to   rf-lgr-num-prg         .
           move      "pgm/dtp/fls/ioc/obj/ioflgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgr                 .
      *              *-------------------------------------------------*
      *              * Se record non trovato : si ignora il record e   *
      *              * si ricicla per il trattamento del record suc-   *
      *              * cessivo                                         *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rou-sca-fin-800.
       rou-sca-fin-500.
      *              *-------------------------------------------------*
      *              * Cancellazione del record                        *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgr                 .
       rou-sca-fin-800.
      *              *-------------------------------------------------*
      *              * Riciclo per il trattamento del record successi- *
      *              * vo                                              *
      *              *-------------------------------------------------*
           go to     rou-sca-fin-100.
       rou-sca-fin-900.
      *              *-------------------------------------------------*
      *              * Cancellazione messaggio di aggiornamento dati   *
      *              * in esecuzione                                   *
      *              *-------------------------------------------------*
           perform   sub-fvi-agd-000      thru sub-fvi-agd-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-sca-fin-999.
       rou-sca-fin-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per la visualizzazione del messaggio di ag-    *
      *    * giornamento dati in esecuzione                            *
      *    *-----------------------------------------------------------*
       sub-vis-agd-000.
           move      "                Aggiornamento dati in esecuzione  
      -              "               "    to   w-err-box-err-msg      .
           perform   msg-cnt-box-000      thru msg-cnt-box-999        .
       sub-vis-agd-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per la cancellazione del messaggio di ag-      *
      *    * giornamento dati in esecuzione                            *
      *    *-----------------------------------------------------------*
       sub-fvi-agd-000.
           move      spaces               to   w-err-box-err-msg      .
           perform   msg-cnt-box-000      thru msg-cnt-box-999        .
       sub-fvi-agd-999.
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
      *    * Routine di lettura archivio [lgv]                         *
      *    *-----------------------------------------------------------*
       let-arc-lgv-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-lgv-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-lgv-num    =    zero  
                     go to let-arc-lgv-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMLGV"             to   f-key                  .
           move      w-let-arc-lgv-num    to   rf-lgv-num-lgv         .
           move      "pgm/dtp/fls/ioc/obj/ioflgv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgv                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-lgv-400.
       let-arc-lgv-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-lgv-alf-lgv       to   w-let-arc-lgv-alf      .
           move      rf-lgv-des-lgv       to   w-let-arc-lgv-des      .
           move      rf-lgv-umi-prd       to   w-let-arc-lgv-umi      .
           move      rf-lgv-dec-qta       to   w-let-arc-lgv-deq      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-lgv-999.
       let-arc-lgv-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-lgv-flg      .
           move      all   "."            to   w-let-arc-lgv-des      .
           go to     let-arc-lgv-600.
       let-arc-lgv-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-lgv-des      .
       let-arc-lgv-600.
           move      spaces               to   w-let-arc-lgv-alf      .
           move      spaces               to   w-let-arc-lgv-umi      .
           move      zero                 to   w-let-arc-lgv-deq      .
       let-arc-lgv-999.
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
      *    * Subroutines per accettazione codice prodotto 'dcp'        *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acoddcp0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per accettazione codice semilavorato 'dps'    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dps/prg/cpy/acoddps0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per accettazione codice sub-distinta          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dtp/prg/cpy/acodlgv0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per accettazione codice materia prima         *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/acoddpm0.acs"                   .

      *    *===========================================================*
      *    * Subroutine di esplosione scalare distinta base            *
      *    *                                                           *
      *    * ----------------                                          *
      *    *                                                           *
      *    * Attenzione : Questa routine utilizza numerosi file, che   *
      *    *              sono di seguito elencati. Essi vengono tut-  *
      *    *              ti trattati in lettura, ma nel far questo    *
      *    *              vengono rovinate le Start eventualmente ese- *
      *    *              guite su di essi.                            *
      *    *                                                           *
      *    *              Pertanto i programmi che intendono utilizza- *
      *    *              re la seguente routine non devono assoluta-  *
      *    *              mente utilizzare come files principali i fi- *
      *    *              les utilizzati :                             *
      *    *                                                           *
      *    *                  - [lgt]                                  *
      *    *                  - [lgr]                                  *
      *    *                  - [dcp]                                  *
      *    *                  - [dps]                                  *
      *    *                  - [dpm]                                  *
      *    *                  - [lgv]                                  *
      *    *                                                           *
      *    *              Inoltre non e' possibile utilizzare nel ci-  *
      *    *              clo principale nemmeno i moduli di filtro    *
      *    *              per ordinamento e/o selezione anagrafiche,   *
      *    *              in quanto questi, in alcune circostanze,     *
      *    *              utilizzano a loro volta come files guida i   *
      *    *              files :                                      *
      *    *                                                           *
      *    *                  - [dcp]                                  *
      *    *                  - [dps]                                  *
      *    *                  - [dpm]                                  *
      *    *                                                           *
      *    *              Nei casi in cui si dovesse assolutamente a-  *
      *    *              vere un file guida tra quelli non consentiti *
      *    *              da questa routine, sara' necessario :        *
      *    *                                                           *
      *    *                  - Preparare preventivamente un file tem- *
      *    *                    poraneo di appoggio come file guida    *
      *    *                                                           *
      *    *                  - Utilizzare tale file temporaneo di ap- *
      *    *                    poggio come file guida                 *
      *    *                                                           *
      *    *                  - Cancellare tale file temporaneo di ap- *
      *    *                    poggio alla fine dell'esecuzione       *
      *    *                                                           *
      *    *                  Nota : Il file di appoggio temporaneo    *
      *    *                         puo' essere costituito da un fi-  *
      *    *                         le di sort, in quanto questa ro-  *
      *    *                         utine non utilizza il sort.       *
      *    *                                                           *
      *    * ----------------                                          *
      *    *                                                           *
      *    * Presume che preventivamente siano stati definiti ed aper- *
      *    * ti i seguenti files:                                      *
      *    *                                                           *
      *    *  - [lgt] : Testate distinte base                          *
      *    *  - [lgr] : Righe distinte base                            *
      *    *  - [dcp] : Anagrafica prodotti finiti                     *
      *    *  - [dps] : Anagrafica semilavorati                        *
      *    *  - [dpm] : Anagrafica materie prime                       *
      *    *  - [lgv] : Anagrafica subdistinte virtuali                *
      *    *                                                           *
      *    * ----------------                                          *
      *    *                                                           *
      *    * - Funzione 'E-INI'                                        *
      *    *                                                           *
      *    *   Deve necessariamente essere richiamata, per ogni esplo- *
      *    *   sione da eseguire, prima delle altre funzioni.          *
      *    *                                                           *
      *    *   - Input  : w-esp-scl-dtp-fun = 'E-INI'                  *
      *    *                                                           *
      *    *              w-esp-scl-dtp-tde = Tipo codice magazzino da *
      *    *                                  esplodere                *
      *    *                                   - 01 : Prodotto finito  *
      *    *                                   - 02 : Semilavorato     *
      *    *                                   - 99 : Subdistinta vir- *
      *    *                                          tuale            *
      *    *                                                           *
      *    *              w-esp-scl-dtp-nde = Codice numerico magazzi- *
      *    *                                  no da esplodere o codice *
      *    *                                  numerico della subdis-   *
      *    *                                  tinta virtuale           *
      *    *                                                           *
      *    *              w-esp-scl-dtp-ade = Codice alfanumerico cor- *
      *    *                                  rispondente al codice    *
      *    *                                  numerico precedente      *
      *    *                                                           *
      *    *              w-esp-scl-dtp-qde = Quantita' da esplodere   *
      *    *                                                           *
      *    *   - Output : w-esp-scl-dtp-tip = Tipo di elemento         *
      *    *                                                           *
      *    *              w-esp-scl-dtp-num = Codice numerico dell'e-  *
      *    *                                  lemento                  *
      *    *                                                           *
      *    *              w-esp-scl-dtp-alf = Codice alfanumerico del- *
      *    *                                  l'elemento               *
      *    *                                                           *
      *    *              w-esp-scl-dtp-liv = Livello di profondita'   *
      *    *                                  relativo all'elemento,   *
      *    *                                  forzato a zero           *
      *    *                                                           *
      *    *              w-esp-scl-dtp-qta = Quantita' totale relati- *
      *    *                                  va all'elemento          *
      *    *                                                           *
      *    *              w-esp-scl-dtp-ana = Segnale di anagrafica e- *
      *    *                                  sistente                 *
      *    *                                   - Spaces : Si           *
      *    *                                   - '#'    : No           *
      *    *                                                           *
      *    *              w-esp-scl-dtp-des = Descrizione da anagrafi- *
      *    *                                  ca elemento              *
      *    *                                                           *
      *    *              w-esp-scl-dtp-umi = Unita' di misura da ana- *
      *    *                                  grafica elemento         *
      *    *                                                           *
      *    *              w-esp-scl-dtp-dec = Numero decimali quantita'*
      *    *                                  da anagrafica elemento   *
      *    *                                                           *
      *    *              rf-dcp, oppure    = Anagrafica elemento, a   *
      *    *              rf-dps, oppure      seconda del tipo ele-    *
      *    *              rf-dpm, oppure      mento, se anagrafica e-  *
      *    *              rf-lgv              sistente                 *
      *    *                                                           *
      *    * ----------------                                          *
      *    *                                                           *
      *    * - Funzione 'E-GET'                                        *
      *    *                                                           *
      *    *   Ottenimento dell'elemento successivo relativo all'es-   *
      *    *   plosione scalare.                                       *
      *    *                                                           *
      *    *   - Input  : w-esp-scl-dtp-fun = 'E-GET'                  *
      *    *                                                           *
      *    *   - Output : w-esp-scl-dtp-sts = Status di uscita         *
      *    *                                   - Spaces : Ok, elemento *
      *    *                                              successivo a *
      *    *                                              disposizione *
      *    *                                   - '#'    : Fine elemen- *
      *    *                                              ti nella di- *
      *    *                                              stinta       *
      *    *                                                           *
      *    *              w-esp-scl-dtp-tip = Tipo di elemento trovato *
      *    *                                  nella scansione          *
      *    *                                   - 01 : Prodotto finito  *
      *    *                                   - 02 : Semilavorato     *
      *    *                                   - 03 : Materia prima    *
      *    *                                   - 99 : Subdistinta vir- *
      *    *                                          tuale            *
      *    *                                                           *
      *    *              w-esp-scl-dtp-num = Codice numerico dell'e-  *
      *    *                                  lemento trovato nella    *
      *    *                                  scansione                *
      *    *                                                           *
      *    *              w-esp-scl-dtp-alf = Codice alfanumerico del- *
      *    *                                  l'elemento trovato nel-  *
      *    *                                  la scansione             *
      *    *                                                           *
      *    *              w-esp-scl-dtp-liv = Livello di profondita'   *
      *    *                                  relativo all'elemento    *
      *    *                                  trovato nella scansione, *
      *    *                                  assumendo come base il   *
      *    *                                  livello zero relativo    *
      *    *                                  all'assieme da esplodere *
      *    *                                                           *
      *    *              w-esp-scl-dtp-qta = Quantita' totale relati- *
      *    *                                  va all'elemento trovato  *
      *    *                                  nella scansione          *
      *    *                                                           *
      *    *              w-esp-scl-dtp-ana = Segnale di anagrafica e- *
      *    *                                  sistente                 *
      *    *                                   - Spaces : Si           *
      *    *                                   - '#'    : No           *
      *    *                                                           *
      *    *              w-esp-scl-dtp-des = Descrizione da anagrafi- *
      *    *                                  ca elemento              *
      *    *                                                           *
      *    *              w-esp-scl-dtp-umi = Unita' di misura da ana- *
      *    *                                  grafica elemento         *
      *    *                                                           *
      *    *              w-esp-scl-dtp-dec = Numero decimali quantita'*
      *    *                                  da anagrafica elemento   *
      *    *                                                           *
      *    *              rf-dcp, oppure    = Anagrafica elemento, a   *
      *    *              rf-dps, oppure      seconda del tipo ele-    *
      *    *              rf-dpm, oppure      mento, se anagrafica e-  *
      *    *              rf-lgv              sistente                 *
      *    *                                                           *
      *    * ----------------                                          *
      *    *                                                           *
      *    * - Funzione 'E-SKP'                                        *
      *    *                                                           *
      *    *   Ottenimento dell'elemento successivo dello stesso li-   *
      *    *   vello dell'ultimo elemento trovato, evitando in tal     *
      *    *   modo di entrare piu' in profondita' per quanto riguar-  *
      *    *   da i semilavorati e le subdistinte virtuali.            *
      *    *                                                           *
      *    *   - Input  : w-esp-scl-dtp-fun = 'E-SKP'                  *
      *    *                                                           *
      *    *   - Output : Esattamente come per funzione 'E-GET'        *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       esp-scl-dtp-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda della funzione richiesta   *
      *              *-------------------------------------------------*
           if        w-esp-scl-dtp-fun    =    "E-INI"
                     go to esp-scl-dtp-050
           else if   w-esp-scl-dtp-fun    =    "E-GET"
                     go to esp-scl-dtp-100
           else if   w-esp-scl-dtp-fun    =    "E-SKP"
                     go to esp-scl-dtp-250
           else      go to esp-scl-dtp-999.
       esp-scl-dtp-050.
      *              *-------------------------------------------------*
      *              * Funzione 'E-INI'                                *
      *              *-------------------------------------------------*
       esp-scl-dtp-055.
      *                  *---------------------------------------------*
      *                  * Livello di lavoro a : 01                    *
      *                  *---------------------------------------------*
           move      01                   to   w-esp-scl-dtp-wlp      .
      *                  *---------------------------------------------*
      *                  * Segnale di prossima operazione da eseguire  *
      *                  * a : Start sul livello di profondita interno *
      *                  * attuale                                     *
      *                  *---------------------------------------------*
           move      "S"                  to   w-esp-scl-dtp-wpo      .
       esp-scl-dtp-060.
      *                  *---------------------------------------------*
      *                  * Preparazione valori per il 1. livello       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo elemento                           *
      *                      *-----------------------------------------*
           move      w-esp-scl-dtp-tde    to   w-esp-scl-dtp-wti (01) .
      *                      *-----------------------------------------*
      *                      * Codice numerico elemento                *
      *                      *-----------------------------------------*
           move      w-esp-scl-dtp-nde    to   w-esp-scl-dtp-wnu (01) .
      *                      *-----------------------------------------*
      *                      * Codice alfanumerico elemento            *
      *                      *-----------------------------------------*
           move      w-esp-scl-dtp-ade    to   w-esp-scl-dtp-wal (01) .
      *                      *-----------------------------------------*
      *                      * Numero progressivo per riga distinta    *
      *                      *-----------------------------------------*
           move      zero                 to   w-esp-scl-dtp-wrg (01) .
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica elemento e determi-  *
      *                      * nazione dei seguenti valori:            *
      *                      *  - Elemento anagraficamente esistente   *
      *                      *  - Descrizione anagrafica elemento      *
      *                      *  - Unita' di misura elemento            *
      *                      *  - Numero decimali per quantita'        *
      *                      *-----------------------------------------*
           perform   esp-scl-dtp-800      thru esp-scl-dtp-809        .
      *                      *-----------------------------------------*
      *                      * Quantita' relativa all'elemento         *
      *                      *-----------------------------------------*
           move      w-esp-scl-dtp-qde    to   w-esp-scl-dtp-wqt (01) .
           if        w-esp-scl-dtp-wnd (01)
                                          =    zero
                     add     0,499        to   w-esp-scl-dtp-wqt (01)
                     divide   1000        into w-esp-scl-dtp-wqt (01)
                                                         rounded
                     multiply 1000        by   w-esp-scl-dtp-wqt (01)
           else if   w-esp-scl-dtp-wnd (01)
                                          =    1
                     add     0,049        to   w-esp-scl-dtp-wqt (01)
                     divide    100        into w-esp-scl-dtp-wqt (01)
                                                         rounded
                     multiply  100        by   w-esp-scl-dtp-wqt (01)
           else if   w-esp-scl-dtp-wnd (01)
                                          =    2
                     add     0,004        to   w-esp-scl-dtp-wqt (01)
                     divide     10        into w-esp-scl-dtp-wqt (01)
                                                         rounded
                     multiply   10        by   w-esp-scl-dtp-wqt (01) .
       esp-scl-dtp-065.
      *                  *---------------------------------------------*
      *                  * Preparazione valori in uscita, escluso lo   *
      *                  * status, relativi all'elemento               *
      *                  *---------------------------------------------*
           perform   esp-scl-dtp-820      thru esp-scl-dtp-829        .
       esp-scl-dtp-070.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     esp-scl-dtp-999.
       esp-scl-dtp-100.
      *              *-------------------------------------------------*
      *              * Funzione 'E-GET'                                *
      *              *-------------------------------------------------*
       esp-scl-dtp-105.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del;valore del segnale *
      *                  * di prossima operazione da eseguire          *
      *                  *---------------------------------------------*
           if        w-esp-scl-dtp-wpo    =    "S"
                     go to esp-scl-dtp-110
           else if   w-esp-scl-dtp-wpo    =    "U"
                     go to esp-scl-dtp-140
           else      go to esp-scl-dtp-145.
       esp-scl-dtp-110.
      *                  *---------------------------------------------*
      *                  * Se operazione da eseguire : Start sul li-   *
      *                  * vello di profondita' interno attuale        *
      *                  *---------------------------------------------*
       esp-scl-dtp-115.
      *                      *-----------------------------------------*
      *                      * Esecuzione 'Start'                      *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "GT"                 to   f-cfr                  .
           move      "TNMASS    "         to   f-key                  .
           move      w-esp-scl-dtp-wti
                    (w-esp-scl-dtp-wlp)   to   rf-lgr-tpm-ass         .
           move      w-esp-scl-dtp-wnu
                    (w-esp-scl-dtp-wlp)   to   rf-lgr-nrm-ass         .
           move      w-esp-scl-dtp-wrg
                    (w-esp-scl-dtp-wlp)   to   rf-lgr-num-prg         .
           move      "pgm/dtp/fls/ioc/obj/ioflgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgr                 .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda dell'esito della   *
      *                      * 'Start'                                 *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     go to esp-scl-dtp-130.
       esp-scl-dtp-120.
      *                      *-----------------------------------------*
      *                      * Se 'Start' errata : non ci sono altri   *
      *                      * elementi nella distinta o subdistinta   *
      *                      *-----------------------------------------*
       esp-scl-dtp-125.
      *                          *-------------------------------------*
      *                          * Decremento livello di profondita'   *
      *                          * interno                             *
      *                          *-------------------------------------*
           subtract  1                    from w-esp-scl-dtp-wlp      .
      *                          *-------------------------------------*
      *                          * Se raggiunto il livello di profon-  *
      *                          * dita' interno zero : uscita con se- *
      *                          * gnale di fine scansione             *
      *                          *-------------------------------------*
           if        w-esp-scl-dtp-wlp    =    zero
                     move  "#"            to   w-esp-scl-dtp-sts
                     go to esp-scl-dtp-999.
      *                          *-------------------------------------*
      *                          * Segnale di prossima operazione da   *
      *                          * eseguire : Start sul livello di     *
      *                          * profondita' attuale                 *
      *                          *-------------------------------------*
           move      "S"                  to   w-esp-scl-dtp-wpo      .
      *                          *-------------------------------------*
      *                          * Riciclo per esecuzione Start        *
      *                          *-------------------------------------*
           go to     esp-scl-dtp-105.
       esp-scl-dtp-130.
      *                      *-----------------------------------------*
      *                      * Se 'Start' andata a buon fine : possono *
      *                      * esserci altri elementi nella distinta o *
      *                      * subdistinta                             *
      *                      *-----------------------------------------*
       esp-scl-dtp-135.
      *                          *-------------------------------------*
      *                          * Segnale di prossima operazione da   *
      *                          * eseguire : Read Next sul livello di *
      *                          * profondita' attuale                 *
      *                          *-------------------------------------*
           move      "N"                  to   w-esp-scl-dtp-wpo      .
      *                          *-------------------------------------*
      *                          * Riciclo per esecuzione Read Next    *
      *                          *-------------------------------------*
           go to     esp-scl-dtp-105.
       esp-scl-dtp-140.
      *                  *---------------------------------------------*
      *                  * Se operazione da eseguire : Incremento di   *
      *                  * un livello e Start sul livello di profondi- *
      *                  * ta' interno aumentato                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Incremento livello di profondita' in-   *
      *                      * terno                                   *
      *                      *-----------------------------------------*
           add       1                    to   w-esp-scl-dtp-wlp      .
      *                      *-----------------------------------------*
      *                      * Segnale di prossima operazione da ese-  *
      *                      * guire : Start sul livello di profondi-  *
      *                      * ta' interno attuale                     *
      *                      *-----------------------------------------*
           move      "S"                  to   w-esp-scl-dtp-wpo      .
      *                      *-----------------------------------------*
      *                      * Riciclo per esecuzione Start            *
      *                      *-----------------------------------------*
           go to     esp-scl-dtp-105.
       esp-scl-dtp-145.
      *                  *---------------------------------------------*
      *                  * Se operazione da eseguire : Read Next sul   *
      *                  * livello di profondita' attuale              *
      *                  *---------------------------------------------*
       esp-scl-dtp-150.
      *                      *-----------------------------------------*
      *                      * Esecuzione 'Read Next'                  *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgr                 .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda dell'esito della   *
      *                      * 'Read Next'                             *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     go to esp-scl-dtp-160.
       esp-scl-dtp-155.
      *                      *-----------------------------------------*
      *                      * Se 'At end' : non ci sono altri elemen- *
      *                      * ti nella distinta o subdistinta         *
      *                      *-----------------------------------------*
           go to     esp-scl-dtp-120.
       esp-scl-dtp-160.
      *                      *-----------------------------------------*
      *                      * Se 'Read Next' andata a buon fine       *
      *                      *-----------------------------------------*
       esp-scl-dtp-165.
      *                          *-------------------------------------*
      *                          * Test max per verificare se si e'    *
      *                          * raggiunta la fine della distinta o  *
      *                          * subdistinta, e deviazione a seconda *
      *                          * dell'esito del test                 *
      *                          *-------------------------------------*
           if        rf-lgr-tpm-ass       =    w-esp-scl-dtp-wti
                                              (w-esp-scl-dtp-wlp) and
                     rf-lgr-nrm-ass       =    w-esp-scl-dtp-wnu
                                              (w-esp-scl-dtp-wlp)
                     go to esp-scl-dtp-175.
       esp-scl-dtp-170.
      *                          *-------------------------------------*
      *                          * Se si e' raggiunta la fine della    *
      *                          * distinta o subdistinta              *
      *                          *-------------------------------------*
           go to     esp-scl-dtp-120.
       esp-scl-dtp-175.
      *                          *-------------------------------------*
      *                          * Se si e' ancora nell'ambito della   *
      *                          * distinta o subdistinta del livello  *
      *                          * di profondita' interno attuale      *
      *                          *-------------------------------------*
       esp-scl-dtp-180.
      *                              *---------------------------------*
      *                              * Aggiornamento del numero pro-   *
      *                              * gressivo riga relativo al li-   *
      *                              * vello di profondita' interno    *
      *                              * attuale                         *
      *                              *---------------------------------*
           move      rf-lgr-num-prg       to   w-esp-scl-dtp-wrg
                                              (w-esp-scl-dtp-wlp)     .
       esp-scl-dtp-185.
      *                              *---------------------------------*
      *                              * Salvataggio livello di profon-  *
      *                              * dita' interno attuale           *
      *                              *---------------------------------*
           move      w-esp-scl-dtp-wlp    to   w-esp-scl-dtp-wls      .
      *                              *---------------------------------*
      *                              * Incremento livello di profondi- *
      *                              * ta' interno attuale             *
      *                              *---------------------------------*
           add       1                    to   w-esp-scl-dtp-wlp      .
       esp-scl-dtp-190.
      *                              *---------------------------------*
      *                              * Preparazione valori per il li-  *
      *                              * vello di profondita' interno    *
      *                              * attuale aumentato di 1          *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Tipo elemento               *
      *                                  *-----------------------------*
           move      rf-lgr-tpm-cpt       to   w-esp-scl-dtp-wti
                                              (w-esp-scl-dtp-wlp)     .
      *                                  *-----------------------------*
      *                                  * Codice numerico elemento    *
      *                                  *-----------------------------*
           move      rf-lgr-nrm-cpt       to   w-esp-scl-dtp-wnu
                                              (w-esp-scl-dtp-wlp)     .
      *                                  *-----------------------------*
      *                                  * Codice alfanumerico elemen- *
      *                                  * to                          *
      *                                  *-----------------------------*
           move      rf-lgr-afm-cpt       to   w-esp-scl-dtp-wal
                                              (w-esp-scl-dtp-wlp)     .
      *                                  *-----------------------------*
      *                                  * Numero progressivo di riga  *
      *                                  * distinta                    *
      *                                  *-----------------------------*
           move      zero                to   w-esp-scl-dtp-wrg
                                             (w-esp-scl-dtp-wlp)      .
      *                                  *-----------------------------*
      *                                  * Lettura anagrafica elemento *
      *                                  * e determinazione dei se-    *
      *                                  * guenti valori :             *
      *                                  *  - Elemento anagraficamente *
      *                                  *    esistente                *
      *                                  *  - Descrizione anagrafica   *
      *                                  *    elemento                 *
      *                                  *  - Unita' di misura elemen- *
      *                                  *    to                       *
      *                                  *  - Numero decimali per la   *
      *                                  *    quantita'                *
      *                                  *-----------------------------*
           perform   esp-scl-dtp-800      thru esp-scl-dtp-809        .
      *                                  *-----------------------------*
      *                                  * Quantita' relativa all'ele- *
      *                                  * mento                       *
      *                                  *-----------------------------*
           move      w-esp-scl-dtp-wqt
                    (w-esp-scl-dtp-wls)  to   w-esp-scl-dtp-wqt
                                             (w-esp-scl-dtp-wlp)      .
           multiply  rf-lgr-qta-ipm      by   w-esp-scl-dtp-wqt
                                             (w-esp-scl-dtp-wlp)      .
           if        rf-lgr-qta-ipd      not  = zero
                     divide   rf-lgr-qta-ipd
                                         into w-esp-scl-dtp-wqt
                                             (w-esp-scl-dtp-wlp)      .
           if        w-esp-scl-dtp-wnd
                    (w-esp-scl-dtp-wlp)   =    zero
                     add     0,499        to   w-esp-scl-dtp-wqt
                                              (w-esp-scl-dtp-wlp)
                     divide   1000        into w-esp-scl-dtp-wqt
                                              (w-esp-scl-dtp-wlp)
                                                         rounded
                     multiply 1000        by   w-esp-scl-dtp-wqt
                                              (w-esp-scl-dtp-wlp)
           else if   w-esp-scl-dtp-wnd
                    (w-esp-scl-dtp-wlp)   =    1
                     add     0,049        to   w-esp-scl-dtp-wqt
                                              (w-esp-scl-dtp-wlp)
                     divide    100        into w-esp-scl-dtp-wqt
                                              (w-esp-scl-dtp-wlp)
                                                         rounded
                     multiply  100        by   w-esp-scl-dtp-wqt
                                              (w-esp-scl-dtp-wlp)
           else if   w-esp-scl-dtp-wnd
                    (w-esp-scl-dtp-wlp)   =    2
                     add     0,004        to   w-esp-scl-dtp-wqt
                                              (w-esp-scl-dtp-wlp)
                     divide     10        into w-esp-scl-dtp-wqt
                                              (w-esp-scl-dtp-wlp)
                                                         rounded
                     multiply   10        by   w-esp-scl-dtp-wqt
                                              (w-esp-scl-dtp-wlp)     .
       esp-scl-dtp-195.
      *                              *---------------------------------*
      *                              * Preparazione valori in uscita,  *
      *                              * escluso lo status, relativi     *
      *                              * all'elemento                    *
      *                              *---------------------------------*
           perform   esp-scl-dtp-820      thru esp-scl-dtp-829        .
       esp-scl-dtp-200.
      *                              *---------------------------------*
      *                              * Preparazione della prossima o-  *
      *                              * perazione da eseguire, a se-    *
      *                              * conda del tipo di elemento      *
      *                              *                                 *
      *                              *   - Prodotto finito       : 'U' *
      *                              *                                 *
      *                              *   - Semilavorato          : 'U' *
      *                              *                                 *
      *                              *   - Materia prima         : 'N' *
      *                              *                                 *
      *                              *   - Subdistinta virtuale  : 'U' *
      *                              *                                 *
      *                              *   - Tipo non riconosciuto : 'N' *
      *                              *                                 *
      *                              *---------------------------------*
           if        w-esp-scl-dtp-wti
                    (w-esp-scl-dtp-wlp)   =    01 or
                     w-esp-scl-dtp-wti
                    (w-esp-scl-dtp-wlp)   =    02 or
                     w-esp-scl-dtp-wti
                    (w-esp-scl-dtp-wlp)   =    99
                     move  "U"            to   w-esp-scl-dtp-wpo
           else      move  "N"            to   w-esp-scl-dtp-wpo      .
       esp-scl-dtp-205.
      *                              *---------------------------------*
      *                              * Ripristino livello di profon-   *
      *                              * dita' interno attuale           *
      *                              *---------------------------------*
           move      w-esp-scl-dtp-wls    to   w-esp-scl-dtp-wlp      .
       esp-scl-dtp-210.
      *                              *---------------------------------*
      *                              * Status di uscita a : Ok         *
      *                              *---------------------------------*
           move      spaces               to   w-esp-scl-dtp-sts      .
       esp-scl-dtp-215.
      *                              *---------------------------------*
      *                              * Uscita                          *
      *                              *---------------------------------*
           go to     esp-scl-dtp-999.
       esp-scl-dtp-250.
      *              *-------------------------------------------------*
      *              * Funzione 'E-SKP'                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se il segnale di prossima operazione da e-  *
      *                  * seguire e' pari a 'U' lo si porta a 'N'     *
      *                  *---------------------------------------------*
           if        w-esp-scl-dtp-wpo    =    "U"
                     move  "N"            to   w-esp-scl-dtp-wpo      .
      *                  *---------------------------------------------*
      *                  * Dopodiche' si esegue la funzione 'E-GET'    *
      *                  *---------------------------------------------*
           go to     esp-scl-dtp-100.
       esp-scl-dtp-800.
      *              *-------------------------------------------------*
      *              * Subroutine per la lettura dell'anagrafica rela- *
      *              * tiva all'elemento di tipo e codice numerico di  *
      *              * cui al livello di profondita' interna definito  *
      *              * in : w-esp-scl-dtp-wlp                          *
      *              *-------------------------------------------------*
       esp-scl-dtp-801.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo elemento      *
      *                  *---------------------------------------------*
           if        w-esp-scl-dtp-wti
                    (w-esp-scl-dtp-wlp)   =    01
                     go to esp-scl-dtp-802
           else if   w-esp-scl-dtp-wti
                    (w-esp-scl-dtp-wlp)   =    02
                     go to esp-scl-dtp-803
           else if   w-esp-scl-dtp-wti
                    (w-esp-scl-dtp-wlp)   =    03
                     go to esp-scl-dtp-804
           else if   w-esp-scl-dtp-wti
                    (w-esp-scl-dtp-wlp)   =    99
                     go to esp-scl-dtp-805
           else      go to esp-scl-dtp-808.
       esp-scl-dtp-802.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento 01 : Prodotto finito       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica [dcp]                *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO"             to   f-key                  .
           move      w-esp-scl-dtp-wnu
                    (w-esp-scl-dtp-wlp)   to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                      *-----------------------------------------*
      *                      * Se anagrafica non esistente             *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-esp-scl-dtp-wes
                                              (w-esp-scl-dtp-wlp)
                     move  all "."        to   w-esp-scl-dtp-wde
                                              (w-esp-scl-dtp-wlp)
                     move  all "."        to   w-esp-scl-dtp-wum
                                              (w-esp-scl-dtp-wlp)
                     move  zero           to   w-esp-scl-dtp-wnd
                                              (w-esp-scl-dtp-wlp)
                     go to esp-scl-dtp-809.
      *                      *-----------------------------------------*
      *                      * Se anagrafica esistente                 *
      *                      *-----------------------------------------*
           move      spaces               to   w-esp-scl-dtp-wes
                                              (w-esp-scl-dtp-wlp)     .
           move      rf-dcp-des-pro       to   w-esp-scl-dtp-wde
                                              (w-esp-scl-dtp-wlp)     .
           move      rf-dcp-umi-ven       to   w-esp-scl-dtp-wum
                                              (w-esp-scl-dtp-wlp)     .
           move      rf-dcp-dec-qta       to   w-esp-scl-dtp-wnd
                                              (w-esp-scl-dtp-wlp)     .
           go to     esp-scl-dtp-809.
       esp-scl-dtp-803.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento 02 : Semilavorato          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica [dps]                *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMSEM"             to   f-key                  .
           move      w-esp-scl-dtp-wnu
                    (w-esp-scl-dtp-wlp)   to   rf-dps-num-sem         .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
      *                      *-----------------------------------------*
      *                      * Se anagrafica non esistente             *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-esp-scl-dtp-wes
                                              (w-esp-scl-dtp-wlp)
                     move  all "."        to   w-esp-scl-dtp-wde
                                              (w-esp-scl-dtp-wlp)
                     move  all "."        to   w-esp-scl-dtp-wum
                                              (w-esp-scl-dtp-wlp)
                     move  zero           to   w-esp-scl-dtp-wnd
                                              (w-esp-scl-dtp-wlp)
                     go to esp-scl-dtp-809.
      *                      *-----------------------------------------*
      *                      * Se anagrafica esistente                 *
      *                      *-----------------------------------------*
           move      spaces               to   w-esp-scl-dtp-wes
                                              (w-esp-scl-dtp-wlp)     .
           move      rf-dps-des-sem       to   w-esp-scl-dtp-wde
                                              (w-esp-scl-dtp-wlp)     .
           move      rf-dps-umi-prd       to   w-esp-scl-dtp-wum
                                              (w-esp-scl-dtp-wlp)     .
           move      rf-dps-dec-qta       to   w-esp-scl-dtp-wnd
                                              (w-esp-scl-dtp-wlp)     .
           go to     esp-scl-dtp-809.
       esp-scl-dtp-804.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento 03 : Materia prima         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica [dpm]                *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMMAP"             to   f-key                  .
           move      w-esp-scl-dtp-wnu
                    (w-esp-scl-dtp-wlp)   to   rf-dpm-num-map         .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
      *                      *-----------------------------------------*
      *                      * Se anagrafica non esistente             *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-esp-scl-dtp-wes
                                              (w-esp-scl-dtp-wlp)
                     move  all "."        to   w-esp-scl-dtp-wde
                                              (w-esp-scl-dtp-wlp)
                     move  all "."        to   w-esp-scl-dtp-wum
                                              (w-esp-scl-dtp-wlp)
                     move  zero           to   w-esp-scl-dtp-wnd
                                              (w-esp-scl-dtp-wlp)
                     go to esp-scl-dtp-809.
      *                      *-----------------------------------------*
      *                      * Se anagrafica esistente                 *
      *                      *-----------------------------------------*
           move      spaces               to   w-esp-scl-dtp-wes
                                              (w-esp-scl-dtp-wlp)     .
           move      rf-dpm-des-map       to   w-esp-scl-dtp-wde
                                              (w-esp-scl-dtp-wlp)     .
           move      rf-dpm-umi-prd       to   w-esp-scl-dtp-wum
                                              (w-esp-scl-dtp-wlp)     .
           move      rf-dpm-dec-qta       to   w-esp-scl-dtp-wnd
                                              (w-esp-scl-dtp-wlp)     .
           go to     esp-scl-dtp-809.
       esp-scl-dtp-805.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento 99 : Subdistinta virtuale  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica [lgv]                *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMLGV"             to   f-key                  .
           move      w-esp-scl-dtp-wnu
                    (w-esp-scl-dtp-wlp)   to   rf-lgv-num-lgv         .
           move      "pgm/dtp/fls/ioc/obj/ioflgv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgv                 .
      *                      *-----------------------------------------*
      *                      * Se anagrafica non esistente             *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-esp-scl-dtp-wes
                                              (w-esp-scl-dtp-wlp)
                     move  all "."        to   w-esp-scl-dtp-wde
                                              (w-esp-scl-dtp-wlp)
                     move  all "."        to   w-esp-scl-dtp-wum
                                              (w-esp-scl-dtp-wlp)
                     move  zero           to   w-esp-scl-dtp-wnd
                                              (w-esp-scl-dtp-wlp)
                     go to esp-scl-dtp-809.
      *                      *-----------------------------------------*
      *                      * Se anagrafica esistente                 *
      *                      *-----------------------------------------*
           move      spaces               to   w-esp-scl-dtp-wes
                                              (w-esp-scl-dtp-wlp)     .
           move      rf-lgv-des-lgv       to   w-esp-scl-dtp-wde
                                              (w-esp-scl-dtp-wlp)     .
           move      rf-lgv-umi-prd       to   w-esp-scl-dtp-wum
                                              (w-esp-scl-dtp-wlp)     .
           move      rf-lgv-dec-qta       to   w-esp-scl-dtp-wnd
                                              (w-esp-scl-dtp-wlp)     .
           go to     esp-scl-dtp-809.
       esp-scl-dtp-808.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento di tipo non riconosciuto   *
      *                  *---------------------------------------------*
           move      "#"                  to   w-esp-scl-dtp-wes
                                              (w-esp-scl-dtp-wlp)     .
           move      all  "."             to   w-esp-scl-dtp-wde
                                              (w-esp-scl-dtp-wlp)     .
           move      all  "."             to   w-esp-scl-dtp-wum
                                              (w-esp-scl-dtp-wlp)     .
           move      zero                 to   w-esp-scl-dtp-wnd
                                              (w-esp-scl-dtp-wlp)     .
           go to     esp-scl-dtp-809.
       esp-scl-dtp-809.
           exit.
       esp-scl-dtp-820.
      *              *-------------------------------------------------*
      *              * Subroutine per la preparazione dei valori in u- *
      *              * scita relativi all'elemento di cui al livello   *
      *              * di profondita' di : w-esp-scl-dtp-wlp.          *
      *              * Ad esclusione dello status di uscita            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo elemento                               *
      *                  *---------------------------------------------*
           move      w-esp-scl-dtp-wti
                    (w-esp-scl-dtp-wlp)   to   w-esp-scl-dtp-tip      .
      *                  *---------------------------------------------*
      *                  * Codice numerico elemento                    *
      *                  *---------------------------------------------*
           move      w-esp-scl-dtp-wnu
                    (w-esp-scl-dtp-wlp)   to   w-esp-scl-dtp-num      .
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico elemento                *
      *                  *---------------------------------------------*
           move      w-esp-scl-dtp-wal
                    (w-esp-scl-dtp-wlp)   to   w-esp-scl-dtp-alf      .
      *                  *---------------------------------------------*
      *                  * Livello di profondita' esterno, pari al li- *
      *                  * vello di profondita' interno diminuito di 1 *
      *                  *---------------------------------------------*
           move      w-esp-scl-dtp-wlp    to   w-esp-scl-dtp-liv      .
           subtract  1                    from w-esp-scl-dtp-liv      .
      *                  *---------------------------------------------*
      *                  * Quantita' relativa all'elemento             *
      *                  *---------------------------------------------*
           move      w-esp-scl-dtp-wqt
                    (w-esp-scl-dtp-wlp)   to   w-esp-scl-dtp-qta      .
      *                  *---------------------------------------------*
      *                  * Flag di anagrafica elemento esistente       *
      *                  *---------------------------------------------*
           move      w-esp-scl-dtp-wes
                    (w-esp-scl-dtp-wlp)   to   w-esp-scl-dtp-ana      .
      *                  *---------------------------------------------*
      *                  * Descrizione da anagrafica elemento          *
      *                  *---------------------------------------------*
           move      w-esp-scl-dtp-wde
                    (w-esp-scl-dtp-wlp)   to   w-esp-scl-dtp-des      .
      *                  *---------------------------------------------*
      *                  * Unita' di misura da anagrafica elemento     *
      *                  *---------------------------------------------*
           move      w-esp-scl-dtp-wum
                    (w-esp-scl-dtp-wlp)   to   w-esp-scl-dtp-umi      .
      *                  *---------------------------------------------*
      *                  * Numero decimali quantita' da anagrafica e-  *
      *                  * lemento                                     *
      *                  *---------------------------------------------*
           move      w-esp-scl-dtp-wnd
                    (w-esp-scl-dtp-wlp)   to   w-esp-scl-dtp-dec      .
      *                  *---------------------------------------------*
      *                  * Se anagrafica esistente si esce, altrimenti *
      *                  * si prepara la descrizione con il codice nu- *
      *                  * merico dell'elemento e con puntini          *
      *                  *---------------------------------------------*
           if        w-esp-scl-dtp-ana    =    spaces
                     go to esp-scl-dtp-829.
           move      all   "."            to   w-esp-scl-dtp-des      .
           string    "("
                                delimited by   size
                     w-esp-scl-dtp-num
                                delimited by   size
                     ")"
                                delimited by   size
                                          into w-esp-scl-dtp-des      .
       esp-scl-dtp-829.
           exit.
       esp-scl-dtp-999.
           exit.

      *    *===========================================================*
      *    * Subroutine di implosione scalare distinta base            *
      *    *                                                           *
      *    * ----------------                                          *
      *    *                                                           *
      *    * Attenzione : Questa routine utilizza numerosi file, che   *
      *    *              sono di seguito elencati. Essi vengono tut-  *
      *    *              ti trattati in lettura, ma nel far questo    *
      *    *              vengono rovinate le Start eventualmente ese- *
      *    *              guite su di essi.                            *
      *    *                                                           *
      *    *              Pertanto i programmi che intendono utilizza- *
      *    *              re la seguente routine non devono assoluta-  *
      *    *              mente utilizzare come files principali i fi- *
      *    *              les utilizzati :                             *
      *    *                                                           *
      *    *                  - [lgr]                                  *
      *    *                  - [dcp]                                  *
      *    *                  - [dps]                                  *
      *    *                  - [dpm]                                  *
      *    *                  - [lgv]                                  *
      *    *                                                           *
      *    *              Inoltre non e' possibile utilizzare nel ci-  *
      *    *              clo principale nemmeno i moduli di filtro    *
      *    *              per ordinamento e/o selezione anagrafiche,   *
      *    *              in quanto questi, in alcune circostanze,     *
      *    *              utilizzano a loro volta come files guida i   *
      *    *              files :                                      *
      *    *                                                           *
      *    *                  - [dcp]                                  *
      *    *                  - [dps]                                  *
      *    *                  - [dpm]                                  *
      *    *                                                           *
      *    *              Nei casi in cui si dovesse assolutamente a-  *
      *    *              vere un file guida tra quelli non consentiti *
      *    *              da questa routine, sara' necessario :        *
      *    *                                                           *
      *    *                  - Preparare preventivamente un file tem- *
      *    *                    poraneo di appoggio come file guida    *
      *    *                                                           *
      *    *                  - Utilizzare tale file temporaneo di ap- *
      *    *                    poggio come file guida                 *
      *    *                                                           *
      *    *                  - Cancellare tale file temporaneo di ap- *
      *    *                    poggio alla fine dell'esecuzione       *
      *    *                                                           *
      *    *                  Nota : Il file di appoggio temporaneo    *
      *    *                         puo' essere costituito da un fi-  *
      *    *                         le di sort, in quanto questa ro-  *
      *    *                         utine non utilizza il sort.       *
      *    *                                                           *
      *    * ----------------                                          *
      *    *                                                           *
      *    * Presume che preventivamente siano stati definiti ed aper- *
      *    * ti i seguenti files:                                      *
      *    *                                                           *
      *    *  - [lgr] : Righe distinte base                            *
      *    *  - [dcp] : Anagrafica prodotti finiti                     *
      *    *  - [dps] : Anagrafica semilavorati                        *
      *    *  - [dpm] : Anagrafica materie prime                       *
      *    *  - [lgv] : Anagrafica subdistinte virtuali                *
      *    *                                                           *
      *    * ----------------                                          *
      *    *                                                           *
      *    * - Funzione 'E-INI'                                        *
      *    *                                                           *
      *    *   Deve necessariamente essere richiamata, per ogni implo- *
      *    *   sione da eseguire, prima delle altre funzioni.          *
      *    *                                                           *
      *    *   - Input  : w-imp-scl-dtp-fun = 'E-INI'                  *
      *    *                                                           *
      *    *              w-imp-scl-dtp-tdi = Tipo codice magazzino    *
      *    *                                  del componente da im-    *
      *    *                                  plodere                  *
      *    *                                   - 02 : Semilavorato     *
      *    *                                   - 03 : Materia prima    *
      *    *                                   - 99 : Subdistinta vir- *
      *    *                                          tuale            *
      *    *                                                           *
      *    *              w-imp-scl-dtp-ndi = Codice numerico magazzi- *
      *    *                                  no o codice numerico     *
      *    *                                  della subdistinta virtu- *
      *    *                                  ale da implodere         *
      *    *                                                           *
      *    *              w-imp-scl-dtp-qdi = Quantita' da implodere   *
      *    *                                                           *
      *    *   - Output : w-imp-scl-dtp-tco = Tipo di componente       *
      *    *                                                           *
      *    *              w-imp-scl-dtp-nco = Codice numerico del      *
      *    *                                  componente               *
      *    *                                                           *
      *    *              w-imp-scl-dtp-tas = Tipo di assieme          *
      *    *                                                           *
      *    *              w-imp-scl-dtp-nas = Codice numerico del-     *
      *    *                                  l'assieme                *
      *    *                                                           *
      *    *              w-imp-scl-dtp-liv = Livello di profondita'   *
      *    *                                                           *
      *    *              w-imp-scl-dtp-qta = Quantita' producibile    *
      *    *                                  relativa all'assieme     *
      *    *                                                           *
      *    *              w-imp-scl-dtp-ana = Segnale di anagrafica e- *
      *    *                                  sistente per l'assieme   *
      *    *                                   - Spaces : Si           *
      *    *                                   - '#'    : No           *
      *    *                                                           *
      *    *              w-imp-scl-dtp-des = Descrizione da anagrafi- *
      *    *                                  ca assieme               *
      *    *                                                           *
      *    *              w-imp-scl-dtp-umi = Unita' di misura da ana- *
      *    *                                  grafica assieme          *
      *    *                                                           *
      *    *              w-imp-scl-dtp-dec = Numero decimali quantita'*
      *    *                                  da anagrafica assieme    *
      *    *                                                           *
      *    *              w-imp-scl-dtp-cum = Sigla unita' di misura   *
      *    *                                  da anagrafica componente *
      *    *                                                           *
      *    *              w-imp-scl-dtp-cnd = Numero decimali quantita'*
      *    *                                  da anagrafica componente *
      *    *                                                           *
      *    *              w-imp-scl-dtp-cmu = Quantita' di utilizzo,   *
      *    *                                  moltiplicatore           *
      *    *                                                           *
      *    *              w-imp-scl-dtp-cdu = Quantita' di utilizzo,   *
      *    *                                  divisore                 *
      *    *                                                           *
      *    *              rf-dcp, oppure    = Anagrafica assieme, a    *
      *    *              rf-dps, oppure      seconda del tipo ele-    *
      *    *              rf-dpm, oppure      mento, se anagrafica e-  *
      *    *              rf-lgv              sistente                 *
      *    *                                                           *
      *    * ----------------                                          *
      *    *                                                           *
      *    * - Funzione 'E-GET'                                        *
      *    *                                                           *
      *    *   Ottenimento dell'elemento successivo relativo all'im-   *
      *    *   plosione scalare.                                       *
      *    *                                                           *
      *    *   - Input  : w-imp-scl-dtp-fun = 'E-GET'                  *
      *    *                                                           *
      *    *   - Output : w-imp-scl-dtp-sts = Status di uscita         *
      *    *                                   - Spaces : Ok, elemento *
      *    *                                              successivo a *
      *    *                                              disposizione *
      *    *                                   - '#'    : Fine elemen- *
      *    *                                              ti nella     *
      *    *                                              scansione    *
      *    *                                                           *
      *    *              w-imp-scl-dtp-tco = Tipo di componente       *
      *    *                                                           *
      *    *              w-imp-scl-dtp-nco = Codice numerico del      *
      *    *                                  componente               *
      *    *                                                           *
      *    *              w-imp-scl-dtp-tas = Tipo di assieme          *
      *    *                                                           *
      *    *              w-imp-scl-dtp-nas = Codice numerico del-     *
      *    *                                  l'assieme                *
      *    *                                                           *
      *    *              w-imp-scl-dtp-liv = Livello di profondita'   *
      *    *                                                           *
      *    *              w-imp-scl-dtp-qta = Quantita' producibile    *
      *    *                                  relativa all'assieme     *
      *    *                                                           *
      *    *              w-imp-scl-dtp-ana = Segnale di anagrafica e- *
      *    *                                  sistente per l'assieme   *
      *    *                                   - Spaces : Si           *
      *    *                                   - '#'    : No           *
      *    *                                                           *
      *    *              w-imp-scl-dtp-des = Descrizione da anagrafi- *
      *    *                                  ca assieme               *
      *    *                                                           *
      *    *              w-imp-scl-dtp-umi = Unita' di misura da ana- *
      *    *                                  grafica assieme          *
      *    *                                                           *
      *    *              w-imp-scl-dtp-dec = Numero decimali quantita'*
      *    *                                  da anagrafica assieme    *
      *    *                                                           *
      *    *              w-imp-scl-dtp-cum = Sigla unita' di misura   *
      *    *                                  da anagrafica componente *
      *    *                                                           *
      *    *              w-imp-scl-dtp-cnd = Numero decimali quantita'*
      *    *                                  da anagrafica componente *
      *    *                                                           *
      *    *              w-imp-scl-dtp-cmu = Quantita' di utilizzo,   *
      *    *                                  moltiplicatore           *
      *    *                                                           *
      *    *              w-imp-scl-dtp-cdu = Quantita' di utilizzo,   *
      *    *                                  divisore                 *
      *    *                                                           *
      *    *              rf-dcp, oppure    = Anagrafica assieme, a    *
      *    *              rf-dps, oppure      seconda del tipo ele-    *
      *    *              rf-dpm, oppure      mento, se anagrafica e-  *
      *    *              rf-lgv              sistente                 *
      *    *                                                           *
      *    * ----------------                                          *
      *    *                                                           *
      *    * - Funzione 'E-SKP'                                        *
      *    *                                                           *
      *    *   Ottenimento dell'elemento successivo dello stesso li-   *
      *    *   vello dell'ultimo elemento trovato, evitando in tal     *
      *    *   modo di entrare piu' in profondita' per quanto riguar-  *
      *    *   da gli utilizzi.                                        *
      *    *                                                           *
      *    *   - Input  : w-imp-scl-dtp-fun = 'E-SKP'                  *
      *    *                                                           *
      *    *   - Output : Esattamente come per funzione 'E-GET'        *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       imp-scl-dtp-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda della funzione richiesta   *
      *              *-------------------------------------------------*
           if        w-imp-scl-dtp-fun    =    "E-INI"
                     go to imp-scl-dtp-050
           else if   w-imp-scl-dtp-fun    =    "E-GET"
                     go to imp-scl-dtp-100
           else if   w-imp-scl-dtp-fun    =    "E-SKP"
                     go to imp-scl-dtp-250
           else      go to imp-scl-dtp-999.
       imp-scl-dtp-050.
      *              *-------------------------------------------------*
      *              * Funzione 'E-INI'                                *
      *              *-------------------------------------------------*
       imp-scl-dtp-055.
      *                  *---------------------------------------------*
      *                  * Livello di lavoro a : 01                    *
      *                  *---------------------------------------------*
           move      01                   to   w-imp-scl-dtp-wlp      .
      *                  *---------------------------------------------*
      *                  * Segnale di prossima operazione da eseguire  *
      *                  * a : Start sul livello di profondita interno *
      *                  * attuale                                     *
      *                  *---------------------------------------------*
           move      "S"                  to   w-imp-scl-dtp-wpo      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione dei seguenti valori :       *
      *                  *  - Ultimo tipo componente letto             *
      *                  *  - Ultimo codice numerico componente letto  *
      *                  *  - Ultima unita' di misura componente letta *
      *                  *  - Ultimo numero decimali componente letto  *
      *                  *---------------------------------------------*
           move      zero                 to   w-imp-scl-dtp-utc      .
           move      zero                 to   w-imp-scl-dtp-ucc      .
           move      spaces               to   w-imp-scl-dtp-uuc      .
           move      zero                 to   w-imp-scl-dtp-udc      .
       imp-scl-dtp-060.
      *                  *---------------------------------------------*
      *                  * Preparazione valori per il 1. livello       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo di componente                      *
      *                      *-----------------------------------------*
           move      w-imp-scl-dtp-tdi    to   w-imp-scl-dtp-wtc (01) .
      *                      *-----------------------------------------*
      *                      * Codice numerico componente              *
      *                      *-----------------------------------------*
           move      w-imp-scl-dtp-ndi    to   w-imp-scl-dtp-wnc (01) .
      *                      *-----------------------------------------*
      *                      * Tipo di assieme                         *
      *                      *-----------------------------------------*
           move      zero                 to   w-imp-scl-dtp-wta (01) .
      *                      *-----------------------------------------*
      *                      * Codice numerico assieme                 *
      *                      *-----------------------------------------*
           move      zero                 to   w-imp-scl-dtp-wna (01) .
      *                      *-----------------------------------------*
      *                      * Numero riga in distinta                 *
      *                      *-----------------------------------------*
           move      zero                 to   w-imp-scl-dtp-wrg (01) .
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica componente e deter-  *
      *                      * minazione dell'unita' di misura e del   *
      *                      * numero decimali quantita' relativi al   *
      *                      * componente                              *
      *                      *-----------------------------------------*
           perform   imp-scl-dtp-800      thru imp-scl-dtp-809        .
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica assieme e determina- *
      *                      * zione dei seguenti valori:              *
      *                      *  - Assieme anagraficamente esistente    *
      *                      *  - Descrizione anagrafica assieme       *
      *                      *  - Unita' di misura assieme             *
      *                      *  - Numero decimali per quantita' per    *
      *                      *    l'assieme                            *
      *                      *-----------------------------------------*
           perform   imp-scl-dtp-810      thru imp-scl-dtp-819        .
      *                      *-----------------------------------------*
      *                      * Quantita' di utilizzo, moltiplicatore   *
      *                      *-----------------------------------------*
           move      1,000                to   w-imp-scl-dtp-wcm (01) .
      *                      *-----------------------------------------*
      *                      * Quantita' di utilizzo, divisore         *
      *                      *-----------------------------------------*
           move      1,000                to   w-imp-scl-dtp-wcd (01) .
      *                      *-----------------------------------------*
      *                      * Quantita' producibile relativa all'as-  *
      *                      * sieme relativa al primo livello         *
      *                      *-----------------------------------------*
           move      w-imp-scl-dtp-qdi    to   w-imp-scl-dtp-wqt (01) .
           if        w-imp-scl-dtp-wnd (01)
                                          =    zero
                     subtract 0,499       from w-imp-scl-dtp-wqt (01)
                     divide   1000        into w-imp-scl-dtp-wqt (01)
                                                         rounded
                     multiply 1000        by   w-imp-scl-dtp-wqt (01)
           else if   w-imp-scl-dtp-wnd (01)
                                          =    1
                     subtract  0,049      from w-imp-scl-dtp-wqt (01)
                     divide    100        into w-imp-scl-dtp-wqt (01)
                                                         rounded
                     multiply  100        by   w-imp-scl-dtp-wqt (01)
           else if   w-imp-scl-dtp-wnd (01)
                                          =    2
                     subtract   0,004     from w-imp-scl-dtp-wqt (01)
                     divide     10        into w-imp-scl-dtp-wqt (01)
                                                         rounded
                     multiply   10        by   w-imp-scl-dtp-wqt (01) .
       imp-scl-dtp-065.
      *                  *---------------------------------------------*
      *                  * Preparazione valori in uscita, escluso lo   *
      *                  * status, relativi all'assieme                *
      *                  *---------------------------------------------*
           perform   imp-scl-dtp-820      thru imp-scl-dtp-829        .
       imp-scl-dtp-070.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     imp-scl-dtp-999.
       imp-scl-dtp-100.
      *              *-------------------------------------------------*
      *              * Funzione 'E-GET'                                *
      *              *-------------------------------------------------*
       imp-scl-dtp-105.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del;valore del segnale *
      *                  * di prossima operazione da eseguire          *
      *                  *---------------------------------------------*
           if        w-imp-scl-dtp-wpo    =    "S"
                     go to imp-scl-dtp-110
           else if   w-imp-scl-dtp-wpo    =    "U"
                     go to imp-scl-dtp-140
           else      go to imp-scl-dtp-145.
       imp-scl-dtp-110.
      *                  *---------------------------------------------*
      *                  * Se operazione da eseguire : Start sul li-   *
      *                  * vello di profondita' interno attuale        *
      *                  *---------------------------------------------*
       imp-scl-dtp-115.
      *                      *-----------------------------------------*
      *                      * Esecuzione 'Start'                      *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "GT"                 to   f-cfr                  .
           move      "CPTASS    "         to   f-key                  .
           move      w-imp-scl-dtp-wtc
                    (w-imp-scl-dtp-wlp)   to   rf-lgr-tpm-cpt         .
           move      w-imp-scl-dtp-wnc
                    (w-imp-scl-dtp-wlp)   to   rf-lgr-nrm-cpt         .
           move      w-imp-scl-dtp-wta
                    (w-imp-scl-dtp-wlp)   to   rf-lgr-tpm-ass         .
           move      w-imp-scl-dtp-wna
                    (w-imp-scl-dtp-wlp)   to   rf-lgr-nrm-ass         .
           move      w-imp-scl-dtp-wrg
                    (w-imp-scl-dtp-wlp)   to   rf-lgr-num-prg         .
           move      "pgm/dtp/fls/ioc/obj/ioflgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgr                 .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda dell'esito della   *
      *                      * 'Start'                                 *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     go to imp-scl-dtp-130.
       imp-scl-dtp-120.
      *                      *-----------------------------------------*
      *                      * Se 'Start' errata : non ci sono altri   *
      *                      * elementi nella lista                    *
      *                      *-----------------------------------------*
       imp-scl-dtp-125.
      *                          *-------------------------------------*
      *                          * Decremento livello di profondita'   *
      *                          * interno                             *
      *                          *-------------------------------------*
           subtract  1                    from w-imp-scl-dtp-wlp      .
      *                          *-------------------------------------*
      *                          * Se raggiunto il livello di profon-  *
      *                          * dita' interno zero : uscita con se- *
      *                          * gnale di fine scansione             *
      *                          *-------------------------------------*
           if        w-imp-scl-dtp-wlp    =    zero
                     move  "#"            to   w-imp-scl-dtp-sts
                     go to imp-scl-dtp-999.
      *                          *-------------------------------------*
      *                          * Segnale di prossima operazione da   *
      *                          * eseguire : Start sul livello di     *
      *                          * profondita' attuale                 *
      *                          *-------------------------------------*
           move      "S"                  to   w-imp-scl-dtp-wpo      .
      *                          *-------------------------------------*
      *                          * Riciclo per esecuzione Start        *
      *                          *-------------------------------------*
           go to     imp-scl-dtp-105.
       imp-scl-dtp-130.
      *                      *-----------------------------------------*
      *                      * Se 'Start' andata a buon fine : possono *
      *                      * esserci altri elementi lista            *
      *                      * subdistinta                             *
      *                      *-----------------------------------------*
       imp-scl-dtp-135.
      *                          *-------------------------------------*
      *                          * Segnale di prossima operazione da   *
      *                          * eseguire : Read Next sul livello di *
      *                          * profondita' attuale                 *
      *                          *-------------------------------------*
           move      "N"                  to   w-imp-scl-dtp-wpo      .
      *                          *-------------------------------------*
      *                          * Riciclo per esecuzione Read Next    *
      *                          *-------------------------------------*
           go to     imp-scl-dtp-105.
       imp-scl-dtp-140.
      *                  *---------------------------------------------*
      *                  * Se operazione da eseguire : Incremento di   *
      *                  * un livello e Start sul livello di profondi- *
      *                  * ta' interno aumentato                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Incremento livello di profondita' in-   *
      *                      * terno                                   *
      *                      *-----------------------------------------*
           add       1                    to   w-imp-scl-dtp-wlp      .
      *                      *-----------------------------------------*
      *                      * Segnale di prossima operazione da ese-  *
      *                      * guire : Start sul livello di profondi-  *
      *                      * ta' interno attuale                     *
      *                      *-----------------------------------------*
           move      "S"                  to   w-imp-scl-dtp-wpo      .
      *                      *-----------------------------------------*
      *                      * Riciclo per esecuzione Start            *
      *                      *-----------------------------------------*
           go to     imp-scl-dtp-105.
       imp-scl-dtp-145.
      *                  *---------------------------------------------*
      *                  * Se operazione da eseguire : Read Next sul   *
      *                  * livello di profondita' attuale              *
      *                  *---------------------------------------------*
       imp-scl-dtp-150.
      *                      *-----------------------------------------*
      *                      * Esecuzione 'Read Next'                  *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgr                 .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda dell'esito della   *
      *                      * 'Read Next'                             *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     go to imp-scl-dtp-160.
       imp-scl-dtp-155.
      *                      *-----------------------------------------*
      *                      * Se 'At end' : non ci sono altri elemen- *
      *                      * ti nella lista                          *
      *                      *-----------------------------------------*
           go to     imp-scl-dtp-120.
       imp-scl-dtp-160.
      *                      *-----------------------------------------*
      *                      * Se 'Read Next' andata a buon fine       *
      *                      *-----------------------------------------*
       imp-scl-dtp-165.
      *                          *-------------------------------------*
      *                          * Test max per verificare se si e'    *
      *                          * raggiunta la fine della lista, e    *
      *                          * deviazione a seconda dell'esito     *
      *                          * del test                            *
      *                          *-------------------------------------*
           if        rf-lgr-tpm-cpt       =    w-imp-scl-dtp-wtc
                                              (w-imp-scl-dtp-wlp) and
                     rf-lgr-nrm-cpt       =    w-imp-scl-dtp-wnc
                                              (w-imp-scl-dtp-wlp)
                     go to imp-scl-dtp-175.
       imp-scl-dtp-170.
      *                          *-------------------------------------*
      *                          * Se si e' raggiunta la fine della    *
      *                          * lista                               *
      *                          *-------------------------------------*
           go to     imp-scl-dtp-120.
       imp-scl-dtp-175.
      *                          *-------------------------------------*
      *                          * Se si e' ancora nell'ambito della   *
      *                          * lista del livello di profondita'    *
      *                          * interno attuale                     *
      *                          *-------------------------------------*
       imp-scl-dtp-180.
      *                              *---------------------------------*
      *                              * Aggiornamento del tipo assieme  *
      *                              * relativo al livello di profon-  *
      *                              * dita' interno attuale           *
      *                              *---------------------------------*
           move      rf-lgr-tpm-ass       to   w-imp-scl-dtp-wta
                                              (w-imp-scl-dtp-wlp)     .
      *                              *---------------------------------*
      *                              * Aggiornamento del codice nume-  *
      *                              * rico dell'assieme relativo al   *
      *                              * ivello di profondita' interno   *
      *                              * attuale                         *
      *                              *---------------------------------*
           move      rf-lgr-nrm-ass       to   w-imp-scl-dtp-wna
                                              (w-imp-scl-dtp-wlp)     .
      *                              *---------------------------------*
      *                              * Aggiornamento del numero pro-   *
      *                              * gressivo riga relativo al li-   *
      *                              * vello di profondita' interno    *
      *                              * attuale                         *
      *                              *---------------------------------*
           move      rf-lgr-num-prg       to   w-imp-scl-dtp-wrg
                                              (w-imp-scl-dtp-wlp)     .
       imp-scl-dtp-185.
      *                              *---------------------------------*
      *                              * Lettura anagrafica componente e *
      *                              * determinazione dell'unita' di   *
      *                              * misura e del numero decimali    *
      *                              * quantita' relativi al componen- *
      *                              * te                              *
      *                              *---------------------------------*
           perform   imp-scl-dtp-800      thru imp-scl-dtp-809        .
      *                              *---------------------------------*
      *                              * Lettura anagrafica assieme e    *
      *                              * determinazione dei seguenti     *
      *                              * valori :                        *
      *                              *  - Assieme anagraficamente esi- *
      *                              *    stente                       *
      *                              *  - Descrizione anagrafica per   *
      *                              *    l'assieme                    *
      *                              *  - Unita' di misura assieme     *
      *                              *  - Numero decimali per la quan- *
      *                              *    tita' per l'assieme          *
      *                              *---------------------------------*
           perform   imp-scl-dtp-810      thru imp-scl-dtp-819        .
      *                              *---------------------------------*
      *                              * Quantita' di utilizzo, molti-   *
      *                              * plicatore                       *
      *                              *---------------------------------*
           move      rf-lgr-qta-ipm       to   w-imp-scl-dtp-wcm
                                              (w-imp-scl-dtp-wlp)     .
           if        w-imp-scl-dtp-wcm
                    (w-imp-scl-dtp-wlp)   =    zero
                     move  1,000          to   w-imp-scl-dtp-wcm
                                              (w-imp-scl-dtp-wlp)     .
      *                              *---------------------------------*
      *                              * Quantita' di utilizzo, divisore *
      *                              *---------------------------------*
           move      rf-lgr-qta-ipd       to   w-imp-scl-dtp-wcd
                                              (w-imp-scl-dtp-wlp)     .
           if        w-imp-scl-dtp-wcd
                    (w-imp-scl-dtp-wlp)   =    zero
                     move  1,000          to   w-imp-scl-dtp-wcd
                                              (w-imp-scl-dtp-wlp)     .
       imp-scl-dtp-190.
      *                              *---------------------------------*
      *                              * Preparazione valori per il li-  *
      *                              * vello di profondita' interno    *
      *                              * attuale aumentato di 1          *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Preparazione in un comodo   *
      *                                  * del livello di profondita'  *
      *                                  * interno attuale aumentato   *
      *                                  * di 1                        *
      *                                  *-----------------------------*
           move      w-imp-scl-dtp-wlp    to   w-imp-scl-dtp-wls      .
           add       1                    to   w-imp-scl-dtp-wls      .
      *                                  *-----------------------------*
      *                                  * Tipo di componente          *
      *                                  *-----------------------------*
           move      rf-lgr-tpm-ass       to   w-imp-scl-dtp-wtc
                                              (w-imp-scl-dtp-wls)     .
      *                                  *-----------------------------*
      *                                  * Codice numerico del compo-  *
      *                                  * nente                       *
      *                                  *-----------------------------*
           move      rf-lgr-nrm-ass       to   w-imp-scl-dtp-wnc
                                              (w-imp-scl-dtp-wls)     .
      *                                  *-----------------------------*
      *                                  * Tipo di assieme             *
      *                                  *-----------------------------*
           move      zero                 to   w-imp-scl-dtp-wta
                                              (w-imp-scl-dtp-wls)     .
      *                                  *-----------------------------*
      *                                  * Codice numerico dell'assie- *
      *                                  * me                          *
      *                                  *-----------------------------*
           move      zero                 to   w-imp-scl-dtp-wna
                                              (w-imp-scl-dtp-wls)     .
      *                                  *-----------------------------*
      *                                  * Numero progressivo di riga  *
      *                                  * distinta                    *
      *                                  *-----------------------------*
           move      zero                to   w-imp-scl-dtp-wrg
                                             (w-imp-scl-dtp-wls)      .
       imp-scl-dtp-195.
      *                              *---------------------------------*
      *                              * Preparazione valori in uscita,  *
      *                              * escluso lo status, relativi     *
      *                              * all'elemento                    *
      *                              *---------------------------------*
           perform   imp-scl-dtp-820      thru imp-scl-dtp-829        .
       imp-scl-dtp-200.
      *                              *---------------------------------*
      *                              * Preparazione della prossima o-  *
      *                              * perazione da eseguire, a se-    *
      *                              * conda del tipo di elemento      *
      *                              *                                 *
      *                              *   - Prodotto finito       : 'N' *
      *                              *                                 *
      *                              *   - Semilavorato          : 'U' *
      *                              *                                 *
      *                              *   - Materia prima         : 'U' *
      *                              *                                 *
      *                              *   - Subdistinta virtuale  : 'U' *
      *                              *                                 *
      *                              *   - Tipo non riconosciuto : 'N' *
      *                              *                                 *
      *                              *---------------------------------*
           if        w-imp-scl-dtp-wta
                    (w-imp-scl-dtp-wlp)   =    02 or
                     w-imp-scl-dtp-wta
                    (w-imp-scl-dtp-wlp)   =    03 or
                     w-imp-scl-dtp-wta
                    (w-imp-scl-dtp-wlp)   =    99
                     move  "U"            to   w-imp-scl-dtp-wpo
           else      move  "N"            to   w-imp-scl-dtp-wpo      .
       imp-scl-dtp-205.
      *                              *---------------------------------*
      *                              * Status di uscita a : Ok         *
      *                              *---------------------------------*
           move      spaces               to   w-imp-scl-dtp-sts      .
       imp-scl-dtp-210.
      *                              *---------------------------------*
      *                              * Uscita                          *
      *                              *---------------------------------*
           go to     imp-scl-dtp-999.
       imp-scl-dtp-250.
      *              *-------------------------------------------------*
      *              * Funzione 'E-SKP'                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se il segnale di prossima operazione da e-  *
      *                  * seguire e' pari a 'U' lo si porta a 'N'     *
      *                  *---------------------------------------------*
           if        w-imp-scl-dtp-wpo    =    "U"
                     move  "N"            to   w-imp-scl-dtp-wpo      .
      *                  *---------------------------------------------*
      *                  * Dopodiche' si esegue la funzione 'E-GET'    *
      *                  *---------------------------------------------*
           go to     imp-scl-dtp-100.
       imp-scl-dtp-800.
      *              *-------------------------------------------------*
      *              * Subroutine per la lettura dell'anagrafica rela- *
      *              * tiva al componente di tipo e codice numerico di *
      *              * cui al livello di profondita' interna definito  *
      *              * in : w-imp-scl-dtp-wlp                          *
      *              *-------------------------------------------------*
       imp-scl-dtp-801.
      *                  *---------------------------------------------*
      *                  * Se il tipo ed il codice numerico del compo- *
      *                  * nente da leggere sono pari agli ultimi let- *
      *                  * ti, si prepara l'ultima unita' di misura e  *
      *                  * l'ultimo numero decimali per la quantita' e *
      *                  * si esce                                     *
      *                  *---------------------------------------------*
           if        w-imp-scl-dtp-wtc
                    (w-imp-scl-dtp-wlp)   =    w-imp-scl-dtp-utc and
                     w-imp-scl-dtp-wnc
                    (w-imp-scl-dtp-wlp)   =    w-imp-scl-dtp-ucc
                     move  w-imp-scl-dtp-uuc
                                          to   w-imp-scl-dtp-wuc
                                              (w-imp-scl-dtp-wlp)
                     move  w-imp-scl-dtp-udc
                                          to   w-imp-scl-dtp-wdc
                                              (w-imp-scl-dtp-wlp)
                     go to imp-scl-dtp-809.
      *                  *---------------------------------------------*
      *                  * Memorizzazione tipo e codice numerico ulti- *
      *                  * mi letti                                    *
      *                  *---------------------------------------------*
           move      w-imp-scl-dtp-wtc
                    (w-imp-scl-dtp-wlp)   to   w-imp-scl-dtp-utc      .
           move      w-imp-scl-dtp-wnc
                    (w-imp-scl-dtp-wlp)   to   w-imp-scl-dtp-ucc      .
      *                  *---------------------------------------------*
      *                  * Test se tipo componente a zero o codice     *
      *                  * numerico del componente a zero              *
      *                  *---------------------------------------------*
           if        w-imp-scl-dtp-wtc
                    (w-imp-scl-dtp-wlp)   =    zero or
                     w-imp-scl-dtp-wnc
                    (w-imp-scl-dtp-wlp)   =    zero
                     go to imp-scl-dtp-807.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo elemento      *
      *                  *---------------------------------------------*
           if        w-imp-scl-dtp-wtc
                    (w-imp-scl-dtp-wlp)   =    01
                     go to imp-scl-dtp-802
           else if   w-imp-scl-dtp-wtc
                    (w-imp-scl-dtp-wlp)   =    02
                     go to imp-scl-dtp-803
           else if   w-imp-scl-dtp-wtc
                    (w-imp-scl-dtp-wlp)   =    03
                     go to imp-scl-dtp-804
           else if   w-imp-scl-dtp-wtc
                    (w-imp-scl-dtp-wlp)   =    99
                     go to imp-scl-dtp-805
           else      go to imp-scl-dtp-806.
       imp-scl-dtp-802.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento 01 : Prodotto di vendita   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica [dcp]                *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO"             to   f-key                  .
           move      w-imp-scl-dtp-wnc
                    (w-imp-scl-dtp-wlp)   to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                      *-----------------------------------------*
      *                      * Se anagrafica non esistente             *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move  zero           to   w-imp-scl-dtp-wdc
                                              (w-imp-scl-dtp-wlp)
                     move  spaces         to   w-imp-scl-dtp-wuc
                                              (w-imp-scl-dtp-wlp)
                     go to imp-scl-dtp-808.
      *                      *-----------------------------------------*
      *                      * Se anagrafica esistente                 *
      *                      *-----------------------------------------*
           move      rf-dcp-dec-qta       to   w-imp-scl-dtp-wdc
                                              (w-imp-scl-dtp-wlp)     .
           move      rf-dcp-umi-ven       to   w-imp-scl-dtp-wuc
                                              (w-imp-scl-dtp-wlp)     .
           go to     imp-scl-dtp-808.
       imp-scl-dtp-803.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento 02 : Semilavorato          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica [dps]                *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMSEM"             to   f-key                  .
           move      w-imp-scl-dtp-wnc
                    (w-imp-scl-dtp-wlp)   to   rf-dps-num-sem         .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
      *                      *-----------------------------------------*
      *                      * Se anagrafica non esistente             *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move  zero           to   w-imp-scl-dtp-wdc
                                              (w-imp-scl-dtp-wlp)
                     move  spaces         to   w-imp-scl-dtp-wuc
                                              (w-imp-scl-dtp-wlp)
                     go to imp-scl-dtp-808.
      *                      *-----------------------------------------*
      *                      * Se anagrafica esistente                 *
      *                      *-----------------------------------------*
           move      rf-dps-dec-qta       to   w-imp-scl-dtp-wdc
                                              (w-imp-scl-dtp-wlp)     .
           move      rf-dps-umi-prd       to   w-imp-scl-dtp-wuc
                                              (w-imp-scl-dtp-wlp)     .
           go to     imp-scl-dtp-808.
       imp-scl-dtp-804.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento 03 : Materia prima         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica [dpm]                *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMMAP"             to   f-key                  .
           move      w-imp-scl-dtp-wnc
                    (w-imp-scl-dtp-wlp)   to   rf-dpm-num-map         .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
      *                      *-----------------------------------------*
      *                      * Se anagrafica non esistente             *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move  zero           to   w-imp-scl-dtp-wdc
                                              (w-imp-scl-dtp-wlp)
                     move  spaces         to   w-imp-scl-dtp-wuc
                                              (w-imp-scl-dtp-wlp)
                     go to imp-scl-dtp-808.
      *                      *-----------------------------------------*
      *                      * Se anagrafica esistente                 *
      *                      *-----------------------------------------*
           move      rf-dpm-dec-qta       to   w-imp-scl-dtp-wdc
                                              (w-imp-scl-dtp-wlp)     .
           move      rf-dpm-umi-prd       to   w-imp-scl-dtp-wuc
                                              (w-imp-scl-dtp-wlp)     .
           go to     imp-scl-dtp-808.
       imp-scl-dtp-805.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento 99 : Subdistinta virtuale  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica [lgv]                *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMLGV"             to   f-key                  .
           move      w-imp-scl-dtp-wnc
                    (w-imp-scl-dtp-wlp)   to   rf-lgv-num-lgv         .
           move      "pgm/dtp/fls/ioc/obj/ioflgv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgv                 .
      *                      *-----------------------------------------*
      *                      * Se anagrafica non esistente             *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move  zero           to   w-imp-scl-dtp-wdc
                                              (w-imp-scl-dtp-wlp)
                     move  spaces         to   w-imp-scl-dtp-wuc
                                              (w-imp-scl-dtp-wlp)
                     go to imp-scl-dtp-808.
      *                      *-----------------------------------------*
      *                      * Se anagrafica esistente                 *
      *                      *-----------------------------------------*
           move      rf-lgv-dec-qta       to   w-imp-scl-dtp-wdc
                                              (w-imp-scl-dtp-wlp)     .
           move      rf-lgv-umi-prd       to   w-imp-scl-dtp-wuc
                                              (w-imp-scl-dtp-wlp)     .
           go to     imp-scl-dtp-808.
       imp-scl-dtp-806.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento di tipo non riconosciuto   *
      *                  *---------------------------------------------*
           move      zero                 to   w-imp-scl-dtp-wdc
                                              (w-imp-scl-dtp-wlp)     .
           move      spaces               to   w-imp-scl-dtp-wuc
                                              (w-imp-scl-dtp-wlp)     .
           go to     imp-scl-dtp-808.
       imp-scl-dtp-807.
      *                  *---------------------------------------------*
      *                  * Se tipo assieme a zero o codice numerico    *
      *                  * assieme a zero                              *
      *                  *---------------------------------------------*
           move      zero                 to   w-imp-scl-dtp-wdc
                                              (w-imp-scl-dtp-wlp)     .
           move      spaces               to   w-imp-scl-dtp-wuc
                                              (w-imp-scl-dtp-wlp)     .
           go to     imp-scl-dtp-808.
       imp-scl-dtp-808.
      *                  *---------------------------------------------*
      *                  * Memorizzazione unita' di misura e numero    *
      *                  * decimali ultimi letti                       *
      *                  *---------------------------------------------*
           move      w-imp-scl-dtp-wuc
                    (w-imp-scl-dtp-wlp)   to   w-imp-scl-dtp-uuc      .
           move      w-imp-scl-dtp-wdc
                    (w-imp-scl-dtp-wlp)   to   w-imp-scl-dtp-udc      .
       imp-scl-dtp-809.
           exit.
       imp-scl-dtp-810.
      *              *-------------------------------------------------*
      *              * Subroutine per la lettura dell'anagrafica rela- *
      *              * tiva all'assieme di tipo e codice numerico di   *
      *              * cui al livello di profondita' interna definito  *
      *              * in : w-imp-scl-dtp-wlp                          *
      *              *-------------------------------------------------*
       imp-scl-dtp-811.
      *                  *---------------------------------------------*
      *                  * Test se tipo assieme a zero o codice nume-  *
      *                  * rico dell'assieme a zero                    *
      *                  *---------------------------------------------*
           if        w-imp-scl-dtp-wta
                    (w-imp-scl-dtp-wlp)   =    zero or
                     w-imp-scl-dtp-wna
                    (w-imp-scl-dtp-wlp)   =    zero
                     go to imp-scl-dtp-818.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo elemento      *
      *                  *---------------------------------------------*
           if        w-imp-scl-dtp-wta
                    (w-imp-scl-dtp-wlp)   =    01
                     go to imp-scl-dtp-812
           else if   w-imp-scl-dtp-wta
                    (w-imp-scl-dtp-wlp)   =    02
                     go to imp-scl-dtp-813
           else if   w-imp-scl-dtp-wta
                    (w-imp-scl-dtp-wlp)   =    03
                     go to imp-scl-dtp-814
           else if   w-imp-scl-dtp-wta
                    (w-imp-scl-dtp-wlp)   =    99
                     go to imp-scl-dtp-815
           else      go to imp-scl-dtp-817.
       imp-scl-dtp-812.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento 01 : Prodotto di vendita   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica [dcp]                *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO"             to   f-key                  .
           move      w-imp-scl-dtp-wna
                    (w-imp-scl-dtp-wlp)   to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                      *-----------------------------------------*
      *                      * Se anagrafica non esistente             *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-imp-scl-dtp-wes
                                              (w-imp-scl-dtp-wlp)
                     move  all "."        to   w-imp-scl-dtp-wde
                                              (w-imp-scl-dtp-wlp)
                     move  all "."        to   w-imp-scl-dtp-wum
                                              (w-imp-scl-dtp-wlp)
                     move  zero           to   w-imp-scl-dtp-wnd
                                              (w-imp-scl-dtp-wlp)
                     go to imp-scl-dtp-819.
      *                      *-----------------------------------------*
      *                      * Se anagrafica esistente                 *
      *                      *-----------------------------------------*
           move      spaces               to   w-imp-scl-dtp-wes
                                              (w-imp-scl-dtp-wlp)     .
           move      rf-dcp-des-pro       to   w-imp-scl-dtp-wde
                                              (w-imp-scl-dtp-wlp)     .
           move      rf-dcp-umi-ven       to   w-imp-scl-dtp-wum
                                              (w-imp-scl-dtp-wlp)     .
           move      rf-dcp-dec-qta       to   w-imp-scl-dtp-wnd
                                              (w-imp-scl-dtp-wlp)     .
           go to     imp-scl-dtp-819.
       imp-scl-dtp-813.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento 02 : Semilavorato          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica [dps]                *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMSEM"             to   f-key                  .
           move      w-imp-scl-dtp-wna
                    (w-imp-scl-dtp-wlp)   to   rf-dps-num-sem         .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
      *                      *-----------------------------------------*
      *                      * Se anagrafica non esistente             *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-imp-scl-dtp-wes
                                              (w-imp-scl-dtp-wlp)
                     move  all "."        to   w-imp-scl-dtp-wde
                                              (w-imp-scl-dtp-wlp)
                     move  all "."        to   w-imp-scl-dtp-wum
                                              (w-imp-scl-dtp-wlp)
                     move  zero           to   w-imp-scl-dtp-wnd
                                              (w-imp-scl-dtp-wlp)
                     go to imp-scl-dtp-819.
      *                      *-----------------------------------------*
      *                      * Se anagrafica esistente                 *
      *                      *-----------------------------------------*
           move      spaces               to   w-imp-scl-dtp-wes
                                              (w-imp-scl-dtp-wlp)     .
           move      rf-dps-des-sem       to   w-imp-scl-dtp-wde
                                              (w-imp-scl-dtp-wlp)     .
           move      rf-dps-umi-prd       to   w-imp-scl-dtp-wum
                                              (w-imp-scl-dtp-wlp)     .
           move      rf-dps-dec-qta       to   w-imp-scl-dtp-wnd
                                              (w-imp-scl-dtp-wlp)     .
           go to     imp-scl-dtp-819.
       imp-scl-dtp-814.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento 03 : Materia prima         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica [dpm]                *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMMAP"             to   f-key                  .
           move      w-imp-scl-dtp-wna
                    (w-imp-scl-dtp-wlp)   to   rf-dpm-num-map         .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
      *                      *-----------------------------------------*
      *                      * Se anagrafica non esistente             *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-imp-scl-dtp-wes
                                              (w-imp-scl-dtp-wlp)
                     move  all "."        to   w-imp-scl-dtp-wde
                                              (w-imp-scl-dtp-wlp)
                     move  all "."        to   w-imp-scl-dtp-wum
                                              (w-imp-scl-dtp-wlp)
                     move  zero           to   w-imp-scl-dtp-wnd
                                              (w-imp-scl-dtp-wlp)
                     go to imp-scl-dtp-819.
      *                      *-----------------------------------------*
      *                      * Se anagrafica esistente                 *
      *                      *-----------------------------------------*
           move      spaces               to   w-imp-scl-dtp-wes
                                              (w-imp-scl-dtp-wlp)     .
           move      rf-dpm-des-map       to   w-imp-scl-dtp-wde
                                              (w-imp-scl-dtp-wlp)     .
           move      rf-dpm-umi-prd       to   w-imp-scl-dtp-wum
                                              (w-imp-scl-dtp-wlp)     .
           move      rf-dpm-dec-qta       to   w-imp-scl-dtp-wnd
                                              (w-imp-scl-dtp-wlp)     .
           go to     imp-scl-dtp-819.
       imp-scl-dtp-815.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento 99 : Subdistinta virtuale  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica [lgv]                *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMLGV"             to   f-key                  .
           move      w-imp-scl-dtp-wna
                    (w-imp-scl-dtp-wlp)   to   rf-lgv-num-lgv         .
           move      "pgm/dtp/fls/ioc/obj/ioflgv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgv                 .
      *                      *-----------------------------------------*
      *                      * Se anagrafica non esistente             *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-imp-scl-dtp-wes
                                              (w-imp-scl-dtp-wlp)
                     move  all "."        to   w-imp-scl-dtp-wde
                                              (w-imp-scl-dtp-wlp)
                     move  all "."        to   w-imp-scl-dtp-wum
                                              (w-imp-scl-dtp-wlp)
                     move  zero           to   w-imp-scl-dtp-wnd
                                              (w-imp-scl-dtp-wlp)
                     go to imp-scl-dtp-819.
      *                      *-----------------------------------------*
      *                      * Se anagrafica esistente                 *
      *                      *-----------------------------------------*
           move      spaces               to   w-imp-scl-dtp-wes
                                              (w-imp-scl-dtp-wlp)     .
           move      rf-lgv-des-lgv       to   w-imp-scl-dtp-wde
                                              (w-imp-scl-dtp-wlp)     .
           move      rf-lgv-umi-prd       to   w-imp-scl-dtp-wum
                                              (w-imp-scl-dtp-wlp)     .
           move      rf-lgv-dec-qta       to   w-imp-scl-dtp-wnd
                                              (w-imp-scl-dtp-wlp)     .
           go to     imp-scl-dtp-819.
       imp-scl-dtp-817.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento di tipo non riconosciuto   *
      *                  *---------------------------------------------*
           move      "#"                  to   w-imp-scl-dtp-wes
                                              (w-imp-scl-dtp-wlp)     .
           move      all  "."             to   w-imp-scl-dtp-wde
                                              (w-imp-scl-dtp-wlp)     .
           move      all  "."             to   w-imp-scl-dtp-wum
                                              (w-imp-scl-dtp-wlp)     .
           move      zero                 to   w-imp-scl-dtp-wnd
                                              (w-imp-scl-dtp-wlp)     .
           go to     imp-scl-dtp-819.
       imp-scl-dtp-818.
      *                  *---------------------------------------------*
      *                  * Se tipo assieme a zero o codice numerico    *
      *                  * assieme a zero                              *
      *                  *---------------------------------------------*
           move      spaces               to   w-imp-scl-dtp-wes
                                              (w-imp-scl-dtp-wlp)     .
           move      spaces               to   w-imp-scl-dtp-wde
                                              (w-imp-scl-dtp-wlp)     .
           move      spaces               to   w-imp-scl-dtp-wum
                                              (w-imp-scl-dtp-wlp)     .
           move      zero                 to   w-imp-scl-dtp-wnd
                                              (w-imp-scl-dtp-wlp)     .
           go to     imp-scl-dtp-819.
       imp-scl-dtp-819.
           exit.
       imp-scl-dtp-820.
      *              *-------------------------------------------------*
      *              * Subroutine per la preparazione dei valori in u- *
      *              * scita relativi all'assieme di cui al livello    *
      *              * di profondita' di : w-imp-scl-dtp-wlp.          *
      *              * Ad esclusione dello status di uscita            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo codice magazzino del componente        *
      *                  *---------------------------------------------*
           move      w-imp-scl-dtp-wtc
                    (w-imp-scl-dtp-wlp)   to   w-imp-scl-dtp-tco      .
      *                  *---------------------------------------------*
      *                  * Codice numerico magazzino del componente    *
      *                  *---------------------------------------------*
           move      w-imp-scl-dtp-wnc
                    (w-imp-scl-dtp-wlp)   to   w-imp-scl-dtp-nco      .
      *                  *---------------------------------------------*
      *                  * Tipo codice magazzino dell'assieme          *
      *                  *---------------------------------------------*
           move      w-imp-scl-dtp-wta
                    (w-imp-scl-dtp-wlp)   to   w-imp-scl-dtp-tas      .
      *                  *---------------------------------------------*
      *                  * Codice numerico magazzino dell'assieme      *
      *                  *---------------------------------------------*
           move      w-imp-scl-dtp-wna
                    (w-imp-scl-dtp-wlp)   to   w-imp-scl-dtp-nas      .
      *                  *---------------------------------------------*
      *                  * Livello di profondita' esterno, pari al li- *
      *                  * vello di profondita' interno                *
      *                  *---------------------------------------------*
           move      w-imp-scl-dtp-wlp    to   w-imp-scl-dtp-liv      .
      *                  *---------------------------------------------*
      *                  * Segnale di anagrafica esistente per l'as-   *
      *                  * sieme                                       *
      *                  *---------------------------------------------*
           move      w-imp-scl-dtp-wes
                    (w-imp-scl-dtp-wlp)   to   w-imp-scl-dtp-ana      .
      *                  *---------------------------------------------*
      *                  * Descrizione da anagrafica assieme           *
      *                  *---------------------------------------------*
           move      w-imp-scl-dtp-wde
                    (w-imp-scl-dtp-wlp)   to   w-imp-scl-dtp-des      .
      *                  *---------------------------------------------*
      *                  * Unita' di misura da anagrafica assieme      *
      *                  *---------------------------------------------*
           move      w-imp-scl-dtp-wum
                    (w-imp-scl-dtp-wlp)   to   w-imp-scl-dtp-umi      .
      *                  *---------------------------------------------*
      *                  * Numero decimali quantita' da anagrafica as- *
      *                  * sieme                                       *
      *                  *---------------------------------------------*
           move      w-imp-scl-dtp-wnd
                    (w-imp-scl-dtp-wlp)   to   w-imp-scl-dtp-dec      .
      *                  *---------------------------------------------*
      *                  * Unita' di misura da anagrafica componente   *
      *                  *---------------------------------------------*
           move      w-imp-scl-dtp-wuc
                    (w-imp-scl-dtp-wlp)   to   w-imp-scl-dtp-cum      .
      *                  *---------------------------------------------*
      *                  * Numero decimali quantita' da anagrafica     *
      *                  * componente                                  *
      *                  *---------------------------------------------*
           move      w-imp-scl-dtp-wdc
                    (w-imp-scl-dtp-wlp)   to   w-imp-scl-dtp-cnd      .
      *                  *---------------------------------------------*
      *                  * Quantita' di utilizzo, moltiplicatore       *
      *                  *---------------------------------------------*
           move      w-imp-scl-dtp-wcm
                    (w-imp-scl-dtp-wlp)   to   w-imp-scl-dtp-cmu      .
      *                  *---------------------------------------------*
      *                  * Quantita' di utilizzo, divisore             *
      *                  *---------------------------------------------*
           move      w-imp-scl-dtp-wcd
                    (w-imp-scl-dtp-wlp)   to   w-imp-scl-dtp-cdu      .
      *                  *---------------------------------------------*
      *                  * Quantita' producibile relativa all'assieme  *
      *                  *---------------------------------------------*
           move      w-imp-scl-dtp-wqt
                    (w-imp-scl-dtp-wlp)   to   w-imp-scl-dtp-qta      .
           multiply  w-imp-scl-dtp-cdu    by   w-imp-scl-dtp-qta      .
           divide    w-imp-scl-dtp-cmu    into w-imp-scl-dtp-qta      .
           if        w-imp-scl-dtp-dec    =    zero
                     subtract 0,499       from w-imp-scl-dtp-qta
                     divide   1000        into w-imp-scl-dtp-qta
                                                         rounded
                     multiply 1000        by   w-imp-scl-dtp-qta
           else if   w-imp-scl-dtp-dec    =    1
                     subtract  0,049      from w-imp-scl-dtp-qta
                     divide    100        into w-imp-scl-dtp-qta
                                                         rounded
                     multiply  100        by   w-imp-scl-dtp-qta
           else if   w-imp-scl-dtp-dec    =    2
                     subtract   0,004     from w-imp-scl-dtp-qta
                     divide     10        into w-imp-scl-dtp-qta
                                                         rounded
                     multiply   10        by   w-imp-scl-dtp-qta      .
           move      w-imp-scl-dtp-wlp    to   w-imp-scl-dtp-wls      .
           add       1                    to   w-imp-scl-dtp-wls      .
           move      w-imp-scl-dtp-qta    to   w-imp-scl-dtp-wqt
                                              (w-imp-scl-dtp-wls)     .
       imp-scl-dtp-829.
           exit.
       imp-scl-dtp-999.
           exit.

      *    *===========================================================*
      *    * Caricamento del buffer, per un massimo di un elemento     *
      *    * diverso da quello da eliminare                            *
      *    *                                                           *
      *    * Nota : Il caricamento avviene per lettura della distinta  *
      *    *        base, ma escludendo il componente da eliminare     *
      *    *-----------------------------------------------------------*
       buf-per-cns-000.
      *              *-------------------------------------------------*
      *              * Preparazioni iniziali                           *
      *              *-------------------------------------------------*
       buf-per-cns-010.
      *                  *---------------------------------------------*
      *                  * Tipo magazzino dell'assieme                 *
      *                  *---------------------------------------------*
           move      w-rig-lgr-tpm-ass    to   w-ctl-nsb-tpm-ass      .
      *                  *---------------------------------------------*
      *                  * Codice numerico magazzino dell'assieme      *
      *                  *---------------------------------------------*
           move      w-rig-lgr-nrm-ass    to   w-ctl-nsb-nrm-ass      .
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico magazzino dell'assieme  *
      *                  *---------------------------------------------*
           move      w-rig-ass-afm-ass    to   w-ctl-nsb-afm-ass      .
       buf-per-cns-020.
      *                  *---------------------------------------------*
      *                  * Numero componenti caricati a zero           *
      *                  *---------------------------------------------*
           move      zero                 to   w-ctl-nsb-num-cpt      .
       buf-per-cns-100.
      *              *-------------------------------------------------*
      *              * Start su [lgr]                                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "TNMASS"             to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-ctl-nsb-tpm-ass    to   rf-lgr-tpm-ass         .
           move      w-ctl-nsb-nrm-ass    to   rf-lgr-nrm-ass         .
           move      zero                 to   rf-lgr-num-prg         .
           move      "pgm/dtp/fls/ioc/obj/ioflgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgr                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : fine caricamento              *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go buf-per-cns-900.
       buf-per-cns-200.
      *              *-------------------------------------------------*
      *              * Read Next da [lgr]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgr                 .
      *              *-------------------------------------------------*
      *              * Se At End : fine caricamento                    *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go buf-per-cns-900.
       buf-per-cns-300.
      *              *-------------------------------------------------*
      *              * Test Max su [lgr], se non superato : fine cari- *
      *              * camento                                         *
      *              *-------------------------------------------------*
           if        rf-lgr-tpm-ass       not  = w-ctl-nsb-tpm-ass or
                     rf-lgr-nrm-ass       not  = w-ctl-nsb-nrm-ass
                     go buf-per-cns-900.
       buf-per-cns-400.
      *              *-------------------------------------------------*
      *              * Se il componente letto corrisponde a quello da  *
      *              * eliminare, lo si ignora e si continua per leg-  *
      *              * gere il componente successico                   *
      *              *-------------------------------------------------*
           if        rf-lgr-tpm-cpt       =    01
                     if    w-tes-cde-tip  =    "P"       and
                           w-tes-cde-num  =    rf-lgr-nrm-cpt
                           go to buf-per-cns-800
                     else  go to buf-per-cns-500.
           if        rf-lgr-tpm-cpt       =    02
                     if    w-tes-cde-tip  =    "S"       and
                           w-tes-cde-num  =    rf-lgr-nrm-cpt
                           go to buf-per-cns-800
                     else  go to buf-per-cns-500.
           if        rf-lgr-tpm-cpt       =    03
                     if    w-tes-cde-tip  =    "M"       and
                           w-tes-cde-num  =    rf-lgr-nrm-cpt
                           go to buf-per-cns-800
                     else  go to buf-per-cns-500.
           if        rf-lgr-tpm-cpt       =    99
                     if    w-tes-cde-tip  =    "D"       and
                           w-tes-cde-num  =    rf-lgr-nrm-cpt
                           go to buf-per-cns-800
                     else  go to buf-per-cns-500.
           go to     buf-per-cns-500.
       buf-per-cns-500.
      *              *-------------------------------------------------*
      *              * Incremento numero elementi presenti nel buffer  *
      *              *-------------------------------------------------*
           add       1                    to   w-ctl-nsb-num-cpt      .
       buf-per-cns-600.
      *              *-------------------------------------------------*
      *              * Bufferizzazione componente letto                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero progressivo                          *
      *                  *---------------------------------------------*
           move      rf-lgr-num-prg       to   w-ctl-nsb-prg-cpt
                                              (w-ctl-nsb-num-cpt)     .
      *                  *---------------------------------------------*
      *                  * Tipo componente                             *
      *                  *---------------------------------------------*
           move      rf-lgr-tpm-cpt       to   w-ctl-nsb-tpm-cpt
                                              (w-ctl-nsb-num-cpt)     .
      *                  *---------------------------------------------*
      *                  * Codice numerico componente                  *
      *                  *---------------------------------------------*
           move      rf-lgr-nrm-cpt       to   w-ctl-nsb-nrm-cpt
                                              (w-ctl-nsb-num-cpt)     .
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico componente              *
      *                  *---------------------------------------------*
           move      rf-lgr-afm-cpt       to   w-ctl-nsb-afm-cpt
                                              (w-ctl-nsb-num-cpt)     .
       buf-per-cns-800.
      *              *-------------------------------------------------*
      *              * Se e' stato raggiunto il massimo numero di ele- *
      *              * menti caricabili nel buffer si esce, altrimenti *
      *              * si ricicla per il componente successivo         *
      *              *-------------------------------------------------*
           if        w-ctl-nsb-num-cpt    not  < w-ctl-nsb-max-ele
                     go to buf-per-cns-900
           else      go to buf-per-cns-200.
       buf-per-cns-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     buf-per-cns-999.
       buf-per-cns-999.
           exit.

