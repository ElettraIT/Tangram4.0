       Identification Division.
       Program-Id.                                 pgep300u           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    gep                 *
      *                                Settore:    mov                 *
      *                                   Fase:    gep300              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 06/06/92    *
      *                       Ultima revisione:    NdK del 14/11/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Movimenti per gestione portafoglio          *
      *                                                                *
      *                    Sottoprogramma per esecuzione funzione di : *
      *                                                                *
      *                    Presunto buon esito per Gruppi di Scadenze  *
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
      *            * Per routine pre-exe-pgm-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-pre-exe-pgm      pic  x(01)                  .
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
      *        * [sdb]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfsdb"                          .
      *        *-------------------------------------------------------*
      *        * [ddp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfddp"                          .
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
      *            * Data di registrazione                             *
      *            *---------------------------------------------------*
               10  w-tes-dat-reg          pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Tipo operazione                                   *
      *            *---------------------------------------------------*
               10  w-tes-tip-ope          pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Descrizione tipo operazione                       *
      *            *---------------------------------------------------*
               10  w-tes-tip-ope-des      pic  x(50)                  .
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
      *            *---------------------------------------------------*
      *            * Flag per impostazione riga vuota                  *
      *            *---------------------------------------------------*
               10  w-tes-flg-irv          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Data scadenza massima da selezionare              *
      *            *---------------------------------------------------*
               10  w-tes-dsc-max          pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Importo totale scadenze selezionate               *
      *            *---------------------------------------------------*
               10  w-tes-imp-tot          pic s9(13)       sign is
                                                           trailing
                                                           separate
                                                           character  .

      *    *===========================================================*
      *    * Work-area per bufferizzazione riga corpo                  *
      *    *-----------------------------------------------------------*
       01  w-rig.
      *        *-------------------------------------------------------*
      *        * Area valori attuali e precedenti                      *
      *        *-------------------------------------------------------*
           05  w-rig-val-aep     occurs 2.
      *            *---------------------------------------------------*
      *            * Numero progressivo                                *
      *            *---------------------------------------------------*
               10  w-rig-num-prg          pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Numero scadenza                                   *
      *            *---------------------------------------------------*
               10  w-rig-num-sdb          pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Tipo scadenza                                     *
      *            *---------------------------------------------------*
               10  w-rig-tip-sdb          pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Data scadenza                                     *
      *            *---------------------------------------------------*
               10  w-rig-dts-sdb          pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Tipo debitore                                     *
      *            *---------------------------------------------------*
               10  w-rig-tip-dbt          pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Codice debitore                                   *
      *            *---------------------------------------------------*
               10  w-rig-cod-dbt          pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice debitore, sottoconto contabile             *
      *            *---------------------------------------------------*
               10  w-rig-cod-dbt-cge      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice debitore, ragione sociale                  *
      *            *---------------------------------------------------*
               10  w-rig-cod-dbt-rag      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Codice dipendenza del debitore                    *
      *            *---------------------------------------------------*
               10  w-rig-dpz-dbt          pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Codice dipendenza del debitore, ragione sociale   *
      *            *---------------------------------------------------*
               10  w-rig-dpz-dbt-rag      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Inoltro scadenza al debitore                      *
      *            *---------------------------------------------------*
               10  w-rig-inl-sdb          pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Importo scadenza                                  *
      *            *---------------------------------------------------*
               10  w-rig-imp-sdb          pic s9(11)       sign is
                                                           trailing
                                                           separate
                                                           character  .
      *            *---------------------------------------------------*
      *            * Data documento di riferimento                     *
      *            *---------------------------------------------------*
               10  w-rig-dat-ddr          pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Numero documento di riferimento                   *
      *            *---------------------------------------------------*
               10  w-rig-num-ddr          pic  x(10)                  .
      *            *---------------------------------------------------*
      *            * Indicatore di aggiornamento contabile eseguito    *
      *            * per la scadenza al momento della emissione        *
      *            * - S : Si, era stato prodotto un aggiornamento     *
      *            * - N : No, non era stato prodotto un aggiornamento *
      *            * - ? : Determinazione non possibile, errore        *
      *            *---------------------------------------------------*
               10  w-rig-agg-emi-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Codice contropartita contabile in caso di aggior- *
      *            * namento non eseguito in fase di emissione         *
      *            *---------------------------------------------------*
               10  w-rig-agg-emi-ctp      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Indicatore di aggiornamento contabile eseguito    *
      *            * per la scadenza al momento della presentazione    *
      *            * della distinta                                    *
      *            * - S : Si, era stato prodotto un aggiornamento     *
      *            * - N : No, non era stato prodotto un aggiornamento *
      *            * - ? : Determinazione non possibile, errore        *
      *            *---------------------------------------------------*
               10  w-rig-agg-pre-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Indicatore di aggiornamento contabile da eseguire *
      *            * sulla scadenza a fronte del Presunto buon esito   *
      *            * - S : Si, aggiornamento da eseguire               *
      *            * - N : No, aggiornamento da non eseguire           *
      *            *---------------------------------------------------*
               10  w-rig-snx-agg-cge      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Indicatore di errori sulla scadenza               *
      *            * - Spaces : nessun errore                          *
      *            * - #      : errori                                 *
      *            *---------------------------------------------------*
               10  w-rig-flg-err-sdb      pic  x(01)                  .

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
               10  filler occurs 1024     pic  x(01)                  .

      *    *===========================================================*
      *    * Work per linea corpo a video                              *
      *    *-----------------------------------------------------------*
       01  w-lin.
      *        *-------------------------------------------------------*
      *        * Numero righe di corpo effettive visibili contempora-  *
      *        * neamente in una pagina di corpo nell'area di scroll   *
      *        *-------------------------------------------------------*
           05  w-lin-num-lin-vis          pic  9(02)       value 07   .
      *        *-------------------------------------------------------*
      *        * Numero linee di display impegnate per ogni riga corpo *
      *        * nell'area di scroll                                   *
      *        *-------------------------------------------------------*
           05  w-lin-num-lin-prc          pic  9(02)       value 01   .
      *        *-------------------------------------------------------*
      *        * Prima linea di display utilizzata per le righe corpo  *
      *        * nell'area di scroll                                   *
      *        *-------------------------------------------------------*
           05  w-lin-pri-lin-vid          pic  9(02)       value 09   .
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
                   15  filler             pic  x(02)                  .
                   15  w-lin-imm-err-dac  pic  x(03)                  .
                   15  filler             pic  x(01)                  .
                   15  w-lin-imm-rag-dpz  pic  x(40)                  .
                   15  filler             pic  x(04)                  .
                   15  filler             pic  x(01)                  .
                   15  w-lin-imm-imp-sca  pic  x(15)                  .
                   15  filler             pic  x(03)                  .
                   15  w-lin-imm-dat-sca  pic  x(08)                  .

      *    *===========================================================*
      *    * Work-area per determinazioni varie                        *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per determinare l'indice dell'operazione di Pre- *
      *        * sunto buon esito sulla tabella tipi operazione        *
      *        *-------------------------------------------------------*
           05  w-det-inx-pbe.
      *            *---------------------------------------------------*
      *            * Indice su tabella tipi operazione                 *
      *            *---------------------------------------------------*
               10  w-det-inx-pbe-top      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per determinare se la scadenza trattata aveva    *
      *        * prodotto oppure no un aggiornamento contabile al      *
      *        * momento dell'emissione, e se no della contropartita   *
      *        * contabile per la presentazione associata al tipo o-   *
      *        * perazione di emissione                                *
      *        *-------------------------------------------------------*
           05  w-det-agg-emi.
      *            *---------------------------------------------------*
      *            * Tipo di scadenza                                  *
      *            *---------------------------------------------------*
               10  w-det-agg-emi-tsc      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Tipo di acquisizione della scadenza               *
      *            *---------------------------------------------------*
               10  w-det-agg-emi-tas      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Data registrazione movimento contabile di emis-   *
      *            * sione scadenza                                    *
      *            *---------------------------------------------------*
               10  w-det-agg-emi-drc      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Numero protocollo movimento contabile di emissio- *
      *            * ne scadenza                                       *
      *            *---------------------------------------------------*
               10  w-det-agg-emi-npc      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Risultato della determinazione                    *
      *            * - S : Si, era stato prodotto un aggiornamento     *
      *            * - N : No, non era stato prodotto un aggiornamento *
      *            * - ? : Determinazione non possibile, errore        *
      *            *---------------------------------------------------*
               10  w-det-agg-emi-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Codice contropartita contabile in caso di aggior- *
      *            * namento non eseguito in fase di emissione         *
      *            *---------------------------------------------------*
               10  w-det-agg-emi-ctp      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Work per tipo operazione di emissione             *
      *            *---------------------------------------------------*
               10  w-det-agg-emi-woe      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Work per indice su tabella tipi operazione        *
      *            *---------------------------------------------------*
               10  w-det-agg-emi-inx      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per determinare se la distinta trattata aveva    *
      *        * prodotto oppure no un aggiornamento contabile al      *
      *        * momento della presentazione                           *
      *        *-------------------------------------------------------*
           05  w-det-agg-pre.
      *            *---------------------------------------------------*
      *            * Tipo di presentazione                             *
      *            *---------------------------------------------------*
               10  w-det-agg-pre-tdp      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Risultato della determinazione                    *
      *            * - S : Si, era stato prodotto un aggiornamento     *
      *            * - N : No, non era stato prodotto un aggiornamento *
      *            * - ? : Determinazione non possibile, errore        *
      *            *---------------------------------------------------*
               10  w-det-agg-pre-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Work per tipo operazione di presentazione         *
      *            *---------------------------------------------------*
               10  w-det-agg-pre-wop      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Work per indice su tabella tipi operazione        *
      *            *---------------------------------------------------*
               10  w-det-agg-pre-inx      pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per aggiornamenti contabili da gestione porta-  *
      *    * foglio per il movimento di presunto buon esito su scaden- *
      *    * ze presentate al SBF o allo sconto                        *
      *    *-----------------------------------------------------------*
       01  w-cge-gep.
      *        *-------------------------------------------------------*
      *        * Si/No aggiornamento da eseguire per il tipo operazio- *
      *        * ne di presunto buon esito su scadenze presentate al   *
      *        * SBF o allo sconto                                     *
      *        *-------------------------------------------------------*
           05  w-cge-gep-pbe-snx          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Data di registrazione del movimento di contabilita'   *
      *        * generale da annullare                                 *
      *        *-------------------------------------------------------*
           05  w-cge-gep-drc-del          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Numero di protocollo del movimento di contabilita'    *
      *        * generale da annullare                                 *
      *        *-------------------------------------------------------*
           05  w-cge-gep-npc-del          pic  9(07)                  .

      *    *===========================================================*
      *    * Work-area per preparazione movimento di contabilita' ge-  *
      *    * nerale a fronte del movimento di presunto buon esito su   *
      *    * scadenze presentate al SBF o allo sconto                  *
      *    *-----------------------------------------------------------*
       01  w-mov-cge.
      *        *-------------------------------------------------------*
      *        * Causale contabile che deve essere utilizzata          *
      *        *-------------------------------------------------------*
           05  w-mov-cge-cau-cge          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Contatore numero di righe movimento contabile         *
      *        *-------------------------------------------------------*
           05  w-mov-cge-ctr-rig          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tabella righe movimento                               *
      *        *-------------------------------------------------------*
           05  w-mov-cge-tbl-rig.
      *            *---------------------------------------------------*
      *            * Elemento della tabella                            *
      *            *---------------------------------------------------*
               10  w-mov-cge-tbl-ele occurs 10.
      *                *-----------------------------------------------*
      *                * Tipo archivio per la riga                     *
      *                * - C : Cliente                                 *
      *                * - G : Generale                                *
      *                *-----------------------------------------------*
                   15  w-mov-cge-tip-arc  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Codice archivio per la riga                   *
      *                * - Solo se tipo archivio 'C'                   *
      *                *-----------------------------------------------*
                   15  w-mov-cge-cod-arc  pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Codice del sottoconto per la riga             *
      *                *-----------------------------------------------*
                   15  w-mov-cge-cod-stc  pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Importo per la riga                           *
      *                *-----------------------------------------------*
                   15  w-mov-cge-imp-rig  pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Work-area per l'accumulo delle righe                  *
      *        *-------------------------------------------------------*
           05  w-mov-cge-acc-rig.
      *            *---------------------------------------------------*
      *            * Tipo archivio                                     *
      *            *---------------------------------------------------*
               10  w-mov-cge-acc-tip      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Codice archivio                                   *
      *            *---------------------------------------------------*
               10  w-mov-cge-acc-arc      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice sottoconto                                 *
      *            *---------------------------------------------------*
               10  w-mov-cge-acc-stc      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Dare o Avere                                      *
      *            *---------------------------------------------------*
               10  w-mov-cge-acc-dav      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Importo                                           *
      *            *---------------------------------------------------*
               10  w-mov-cge-acc-imp      pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Work-area locale                                      *
      *        *-------------------------------------------------------*
           05  w-mov-cge-wrk-loc.
      *            *---------------------------------------------------*
      *            * Indici e contatori di comodo                      *
      *            *---------------------------------------------------*
               10  w-mov-cge-inx-001      pic  9(02)                  .
               10  w-mov-cge-inx-rig      pic  9(02)                  .

      *    *===========================================================*
      *    * Link-area per aggiornamento contabilita' generale, clien- *
      *    * ti, fornitori, iva                                        *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/pcge300z.pgl"                   .

      *    *===========================================================*
      *    * Work per subroutines di Espansione                        *
      *    *-----------------------------------------------------------*
       01  w-esp.
      *        *-------------------------------------------------------*
      *        * Work per Espansione su archivio [sdb]                 *
      *        *-------------------------------------------------------*
           05  w-esp-arc-sdb.
               10  w-esp-arc-sdb-sns      pic  x(01)                  .
               10  w-esp-arc-sdb-sel      pic  x(01)                  .
               10  w-esp-arc-sdb-num      pic  9(11)                  .
               
      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio debitori                     *
      *        *-------------------------------------------------------*
           05  w-let-arc-dbt.
               10  w-let-arc-dbt-flg      pic  x(01)                  .
               10  w-let-arc-dbt-tip      pic  9(02)                  .
               10  w-let-arc-dbt-cod      pic  9(07)                  .
               10  w-let-arc-dbt-rag      pic  x(40)                  .
               10  w-let-arc-dbt-via      pic  x(40)                  .
               10  w-let-arc-dbt-loc      pic  x(40)                  .
               10  w-let-arc-dbt-cge      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio dipendenze debitori          *
      *        *-------------------------------------------------------*
           05  w-let-dpz-dbt.
               10  w-let-dpz-dbt-flg      pic  x(01)                  .
               10  w-let-dpz-dbt-tip      pic  9(02)                  .
               10  w-let-dpz-dbt-cod      pic  9(07)                  .
               10  w-let-dpz-dbt-dpz      pic  x(04)                  .
               10  w-let-dpz-dbt-rag      pic  x(40)                  .
               10  w-let-dpz-dbt-via      pic  x(40)                  .
               10  w-let-dpz-dbt-loc      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [cli]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-cli.
               10  w-let-arc-cli-flg      pic  x(01)                  .
               10  w-let-arc-cli-cod      pic  9(07)                  .
               10  w-let-arc-cli-rag      pic  x(40)                  .
               10  w-let-arc-cli-via      pic  x(40)                  .
               10  w-let-arc-cli-loc      pic  x(40)                  .
               10  w-let-arc-cli-cge      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcc.
               10  w-let-arc-dcc-flg      pic  x(01)                  .
               10  w-let-arc-dcc-tle      pic  x(01)                  .
               10  w-let-arc-dcc-cod      pic  9(07)                  .
               10  w-let-arc-dcc-dpz      pic  x(04)                  .
               10  w-let-arc-dcc-rag      pic  x(40)                  .
               10  w-let-arc-dcc-via      pic  x(40)                  .
               10  w-let-arc-dcc-loc      pic  x(40)                  .
               10  w-let-arc-dcc-abi      pic  9(05)                  .
               10  w-let-arc-dcc-cab      pic  9(05)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Numero scadenza                                       *
      *        *-------------------------------------------------------*
           05  w-sav-num-sdb              pic  9(11)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo scadenza                              *
      *        *-------------------------------------------------------*
           05  w-exp-tip-sdb.
               10  w-exp-tip-sdb-num      pic  9(02)       value 11   .
               10  w-exp-tip-sdb-lun      pic  9(02)       value 20   .
               10  w-exp-tip-sdb-tbl.
                   15  filler             pic  x(20) value
                            "Tipo : RD           "                    .
                   15  filler             pic  x(20) value
                            "Tipo : IE           "                    .
                   15  filler             pic  x(20) value
                            "Tipo : RIBA         "                    .
                   15  filler             pic  x(20) value
                            "Tipo : CDO          "                    .
                   15  filler             pic  x(20) value
                            "Tipo : MAV          "                    .
                   15  filler             pic  x(20) value
                            "Tipo : RID          "                    .
                   15  filler             pic  x(20) value
                            "Tipo : BB           "                    .
                   15  filler             pic  x(20) value
                            "Tipo : CCP          "                    .
                   15  filler             pic  x(20) value
                            "Tipo : RB           "                    .
                   15  filler             pic  x(20) value
                            "Tipo : TR           "                    .
                   15  filler             pic  x(20) value
                            "Tipo : PC           "                    .

      *    *===========================================================*
      *    * Work per accettazioni particolari                         *
      *    *-----------------------------------------------------------*
       01  w-acc.
      *        *-------------------------------------------------------*
      *        * Work per : Completamento accettazione testata e con-  *
      *        *            seguente precaricamento delle righe tra-   *
      *        *            mite selezione                             *
      *        *-------------------------------------------------------*
           05  w-acc-tes-sel.
      *            *---------------------------------------------------*
      *            * Flag di completamento eseguito                    *
      *            * - Spaces : No                                     *
      *            * - #      : Si                                     *
      *            *---------------------------------------------------*
               10  w-acc-tes-sel-flg      pic  x(01)                  .

      *    *===========================================================*
      *    * Work per caricamento iniziale scadenze per mezzo della    *
      *    * selezione sui parametri impostati                         *
      *    *-----------------------------------------------------------*
       01  w-loa-sdb.
      *        *-------------------------------------------------------*
      *        * Parametri in input                                    *
      *        *-------------------------------------------------------*
           05  w-loa-sdb-inp.
      *            *---------------------------------------------------*
      *            * Data operazione                                   *
      *            *---------------------------------------------------*
               10  w-loa-sdb-inp-dop      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Data scadenza massima da selezionare              *
      *            *---------------------------------------------------*
               10  w-loa-sdb-inp-sma      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Numero scadenze massimo da selezionare            *
      *            *---------------------------------------------------*
               10  w-loa-sdb-inp-nsm      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Importo globale massimo da selezionare            *
      *            *---------------------------------------------------*
               10  w-loa-sdb-inp-igm      pic s9(13)       sign is
                                                           trailing
                                                           separate
                                                           character  .
      *        *-------------------------------------------------------*
      *        * Parametri in output                                   *
      *        *-------------------------------------------------------*
           05  w-loa-sdb-out.
      *            *---------------------------------------------------*
      *            * Flag fondamentale di uscita                       *
      *            * - 00 : Caricamento con selezione effettuato       *
      *            * - 01 : Caricamento con selezione non effettuato   *
      *            *---------------------------------------------------*
               10  w-loa-sdb-out-flg      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Numero di scadenze selezionate                    *
      *            *---------------------------------------------------*
               10  w-loa-sdb-out-nss      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Importo globale selezionato                       *
      *            *---------------------------------------------------*
               10  w-loa-sdb-out-igs      pic s9(13)       sign is
                                                           trailing
                                                           separate
                                                           character  .
      *        *-------------------------------------------------------*
      *        * Work-area locale                                      *
      *        *-------------------------------------------------------*
           05  w-loa-sdb-wrk.
      *            *---------------------------------------------------*
      *            * Per simulazione numero di scadenze selezionate    *
      *            *---------------------------------------------------*
               10  w-loa-sdb-wrk-nss      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Per simulazione importo globale selezionato       *
      *            *---------------------------------------------------*
               10  w-loa-sdb-wrk-igs      pic s9(13)       sign is
                                                           trailing
                                                           separate
                                                           character  .

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

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di Link per programmi della fase 'pgep3000'          *
      *    *-----------------------------------------------------------*
           copy      "pgm/gep/prg/cpy/pgep3000.pgl"                   .

      ******************************************************************
       Procedure Division                using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
           perform   pre-exe-pgm-000      thru pre-exe-pgm-999        .
           if        w-cnt-pre-exe-pgm    not  = spaces
                     go to main-999.
      *              *-------------------------------------------------*
      *              * Flag di visualizzazione titolo in On            *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-sts-vis-tit      .
      *              *-------------------------------------------------*
      *              * Accettazione campi chiave                       *
      *              *-------------------------------------------------*
           perform   acc-key-reg-000      thru acc-key-reg-999        .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo uscita            *
      *              *-------------------------------------------------*
           if        w-cnt-tus-acc-key    =    "E"
                     go to main-100
           else if   w-cnt-tus-acc-key    =    "U"
                     go to main-200
           else      go to main-300.
       main-100.
      *              *-------------------------------------------------*
      *              * Se tipo uscita 'E'                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad 'E'                       *
      *                  *---------------------------------------------*
           move      "E"                  to   w-key-tus-ack          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     main-900.
       main-200.
      *              *-------------------------------------------------*
      *              * Se tipo uscita 'U'                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad 'U'                       *
      *                  *---------------------------------------------*
           move      "U"                  to   w-key-tus-ack          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     main-900.
       main-300.
      *              *-------------------------------------------------*
      *              * Se tipo uscita a Spaces                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura registrazione pre-esistente         *
      *                  *---------------------------------------------*
           perform   rou-let-reg-000      thru rou-let-reg-999        .
      *                  *---------------------------------------------*
      *                  * Se esito negativo : uscita con Spaces       *
      *                  *---------------------------------------------*
           if        w-cnt-rou-let-reg    not  = spaces
                     move  spaces         to   w-key-tus-ack
                     go to main-900.
       main-400.
      *                  *---------------------------------------------*
      *                  * Routine pre-accettazione campi non chiave;  *
      *                  * se anomalie si esce con Spaces              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "I"
                     go to main-420
           else if   w-cnt-mfu-tip-fun    =    "M"
                     go to main-440
           else if   w-cnt-mfu-tip-fun    =    "V"
                     go to main-460
           else      move  spaces         to   w-key-tus-ack
                     go to main-900.
       main-420.
           move      spaces               to   w-cnt-pre-acc-ins      .
           perform   pre-acc-ins-000      thru pre-acc-ins-999        .
           if        w-cnt-pre-acc-ins    =    spaces
                     go to main-500
           else      move  spaces         to   w-key-tus-ack
                     go to main-900.
       main-440.
           move      spaces               to   w-cnt-pre-acc-mod      .
           perform   pre-acc-mod-000      thru pre-acc-mod-999        .
           if        w-cnt-pre-acc-mod    =    spaces
                     go to main-500
           else      move  spaces         to   w-key-tus-ack
                     go to main-900.
       main-460.
           move      spaces               to   w-cnt-pre-acc-vis      .
           perform   pre-acc-vis-000      thru pre-acc-vis-999        .
           if        w-cnt-pre-acc-vis    =    spaces
                     go to main-500
           else      move  spaces         to   w-key-tus-ack
                     go to main-900.
       main-500.
      *                  *---------------------------------------------*
      *                  * Normalizzazione flag di modifica di almeno  *
      *                  * un campo non chiave                         *
      *                  *---------------------------------------------*
           move      spaces               to   w-cnt-acc-flg-aum      .
      *                  *---------------------------------------------*
      *                  * Accettazione campi non chiave               *
      *                  *---------------------------------------------*
           perform   acc-nok-reg-000      thru acc-nok-reg-999        .
       main-600.
      *                  *---------------------------------------------*
      *                  * Se uscita per Exit si esegue la routine     *
      *                  * post-exit                                   *
      *                  *---------------------------------------------*
           if        w-cnt-tus-acc-nok    not  = "E"
                     go to main-700.
      *                      *-----------------------------------------*
      *                      * Se Inserimento                          *
      *                      *-----------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "I"
                     perform   pos-exi-ins-000
                                          thru pos-exi-ins-999
      *                      *-----------------------------------------*
      *                      * Se Modifica                             *
      *                      *-----------------------------------------*
           else if   w-cnt-mfu-tip-fun    =    "M"
                     perform   pos-exi-mod-000
                                          thru pos-exi-mod-999
      *                      *-----------------------------------------*
      *                      * Se Visualizzazione                      *
      *                      *-----------------------------------------*
           else if   w-cnt-mfu-tip-fun    =    "V"
                     perform   pos-exi-vis-000
                                          thru pos-exi-vis-999        .
      *                      *-----------------------------------------*
      *                      * Quindi si esce con Spaces               *
      *                      *-----------------------------------------*
           move      spaces               to   w-key-tus-ack          .
           go to     main-900.
       main-700.
      *                  *---------------------------------------------*
      *                  * Se uscita per annullamento                  *
      *                  *---------------------------------------------*
           if        w-cnt-tus-acc-nok    not  = "X"
                     go to main-800.
      *                      *-----------------------------------------*
      *                      * Routine post-conferma di annullamento   *
      *                      *-----------------------------------------*
           perform   pos-cnf-ann-000      thru pos-cnf-ann-999        .
      *                      *-----------------------------------------*
      *                      * Quindi si esce con Spaces               *
      *                      *-----------------------------------------*
           move      spaces               to   w-key-tus-ack          .
           go to     main-900.
       main-800.
      *                  *---------------------------------------------*
      *                  * Se uscita per conferma si esegue la routine *
      *                  * post-conferma                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se post-conferma di Inserimento         *
      *                      *-----------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "I"
                     perform   pos-cnf-ins-000
                                          thru pos-cnf-ins-999
      *                      *-----------------------------------------*
      *                      * Se post-conferma di Modifica            *
      *                      *-----------------------------------------*
           else if   w-cnt-mfu-tip-fun    =    "M"
                     perform   pos-cnf-mod-000
                                          thru pos-cnf-mod-999        .
      *                      *-----------------------------------------*
      *                      * Quindi si esce con Spaces               *
      *                      *-----------------------------------------*
           move      spaces               to   w-key-tus-ack          .
           go to     main-900.
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
      *              * Tasto di funzione Delt : sempre disabilitato    *
      *              *-------------------------------------------------*
           move      spaces               to   v-pfk (19)             .
       exe-acc-cmp-100.
      *              *-------------------------------------------------*
      *              * Tasto di funzione Do :                          *
      *              *  - se Visualizzazione     : disabilitato        *
      *              *  - altrimenti             : inalterato          *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "V"
                     move  spaces         to   v-pfk (05)             .
      *              *-------------------------------------------------*
      *              * Tasto di funzione Insr                          *
      *              *  - se primo campo riga corpo : disabilitato     *
      *              *  - altrimenti                : inalterato       *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    "C"
                     move  spaces         to   v-pfk (04)             .
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
      *              * Determinazione indice operazione di Presunto    *
      *              * buon esito su tabella tipi operazione           *
      *              *-------------------------------------------------*
           perform   det-inx-pbe-000      thru det-inx-pbe-999        .
           if        w-det-inx-pbe-top    =    zero
                     move  "#"            to   w-cnt-pre-exe-pgm
                     go to pre-exe-pgm-999.
      *              *-------------------------------------------------*
      *              * Open modulo aggiornamento contabilita' genera-  *
      *              * le, clienti, fornitori, iva                     *
      *              *-------------------------------------------------*
           perform   mdl-agg-cge-opn-000  thru mdl-agg-cge-opn-999    .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Determinazione indice operazione di Presunto buon esito   *
      *    * su tabella tipi operazione                                *
      *    *-----------------------------------------------------------*
       det-inx-pbe-000.
           move      zero                 to   w-det-inx-pbe-top      .
       det-inx-pbe-200.
           add       1                    to   w-det-inx-pbe-top      .
           if        w-det-inx-pbe-top    >    w-top-ele-num
                     move  zero           to   w-det-inx-pbe-top
                     go to det-inx-pbe-999.
           if        w-top-cod-top
                    (w-det-inx-pbe-top)   not  = 0740
                     go to det-inx-pbe-200.
       det-inx-pbe-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Close modulo aggiornamento contabilita' genera- *
      *              * le, clienti, fornitori, iva                     *
      *              *-------------------------------------------------*
           perform   mdl-agg-cge-cls-000  thru mdl-agg-cge-cls-999    .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per gestione catena righe         *
      *    *-----------------------------------------------------------*
       cll-sub-cat-000.
           move      "pgm/gep/prg/obj/pgep3002"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using w-cat-rig              .
       cll-sub-cat-999.
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
      *              * Deviazione a seconda del tipo impostazione      *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    spaces
                     go to vis-key-reg-100
           else      go to vis-key-reg-200.
       vis-key-reg-100.
      *              *-------------------------------------------------*
      *              * Se in continuazione impostazione chiave per     *
      *              * provenienza dal main-program                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Nessuna visualizzazione, uscita             *
      *                  *---------------------------------------------*
           go to     vis-key-reg-999.
       vis-key-reg-200.
      *              *-------------------------------------------------*
      *              * In tutti gli altri casi                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data registrazione                          *
      *                  *---------------------------------------------*
           perform   vis-dat-reg-000      thru vis-dat-reg-999        .
      *                  *---------------------------------------------*
      *                  * Codice tipo operazione                      *
      *                  *---------------------------------------------*
           perform   vis-tip-ope-000      thru vis-tip-ope-999        .
      *                  *---------------------------------------------*
      *                  * Codice tipo operazione, descrizione         *
      *                  *---------------------------------------------*
           perform   vis-tip-ope-des-000  thru vis-tip-ope-des-999    .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-key-reg-999.
       vis-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per campi chiave                  *
      *    *-----------------------------------------------------------*
       pmt-key-reg-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo impostazione      *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    spaces
                     go to pmt-key-reg-100
           else      go to pmt-key-reg-200.
       pmt-key-reg-100.
      *              *-------------------------------------------------*
      *              * Se in continuazione impostazione chiave per     *
      *              * provenienza dal main-program                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Erase linee impegnate                       *
      *                  *---------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      06                   to   v-lin                  .
           move      06                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Linea di trattini                           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-key-reg-999.
       pmt-key-reg-200.
      *              *-------------------------------------------------*
      *              * In tutti gli altri casi                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Erase linee impegnate                       *
      *                  *---------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      06                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Data registrazione                          *
      *                  *---------------------------------------------*
           perform   pmt-dat-reg-000      thru pmt-dat-reg-999        .
      *                  *---------------------------------------------*
      *                  * Tipo operazione                             *
      *                  *---------------------------------------------*
           perform   pmt-tip-ope-000      thru pmt-tip-ope-999        .
      *                  *---------------------------------------------*
      *                  * Linea di trattini                           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-key-reg-999.
       pmt-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Prompt per data di registrazione                          *
      *    *-----------------------------------------------------------*
       pmt-dat-reg-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data registrazione :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-dat-reg-999.
           exit.

      *    *===========================================================*
      *    * Prompt per tipo operazione                                *
      *    *-----------------------------------------------------------*
       pmt-tip-ope-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo operazione    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-tip-ope-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Data registrazione                *
      *    *-----------------------------------------------------------*
       vis-dat-reg-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      04                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-tes-dat-reg        to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-dat-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice tipo operazione            *
      *    *-----------------------------------------------------------*
       vis-tip-ope-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      05                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-tes-tip-ope        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-ope-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice tipo operazione, descri-   *
      *    *                         zione                             *
      *    *-----------------------------------------------------------*
       vis-tip-ope-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      27                   to   v-pos                  .
           move      w-tes-tip-ope-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-tip-ope-des-999.
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
      *                  * Pagina 1                                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione func-key d'impostazione *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Data scadenza massima da selezionare    *
      *                      *-----------------------------------------*
           perform   acc-dsc-max-000      thru acc-dsc-max-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-190.
      *                      *-----------------------------------------*
      *                      * Presa visione per pagina 1              *
      *                      *-----------------------------------------*
           perform   acc-pre-vpg-000      thru acc-pre-vpg-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-100.
      *                      *-----------------------------------------*
      *                      * Fine Pagina                             *
      *                      *-----------------------------------------*
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
      *              * Pagina 1                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data scadenza massima da selezionare        *
      *                  *---------------------------------------------*
           perform   vis-dsc-max-000      thru vis-dsc-max-999        .
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           go to     vis-tes-reg-999.
       vis-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts testata                           *
      *    *-----------------------------------------------------------*
       pmt-tes-reg-000.
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
      *                  * Erase linee impegnate                       *
      *                  *---------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      07                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Data scadenza massima da selezionare        *
      *                  *---------------------------------------------*
           perform   pmt-dsc-max-000      thru pmt-dsc-max-999        .
      *                  *---------------------------------------------*
      *                  * Note esplicative sull'operazione            *
      *                  *---------------------------------------------*
           perform   pmt-not-esp-000      thru pmt-not-esp-999        .
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           go to     pmt-tes-reg-999.
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Prompt per : Data scadenza massima da selezionare         *
      *    *-----------------------------------------------------------*
       pmt-dsc-max-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data scadenza massima      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dsc-max-999.
           exit.

      *    *===========================================================*
      *    * Prompt per : Note esplicative sull'operazione             *
      *    *-----------------------------------------------------------*
       pmt-not-esp-000.
      *              *-------------------------------------------------*
      *              * Trattini di separazione                         *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-not-esp-200.
      *              *-------------------------------------------------*
      *              * Righe esplicative                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Nota : Verranno considerate solamente le Scadenze 
      -              "presentate al Salvo buon fine,"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "       o allo Sconto, sempreche' non siano gia' st
      -              "ate dichiarate Insolute, o per"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "       le quali non sia gia' stata dichiarata una 
      -              "Notizia di buon esito, ne' re-"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "       gistrato un Presunto buon esito.           
      -              "                              "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "       Le Scadenze con Data di scadenza superiore 
      -              "alla Data scadenza massima im-"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "       postata non saranno incluse in questa opera
      -              "zione.                        "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-not-esp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Data scadenza massima                *
      *    *-----------------------------------------------------------*
       acc-dsc-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dsc-max-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se le impostazioni di testata sono gia' *
      *                      * state tutte completate: no accettazione *
      *                      *-----------------------------------------*
           if        w-acc-tes-sel-flg    not  = spaces
                     go to acc-dsc-max-999.
       acc-dsc-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      spaces               to   v-edm                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-dsc-max (1)    to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dsc-max-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dsc-max-999.
       acc-dsc-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   w-tes-dsc-max (1)      .
       acc-dsc-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Che non sia a zero                          *
      *                  *---------------------------------------------*
           if        w-tes-dsc-max (1)    =    zero
                     go to acc-dsc-max-100.
       acc-dsc-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dsc-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dsc-max-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dsc-max-100.
       acc-dsc-max-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Data scadenza massima             *
      *    *-----------------------------------------------------------*
       vis-dsc-max-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      spaces               to   v-edm                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-dsc-max (1)    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dsc-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Presa visione per pagina     *
      *    *-----------------------------------------------------------*
       acc-pre-vpg-000.
      *              *-------------------------------------------------*
      *              * Test se il caricamento delle righe e' gia' sta- *
      *              * to effettuato, e conseguente deviazione         *
      *              *-------------------------------------------------*
           if        w-acc-tes-sel-flg    =    spaces
                     go to acc-pre-vpg-150
           else      go to acc-pre-vpg-225.
       acc-pre-vpg-150.
      *              *-------------------------------------------------*
      *              * Se il caricamento delle righe non e' ancora     *
      *              * stato effettuato                                *
      *              *-------------------------------------------------*
       acc-pre-vpg-155.
      *                 *----------------------------------------------*
      *                 * Caricamento righe tramite selezione sui pa-  *
      *                 * rametri impostati                            *
      *                 *----------------------------------------------*
       acc-pre-vpg-156.
      *                     *------------------------------------------*
      *                     * Preparazione routine                     *
      *                     *------------------------------------------*
      *                         *--------------------------------------*
      *                         * Data operazione                      *
      *                         *--------------------------------------*
           move      w-tes-dat-reg        to   w-loa-sdb-inp-dop      .
      *                         *--------------------------------------*
      *                         * Data scadenza massima da selezionare *
      *                         *--------------------------------------*
           move      w-tes-dsc-max (1)    to   w-loa-sdb-inp-sma      .
      *                         *--------------------------------------*
      *                         * Numero di scadenze massimo da sele-  *
      *                         * zionare                              *
      *                         *--------------------------------------*
           move      900                  to   w-loa-sdb-inp-nsm      .
      *                         *--------------------------------------*
      *                         * Importo massimo da selezionare       *
      *                         *--------------------------------------*
           move      zero                 to   w-loa-sdb-inp-igm      .
       acc-pre-vpg-157.
      *                     *------------------------------------------*
      *                     * Richiamo routine di caricamento          *
      *                     *------------------------------------------*
           perform   loa-sdb-sel-000      thru loa-sdb-sel-999        .
       acc-pre-vpg-160.
      *                 *----------------------------------------------*
      *                 * Deviazione a seconda dell'esito del carica-  *
      *                 * mento                                        *
      *                 *----------------------------------------------*
           if        w-loa-sdb-out-flg    =    zero
                     go to acc-pre-vpg-175
           else      go to acc-pre-vpg-200.
       acc-pre-vpg-175.
      *                 *----------------------------------------------*
      *                 * Se caricamento completato con selezione      *
      *                 *----------------------------------------------*
      *                     *------------------------------------------*
      *                     * Flag di completamento accettazione te-   *
      *                     * stata e conseguente precaricamento delle *
      *                     * righe in On                              *
      *                     *------------------------------------------*
           move      "#"                  to   w-acc-tes-sel-flg      .
      *                     *------------------------------------------*
      *                     * Preparazione importo totale scadenze se- *
      *                     * lezionate da importo globale seleziona-  *
      *                     * to                                       *
      *                     *------------------------------------------*
           move      w-loa-sdb-out-igs    to   w-tes-imp-tot (1)      .
      *                     *------------------------------------------*
      *                     * Preparazione uscita per Ok               *
      *                     *------------------------------------------*
           move      spaces               to   v-key                  .
      *                     *------------------------------------------*
      *                     * Uscita                                   *
      *                     *------------------------------------------*
           go to     acc-pre-vpg-999.
       acc-pre-vpg-200.
      *                 *----------------------------------------------*
      *                 * Se caricamento completato con errori o senza *
      *                 * selezione                                    *
      *                 *----------------------------------------------*
      *                     *------------------------------------------*
      *                     * Flag di completamento accettazione te-   *
      *                     * stata e conseguente precaricamento delle *
      *                     * righe in On                              *
      *                     *------------------------------------------*
           move      "#"                  to   w-acc-tes-sel-flg      .
      *                     *------------------------------------------*
      *                     * Preparazione importo totale scadenze se- *
      *                     * lezionate a zero                         *
      *                     *------------------------------------------*
           move      zero                 to   w-tes-imp-tot (1)      .
      *                     *------------------------------------------*
      *                     * Preparazione uscita per Ko               *
      *                     *------------------------------------------*
           move      "EXIT"               to   v-key                  .
           move      "E"                  to   w-cnt-tus-acc-tes      .
      *                     *------------------------------------------*
      *                     * Uscita                                   *
      *                     *------------------------------------------*
           go to     acc-pre-vpg-999.
       acc-pre-vpg-225.
      *              *-------------------------------------------------*
      *              * Se il caricamento delle righe e' gia' stato ef- *
      *              * fettuato                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad accettazione presa visione               *
      *                  *---------------------------------------------*
           go to     acc-pre-vpg-600.
       acc-pre-vpg-600.
      *              *-------------------------------------------------*
      *              * Accettazione valore per presa visione           *
      *              *-------------------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      spaces               to   v-msk                  .
           move      spaces               to   v-alf                  .
           move      spaces               to   v-not                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-cnt-sts-imp-npt    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
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
      *              *-------------------------------------------------*
           if        w-cat-rig-max        =    zero
                     move  spaces         to   w-cnt-sts-imp-cor
           else      move  "#"            to   w-cnt-sts-imp-cor      .
           if        w-cat-rig-max        =    1        and
                     w-cat-rig-new        not  = spaces and
                     w-cat-rig-lst        not  = spaces
                     move  spaces         to   w-cnt-sts-imp-cor      .
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
      *                  * Accettazione numero scadenza                *
      *                  *---------------------------------------------*
           perform   acc-num-sdb-000      thru acc-num-sdb-999        .
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
      *              *-------------------------------------------------*
           if        w-cat-rig-max        =    zero
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
           move      10                   to   v-lin                  .
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
       vis-lin-cor-500.
      *                      *-----------------------------------------*
      *                      * Editings                                *
      *                      *-----------------------------------------*
       vis-lin-cor-525.
      *                          *-------------------------------------*
      *                          * Flag di errore su determinazione se *
      *                          * aggiornamenti contabili eseguiti a  *
      *                          * fronte della scadenza o della di-   *
      *                          * stinta                              *
      *                          *-------------------------------------*
           if        w-rig-flg-err-sdb (1)
                                          not  = spaces
                     move  "*E*"          to   w-lin-imm-err-dac      .
       vis-lin-cor-550.
      *                          *-------------------------------------*
      *                          * Codice debitore, ragione sociale    *
      *                          *-------------------------------------*
           move      w-rig-dpz-dbt-rag (1)
                                          to   w-lin-imm-rag-dpz      .
       vis-lin-cor-575.
      *                          *-------------------------------------*
      *                          * Importo scadenza                    *
      *                          *-------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           move      w-rig-imp-sdb (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-lin-imm-imp-sca      .
       vis-lin-cor-600.
      *                          *-------------------------------------*
      *                          * Data scadenza                       *
      *                          *-------------------------------------*
           if        w-rig-dts-sdb (1)    =    zero
                     go to vis-lin-cor-605
           else      go to vis-lin-cor-610.
       vis-lin-cor-605.
           move      " A vista"           to   w-lin-imm-dat-sca      .
           go to     vis-lin-cor-625.
       vis-lin-cor-610.
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      w-rig-dts-sdb (1)    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-lin-imm-dat-sca      .
           go to     vis-lin-cor-625.
       vis-lin-cor-625.
      *                      *-----------------------------------------*
      *                      * Fine editings                           *
      *                      *-----------------------------------------*
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
           move      07                   to   v-lin                  .
           move      18                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Fincatura                                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "N.R            Ragione sociale del debitore       
      -              "        Importo       Scadenza"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Sottolineatura fincatura                        *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "---  ---------------------------------------------
      -              "---  --------------   --------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Sopralineatura per il totale selezionato        *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                                     -------------
      -              "-------------------           "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Literal per il totale selezionato               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                                     Totale scaden
      -              "ze :                          "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Totale delle scadenze selezionate               *
      *              *-------------------------------------------------*
           perform   vis-imp-tot-000      thru vis-imp-tot-999        .
      *              *-------------------------------------------------*
      *              * Riga di chiusura                                *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "--------------------------------------------------
      -              "------------------------------"
                                          to   v-alf                  .
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
      *              * Visualizzazione riga corpo di accettazione in   *
      *              * w-rig senza test se riga di tipo New            *
      *              *-------------------------------------------------*
           perform   vfo-rig-cor-000      thru vfo-rig-cor-999        .
       vis-rig-cor-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione riga corpo di accettazione in w-rig senza *
      *    * test se riga di tipo New                                  *
      *    *-----------------------------------------------------------*
       vfo-rig-cor-000.
      *              *-------------------------------------------------*
      *              * Numero scadenza                                 *
      *              *-------------------------------------------------*
           perform   vis-num-sdb-000      thru vis-num-sdb-999        .
      *              *-------------------------------------------------*
      *              * Tipo scadenza                                   *
      *              *-------------------------------------------------*
           perform   vis-tip-sdb-000      thru vis-tip-sdb-999        .
      *              *-------------------------------------------------*
      *              * Data scadenza                                   *
      *              *-------------------------------------------------*
           perform   vis-dts-sdb-000      thru vis-dts-sdb-999        .
      *              *-------------------------------------------------*
      *              * Importo scadenza                                *
      *              *-------------------------------------------------*
           perform   vis-imp-sdb-000      thru vis-imp-sdb-999        .
      *              *-------------------------------------------------*
      *              * Codice debitore con Codice dipendenza           *
      *              *-------------------------------------------------*
           perform   vis-ccd-dbt-000      thru vis-ccd-dbt-999        .
      *              *-------------------------------------------------*
      *              * Codice debitore, ragione sociale                *
      *              *-------------------------------------------------*
           perform   vis-cod-dbt-rag-000  thru vis-cod-dbt-rag-999    .
       vfo-rig-cor-999.
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
           move      19                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts per riga corpo espansa  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero scadenza                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero scadenza :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Data scadenza                               *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data scadenza   :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Importo scadenza                            *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      46                   to   v-pos                  .
           move      "Importo :"          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Codice cliente                              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice cliente  :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-rig-cor-999.
           exit.

      *    *===========================================================*
      *    * Accettazione Numero scadenza                              *
      *    *-----------------------------------------------------------*
       acc-num-sdb-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-num-sdb-025.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-rig-num-sdb (1)    to   w-sav-num-sdb          .
       acc-num-sdb-050.
      *                  *---------------------------------------------*
      *                  * Tests per riga vuota, il che puo' succedere *
      *                  * solamente in Append                         *
      *                  *---------------------------------------------*
       acc-num-sdb-055.
      *                      *-----------------------------------------*
      *                      * Se Numero scadenza diverso da zero : la *
      *                      * riga non puo' essere vuota, pertanto si *
      *                      * va' alla accettazione, dopo aver norma- *
      *                      * lizzato il Flag per impostazione riga   *
      *                      * vuota                                   *
      *                      *-----------------------------------------*
           if        w-rig-num-sdb (1)    not  = zero
                     move  spaces         to   w-tes-flg-irv (1)
                     go to acc-num-sdb-100.
       acc-num-sdb-060.
      *                      *-----------------------------------------*
      *                      * Se Numero scadenza pari a zero          *
      *                      *-----------------------------------------*
       acc-num-sdb-065.
      *                          *-------------------------------------*
      *                          * Se il flag per impostazione riga    *
      *                          * vuota e' a Spaces : si pone questo  *
      *                          * flag in On e si simula una impo-    *
      *                          * stazione con Return                 *
      *                          *-------------------------------------*
           if        w-tes-flg-irv (1)    =    spaces
                     move  "#"            to   w-tes-flg-irv (1)
                     move  w-rig-num-sdb (1)
                                          to   v-num
                     move  spaces         to   v-key
                     go to acc-num-sdb-210.
       acc-num-sdb-070.
      *                          *-------------------------------------*
      *                          * Altrimenti si pone questo flag in   *
      *                          * Off e si simula una impostazione    *
      *                          * con Up                              *
      *                          *-------------------------------------*
           move      spaces               to   w-tes-flg-irv (1)      .
           move      w-rig-num-sdb (1)    to   v-num                  .
           move      "UP  "               to   v-key                  .
           go to     acc-num-sdb-210.
       acc-num-sdb-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripristino valore precedente                *
      *                  *---------------------------------------------*
           move      w-sav-num-sdb        to   w-rig-num-sdb (1)      .
      *                  *---------------------------------------------*
      *                  * Parametri generici                          *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      "<"                  to   v-edm                  .
           move      19                   to   v-lin                  .
           move      19                   to   v-pos                  .
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
      *                      * Find : mai ammesso                      *
      *                      *-----------------------------------------*
           move      spaces               to   v-pfk (03)             .
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
                     move  "DO  "         to   v-pfk (05)             .
      *                      *-----------------------------------------*
      *                      * Remv : sempre ammesso, a meno che non   *
      *                      * si sia sull'unica  riga                 *     
      *                      *-----------------------------------------*
           if        w-cat-rig-max        >    1
                     move  "REMV"         to   v-pfk (06)             .
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
      *                      * Expd : sempre ammesso                   *
      *                      *-----------------------------------------*
           move      "EXPD"               to   v-pfk (12)             .
      *                      *-----------------------------------------*
      *                      * Valore di accettazione                  *
      *                      *-----------------------------------------*
           move      w-rig-num-sdb (1)    to   v-num                  .
       acc-num-sdb-200.
      *                      *-----------------------------------------*
      *                      * Richiamo subroutine di accettazione     *
      *                      *-----------------------------------------*
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-num-sdb-210.
      *              *-------------------------------------------------*
      *              * Se Return                                       *
      *              *-------------------------------------------------*
           if        v-key                =    spaces
                     go to acc-num-sdb-400.
       acc-num-sdb-225.
      *              *-------------------------------------------------*
      *              * Se Expd                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "EXPD"
                     go to acc-num-sdb-250.
      *                  *---------------------------------------------*
      *                  * Espansione su scadenze debitori             *
      *                  *---------------------------------------------*
           move      "N"                  to   w-esp-arc-sdb-sns      .
           move      v-num                to   w-esp-arc-sdb-num      .
           perform   esp-arc-sdb-000      thru esp-arc-sdb-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-num-sdb-100.
       acc-num-sdb-250.
      *              *-------------------------------------------------*
      *              * Se Slct                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "SLCT"
                     go to acc-num-sdb-300
           else      go to acc-num-sdb-350.
       acc-num-sdb-300.
      *                  *---------------------------------------------*
      *                  * Conversione numero riga impostato in forma- *
      *                  * to numerico                                 *
      *                  *---------------------------------------------*
           move      v-num                to   w-cnt-slc-rap-num      .
       acc-num-sdb-320.
      *                  *---------------------------------------------*
      *                  * Se numero riga impostato minore di 1 o mag- *
      *                  * maggiore del massimo : reimpostazione       *
      *                  *---------------------------------------------*
           if        w-cnt-slc-rap-num    =    zero       or
                     w-cnt-slc-rap-num    >    w-cat-rig-max
                     go to acc-num-sdb-100.
      *                  *---------------------------------------------*
      *                  * Se numero riga impostato pari a numero riga *
      *                  * attuale : reimpostazione                    *
      *                  *---------------------------------------------*
           if        w-cnt-slc-rap-num    =    w-cnt-cor-nrg-dac
                     go to acc-num-sdb-100.
      *                  *---------------------------------------------*
      *                  * Preparazione parametri e uscita             *
      *                  *---------------------------------------------*
           move      w-cnt-slc-rap-num    to   w-cnt-slc-num-rig      .
           move      "."                  to   w-cnt-tus-acc-rig      .
           go to     acc-num-sdb-999.
       acc-num-sdb-350.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-rig
                     go to acc-num-sdb-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-rig
                     go to acc-num-sdb-999.
      *              *-------------------------------------------------*
      *              * Se premuto un altro tasto funzione non deve es- *
      *              * sere avvenuta variazione del campo d'impostaz.  *
      *              *-------------------------------------------------*
           if        v-mod                not  = spaces
                     go to acc-num-sdb-100.
      *              *-------------------------------------------------*
      *              * Se Up                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "UP  "
                     move  "U"            to   w-cnt-tus-acc-rig
                     go to acc-num-sdb-999.
      *              *-------------------------------------------------*
      *              * Se Down                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DOWN"
                     move  "D"            to   w-cnt-tus-acc-rig
                     go to acc-num-sdb-999.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-rig
                             go to acc-num-sdb-999
                     else    go to acc-num-sdb-100.
      *              *-------------------------------------------------*
      *              * Se Insr                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "INSR"
                     move  "I"            to   w-cnt-tus-acc-rig
                     go to acc-num-sdb-999.
      *              *-------------------------------------------------*
      *              * Se Remv                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "REMV"
                     move  "R"            to   w-cnt-tus-acc-rig
                     go to acc-num-sdb-999.
      *              *-------------------------------------------------*
      *              * Se Tab                                          *
      *              *-------------------------------------------------*
           if        v-key                =    "TAB "
                     move  "T"            to   w-cnt-tus-acc-rig
                     go to acc-num-sdb-999.
      *              *-------------------------------------------------*
      *              * Se Back                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "BACK"
                     move  "B"            to   w-cnt-tus-acc-rig
                     go to acc-num-sdb-999.
      *              *-------------------------------------------------*
      *              * Se Nxsc                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "NXSC"
                     move  "N"            to   w-cnt-tus-acc-rig
                     go to acc-num-sdb-999.
      *              *-------------------------------------------------*
      *              * Se Prsc                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "PRSC"
                     move  "P"            to   w-cnt-tus-acc-rig
                     go to acc-num-sdb-999.
       acc-num-sdb-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del valore precedente  *
      *                  *---------------------------------------------*
           if        w-sav-num-sdb        =    zero
                     go to acc-num-sdb-450.
       acc-num-sdb-425.
      *                  *---------------------------------------------*
      *                  * Se valore precedente diverso da zero        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda se il valore e'    *
      *                      * stato variato oppure no                 *
      *                      *-----------------------------------------*
           if        v-num                =    w-sav-num-sdb
                     go to acc-num-sdb-430
           else      go to acc-num-sdb-435.
       acc-num-sdb-430.
      *                      *-----------------------------------------*
      *                      * Se il valore non e' stato variato       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Normalizzazione function-key        *
      *                          *-------------------------------------*
           move      spaces               to   v-key                  .
      *                          *-------------------------------------*
      *                          * Ad uscita                           *
      *                          *-------------------------------------*
           go to     acc-num-sdb-999.
       acc-num-sdb-435.
      *                      *-----------------------------------------*
      *                      * Se il valore e' stato variato           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A reimpostazione                    *
      *                          *-------------------------------------*
           go to     acc-num-sdb-100.
       acc-num-sdb-450.
      *                  *---------------------------------------------*
      *                  * Se valore precedente a zero                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Simulazione Down                        *
      *                      *-----------------------------------------*
           move      "D"                  to   w-cnt-tus-acc-rig      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     acc-num-sdb-999.
       acc-num-sdb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione Numero scadenza                           *
      *    *-----------------------------------------------------------*
       vis-num-sdb-000.
           move      "DS"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      "<"                  to   v-edm                  .
           move      19                   to   v-lin                  .
           move      19                   to   v-pos                  .
           move      w-rig-num-sdb (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-sdb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione Tipo scadenza                             *
      *    *-----------------------------------------------------------*
       vis-tip-sdb-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-sdb-lun    to   v-car                  .
           move      w-exp-tip-sdb-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-sdb-tbl    to   v-txt                  .
           move      19                   to   v-lin                  .
           move      32                   to   v-pos                  .
           move      w-rig-tip-sdb (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-sdb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione Data scadenza                             *
      *    *-----------------------------------------------------------*
       vis-dts-sdb-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del valore della data di *
      *              * scadenza                                        *
      *              *-------------------------------------------------*
           if        w-rig-dts-sdb (1)    =    zero
                     go to vis-dts-sdb-200
           else      go to vis-dts-sdb-400.
       vis-dts-sdb-200.
      *              *-------------------------------------------------*
      *              * Se data di scadenza a zero                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione literal 'A vista'           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      19                   to   v-pos                  .
           move      " A vista"           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-dts-sdb-999.
       vis-dts-sdb-400.
      *              *-------------------------------------------------*
      *              * Se data di scadenza diversa da zero             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione data scadenza               *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      20                   to   v-lin                  .
           move      19                   to   v-pos                  .
           move      w-rig-dts-sdb (1)    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-dts-sdb-999.
       vis-dts-sdb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione Importo scadenza                          *
      *    *-----------------------------------------------------------*
       vis-imp-sdb-000.
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           move      20                   to   v-lin                  .
           move      55                   to   v-pos                  .
           move      w-rig-imp-sdb (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-imp-sdb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione Codice debitore con Codice dipendenza     *
      *    *-----------------------------------------------------------*
       vis-ccd-dbt-000.
      *              *-------------------------------------------------*
      *              * Editing del codice debitore                     *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-rig-cod-dbt (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del valore del codice del- *
      *              * la dipendenza                                   *
      *              *-------------------------------------------------*
           if        w-rig-dpz-dbt (1)    =    spaces
                     go to vis-ccd-dbt-200
           else      go to vis-ccd-dbt-400.
       vis-ccd-dbt-200.
      *              *-------------------------------------------------*
      *              * Se codice dipendenza a spaces                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      19                   to   v-pos                  .
           move      v-edt                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-ccd-dbt-999.
       vis-ccd-dbt-400.
      *              *-------------------------------------------------*
      *              * Se codice dipendenza diverso da spaces          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione valore editato composto        *
      *                  *---------------------------------------------*
           move      spaces               to   v-alf                  .
           string    v-edt      delimited by   spaces
                     "-"        delimited by   size
                     w-rig-dpz-dbt (1)
                                delimited by   spaces
                                          into v-alf                  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      19                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-ccd-dbt-999.
       vis-ccd-dbt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione Codice debitore, ragione sociale          *
      *    *-----------------------------------------------------------*
       vis-cod-dbt-rag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      32                   to   v-pos                  .
           move      w-rig-dpz-dbt-rag (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-dbt-rag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione Totale selezionato                        *
      *    *-----------------------------------------------------------*
       vis-imp-tot-000.
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "G"                  to   v-edm                  .
           move      17                   to   v-lin                  .
           move      55                   to   v-pos                  .
           move      w-tes-imp-tot (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-imp-tot-999.
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
      *              *-------------------------------------------------*
      *              * Prelevamento da area di controllo catena        *
      *              *-------------------------------------------------*
           move      w-cat-rig-prg        to   w-rig-num-prg (1)      .
       prg-rig-cor-999.
           exit.

      *    *===========================================================*
      *    * Azioni post-accettazione riga corpo                       *
      *    *-----------------------------------------------------------*
       pos-rig-cor-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo aggiornamento     *
      *              *-------------------------------------------------*
           if        w-cnt-cor-tip-agg    =    "+"
                     go to pos-rig-cor-300
           else if   w-cnt-cor-tip-agg    =    "-"
                     go to pos-rig-cor-600
           else      go to pos-rig-cor-999.
       pos-rig-cor-300.
      *              *-------------------------------------------------*
      *              * Se tipo aggiornamento '+'                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non e' una riga di tipo New non si ese-  *
      *                  * gue alcun aggiornamento                     *
      *                  *---------------------------------------------*
           if        w-cat-rig-new        =    spaces
                     go to pos-rig-cor-350.
      *                  *---------------------------------------------*
      *                  * Aggiornamento in piu' per il totale sele-   *
      *                  * zionato                                     *
      *                  *---------------------------------------------*
           add       w-rig-imp-sdb (1)    to   w-tes-imp-tot (1)      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione totale selezionato          *
      *                  *---------------------------------------------*
           perform   vis-imp-tot-000      thru vis-imp-tot-999        .
       pos-rig-cor-350.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pos-rig-cor-999.
       pos-rig-cor-600.
      *              *-------------------------------------------------*
      *              * Se tipo aggiornamento '-'                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Aggiornamento in meno per il totale sele-   *
      *                  * zionato                                     *
      *                  *---------------------------------------------*
           subtract  w-rig-imp-sdb (1)    from w-tes-imp-tot (1)      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione totale selezionato          *
      *                  *---------------------------------------------*
           perform   vis-imp-tot-000      thru vis-imp-tot-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pos-rig-cor-999.
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
      *              *-------------------------------------------------*
      *              * Data registrazione                              *
      *              *-------------------------------------------------*
           move      w-key-dat-reg        to   w-tes-dat-reg          .
      *              *-------------------------------------------------*
      *              * Codice tipo operazione                          *
      *              *-------------------------------------------------*
           move      w-key-tip-ope        to   w-tes-tip-ope          .
      *              *-------------------------------------------------*
      *              * Codice tipo operazione, descrizione             *
      *              *-------------------------------------------------*
           move      w-key-tip-ope-des    to   w-tes-tip-ope-des      .
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *-----------------------------------------------------------*
       nor-nok-tes-000.
           move      spaces               to   w-tes-flg-irv (1)      .
           move      zero                 to   w-tes-dsc-max (1)      .
           move      zero                 to   w-tes-imp-tot (1)      .
       nor-nok-tes-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave riga corpo, ad esclusione *
      *    * del numero progressivo riga                               *
      *    *-----------------------------------------------------------*
       nor-nok-rig-000.
           move      zero                 to   w-rig-num-sdb (1)      .
           move      zero                 to   w-rig-tip-sdb (1)      .
           move      zero                 to   w-rig-dts-sdb (1)      .
           move      zero                 to   w-rig-tip-dbt (1)      .
           move      zero                 to   w-rig-cod-dbt (1)      .
           move      zero                 to   w-rig-cod-dbt-cge (1)  .
           move      spaces               to   w-rig-cod-dbt-rag (1)  .
           move      spaces               to   w-rig-dpz-dbt (1)      .
           move      spaces               to   w-rig-dpz-dbt-rag (1)  .
           move      zero                 to   w-rig-inl-sdb (1)      .
           move      zero                 to   w-rig-imp-sdb (1)      .
           move      zero                 to   w-rig-dat-ddr (1)      .
           move      spaces               to   w-rig-num-ddr (1)      .
           move      spaces               to   w-rig-agg-emi-snx (1)  .
           move      zero                 to   w-rig-agg-emi-ctp (1)  .
           move      spaces               to   w-rig-agg-pre-snx (1)  .
           move      spaces               to   w-rig-snx-agg-cge (1)  .
           move      spaces               to   w-rig-flg-err-sdb (1)  .
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
      *              * Flag di completamento accettazione e conseguen- *
      *              * te precaricamento delle righe in Off            *
      *              *-------------------------------------------------*
           move      spaces               to   w-acc-tes-sel-flg      .
      *              *-------------------------------------------------*
      *              * Tipo funzionamento : Inserimento                *
      *              *-------------------------------------------------*
           move      "I"                  to   w-cnt-mfu-tip-fun      .
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
      *              * Aggiornamento default per data registrazione    *
      *              *-------------------------------------------------*
           move      w-key-dat-reg        to   w-def-dat-reg          .
      *              *-------------------------------------------------*
      *              * Aggiornamento default per tipo operazione       *
      *              *-------------------------------------------------*
           move      w-key-tip-ope        to   w-def-tip-ope          .
       pos-cnf-ins-050.
      *              *-------------------------------------------------*
      *              * Messaggio di caricamento in esecuzione          *
      *              *-------------------------------------------------*
           move      "         Operazione di Presunto buon esito in esec
      -              "uzione         "
                                          to   w-err-box-err-msg      .
           perform   msg-cnt-box-000      thru msg-cnt-box-999        .
      *              *-------------------------------------------------*
      *              * Start su righe corpo in catena movimenti        *
      *              *-------------------------------------------------*
           move      "ST"                 to   w-cat-rig-ope          .
           move      1                    to   w-cat-rig-num          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
      *              *-------------------------------------------------*
      *              * Se start non valida : a fine trattamento righe  *
      *              * caricate                                        *
      *              *-------------------------------------------------*
           if        w-cat-rig-exs        not  = spaces
                     go to pos-cnf-ins-800.
       pos-cnf-ins-200.
      *              *-------------------------------------------------*
      *              * Next su righe corpo in catena movimenti         *
      *              *-------------------------------------------------*
           move      "RN"                 to   w-cat-rig-ope          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
      *              *-------------------------------------------------*
      *              * Se At end : a fine trattamento righe caricate   *
      *              *-------------------------------------------------*
           if        w-cat-rig-exs        not  = spaces
                     go to pos-cnf-ins-800.
       pos-cnf-ins-250.
      *              *-------------------------------------------------*
      *              * Movimento da buffer catena movimenti a work per *
      *              * riga w-rig                                      *
      *              *-------------------------------------------------*
           move      w-cat-rig-buf        to   w-rig                  .
       pos-cnf-ins-275.
      *              *-------------------------------------------------*
      *              * Se scadenza con errori : la si ignora e si ri-  *
      *              * cicla alla riga corpo successiva                *
      *              *-------------------------------------------------*
           if        w-rig-flg-err-sdb (1)
                                          not  = spaces
                     go to pos-cnf-ins-200.
      *              *-------------------------------------------------*
      *              * Lettura scadenza, con lock                      *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "NUMSDB    "         to   f-key                  .
           move      w-rig-num-sdb (1)    to   rf-sdb-num-sdb         .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
       pos-cnf-ins-300.
      *              *-------------------------------------------------*
      *              * Se scadenza non trovata                         *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to pos-cnf-ins-350.
      *                  *---------------------------------------------*
      *                  * Emissione messaggio di errore               *
      *                  *---------------------------------------------*
           perform   e11-cnf-ins-000      thru e11-cnf-ins-999        .
      *                  *---------------------------------------------*
      *                  * Riciclo a leggere riga corpo successiva     *
      *                  *---------------------------------------------*
           go to     pos-cnf-ins-200.
       pos-cnf-ins-350.
      *              *-------------------------------------------------*
      *              * Test se scadenza nel frattempo variata          *
      *              *-------------------------------------------------*
       pos-cnf-ins-360.
      *                  *---------------------------------------------*
      *                  * Test su movimenti effettuati sulla scadenza *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-sto       not  = zero or
                     rf-sdb-dtr-ris       not  = zero or
                     rf-sdb-dtr-rsp       not  = zero or
                     rf-sdb-dtr-isp       not  = zero or
                     rf-sdb-dtr-acs       not  = zero or
                     rf-sdb-dtr-nbe       not  = zero or
                     rf-sdb-dtr-pbe       not  = zero
                     go to pos-cnf-ins-380.
      *                  *---------------------------------------------*
      *                  * Test su flags della scadenza                *
      *                  *---------------------------------------------*
           if        rf-sdb-flg-blo       not  = spaces or
                     rf-sdb-flg-pul       not  = spaces
                     go to pos-cnf-ins-380.
      *                  *---------------------------------------------*
      *                  * Test sulla distinta di presentazione        *
      *                  *---------------------------------------------*
           if        rf-sdb-num-ddp       =    zero
                     go to pos-cnf-ins-380.
       pos-cnf-ins-370.
      *                  *---------------------------------------------*
      *                  * Se la scadenza non e' nel frattempo stata   *
      *                  * variata : continuazione                     *
      *                  *---------------------------------------------*
           go to     pos-cnf-ins-400.
       pos-cnf-ins-380.
      *                  *---------------------------------------------*
      *                  * Se la scadenza e' nel frattempo stata va-   *
      *                  * riata                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Unlock [sdb]                            *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *                      *-----------------------------------------*
      *                      * Emissione messaggio di errore           *
      *                      *-----------------------------------------*
           perform   e21-cnf-ins-000      thru e21-cnf-ins-999        .
      *                      *-----------------------------------------*
      *                      * Riciclo a leggere riga corpo successiva *
      *                      *-----------------------------------------*
           go to     pos-cnf-ins-200.
       pos-cnf-ins-400.
      *              *-------------------------------------------------*
      *              * Aggiornamenti contabili generati dalla gestione *
      *              * portafoglio a fronte del movimento di presunto  *
      *              * buon esito per scadenze presentate al SBF o al- *
      *              * lo sconto                                       *
      *              *-------------------------------------------------*
           perform   cge-gep-pbe-000      thru cge-gep-pbe-999        .
       pos-cnf-ins-500.
      *              *-------------------------------------------------*
      *              * Completamento del record scadenze con i dati    *
      *              * relativi al movimento di presunto buon esito    *
      *              * per scadenze presentate al SBF o allo sconto    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data di sistema di ultimo inserimento o mo- *
      *                  * difica, utente, fase                        *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-sdb-ide-dat         .
           move      s-ute                to   rf-sdb-ide-ute         .
           move      s-fas                to   rf-sdb-ide-fas         .
      *                  *---------------------------------------------*
      *                  * Data registrazione movimento                *
      *                  *---------------------------------------------*
           move      w-tes-dat-reg        to   rf-sdb-dtr-pbe         .
      *                  *---------------------------------------------*
      *                  * Data registrazione movimento contabile a    *
      *                  * fronte del movimento                        *
      *                  *---------------------------------------------*
           move      l-cge-300-dat-reg    to   rf-sdb-drc-pbe         .
      *                  *---------------------------------------------*
      *                  * Numero protocollo movimento contabile a     *
      *                  * fronte del movimento                        *
      *                  *---------------------------------------------*
           move      l-cge-300-num-prt    to   rf-sdb-npc-pbe         .
      *                  *---------------------------------------------*
      *                  * Data chiusura scadenza                      *
      *                  *---------------------------------------------*
           perform   det-dat-chs-000      thru det-dat-chs-999        .
       pos-cnf-ins-550.
      *              *-------------------------------------------------*
      *              * Update record [sdb]                             *
      *              *-------------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
       pos-cnf-ins-600.
      *              *-------------------------------------------------*
      *              * Unlock record [sdb]                             *
      *              *-------------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
       pos-cnf-ins-700.
      *              *-------------------------------------------------*
      *              * Riciclo a riga corpo successiva                 *
      *              *-------------------------------------------------*
           go to     pos-cnf-ins-200.
       pos-cnf-ins-800.
      *              *-------------------------------------------------*
      *              * Fine trattamento righe caricate                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Cancellazione messaggio di caricamento in   *
      *                  * esecuzione                                  *
      *                  *---------------------------------------------*
           move      spaces               to   w-err-box-err-msg      .
           perform   msg-cnt-box-000      thru msg-cnt-box-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pos-cnf-ins-999.
       pos-cnf-ins-999.
           exit.

      *    *===========================================================*
      *    * Messaggio di errore di tipo 11 su conferma impostazioni   *
      *    * per inserimento                                           *
      *    *                                                           *
      *    * Scadenza non piu' esistente                               *
      *    *-----------------------------------------------------------*
       e11-cnf-ins-000.
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
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      17                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Literal per 'Attenzione'                        *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Attenzione :"       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Contenuto per il literal di 'Attenzione'        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione numero scadenza editato        *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      "<"                  to   v-edm                  .
           move      w-rig-num-sdb (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione contenuto literal di 'At-   *
      *                  * tenzione'                                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      63                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      16                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           string    "La scadenza numero "
                                delimited by   size
                     v-edt      delimited by   spaces
                     " non esiste piu' in archivio !"
                                delimited by   size
                                          into v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Literal per 'Messaggio'                         *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Messaggio  :"       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Contenuto per il literal di 'Messaggio'         *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      63                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      16                   to   v-pos                  .
           move      "Per la scadenza non sara' applicato il Presunto bu
      -              "uon esito.   "      to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Literal per 'Presa visione'                     *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      "Premere un tasto per presa visione [ ]"
                                          to   v-alf                  .
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
           move      15                   to   v-lin                  .
           move      77                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       e11-cnf-ins-999.
           exit.

      *    *===========================================================*
      *    * Messaggio di errore di tipo 21 su conferma impostazioni   *
      *    * per inserimento                                           *
      *    *                                                           *
      *    * Scadenza che nel frattempo e' stata variata               *
      *    *-----------------------------------------------------------*
       e21-cnf-ins-000.
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
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      17                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Literal per 'Attenzione'                        *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Attenzione :"       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Contenuto per il literal di 'Attenzione'        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione numero scadenza editato        *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      "<"                  to   v-edm                  .
           move      w-rig-num-sdb (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione contenuto literal di 'At-   *
      *                  * tenzione'                                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      63                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      16                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           string    "La scadenza nr. "
                                delimited by   size
                     v-edt      delimited by   spaces
                     " e' nel frattempo stata variata !"
                                delimited by   size
                                          into v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Literal per 'Messaggio'                         *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Messaggio  :"       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Contenuto per il literal di 'Messaggio'         *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      63                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      16                   to   v-pos                  .
           move      "Per la scadenza non sara' applicato il Presunto bu
      -              "uon esito.   "      to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Literal per 'Presa visione'                     *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      "Premere un tasto per presa visione [ ]"
                                          to   v-alf                  .
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
           move      15                   to   v-lin                  .
           move      77                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       e21-cnf-ins-999.
           exit.

      *    *===========================================================*
      *    * Routine post-conferma di modifica                         *
      *    *-----------------------------------------------------------*
       pos-cnf-mod-000.
       pos-cnf-mod-999.
           exit.

      *    *===========================================================*
      *    * Routine post-conferma di annullamento                     *
      *    *-----------------------------------------------------------*
       pos-cnf-ann-000.
       pos-cnf-ann-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio debitori                      *
      *    *-----------------------------------------------------------*
       let-arc-dbt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dbt-flg      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo debitore          *
      *              *-------------------------------------------------*
           if        w-let-arc-dbt-tip    =    zero
                     go to let-arc-dbt-100
           else if   w-let-arc-dbt-tip    =    01
                     go to let-arc-dbt-200
           else      go to let-arc-dbt-900.
       let-arc-dbt-100.
      *              *-------------------------------------------------*
      *              * Se tipo debitore : zero                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del codice debitore    *
      *                  *---------------------------------------------*
           if        w-let-arc-dbt-cod    =    zero
                     go to let-arc-dbt-120
           else      go to let-arc-dbt-140.
       let-arc-dbt-120.
      *                  *---------------------------------------------*
      *                  * Se codice debitore : zero                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione work area               *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-dbt-rag      .
           move      spaces               to   w-let-arc-dbt-via      .
           move      spaces               to   w-let-arc-dbt-loc      .
           move      zero                 to   w-let-arc-dbt-cge      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-arc-dbt-999.
       let-arc-dbt-140.
      *                  *---------------------------------------------*
      *                  * Se codice debitore : diverso da zero        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Marker di uscita ad errore              *
      *                      *-----------------------------------------*
           move      "#"                  to   w-let-arc-dbt-flg      .
           move      all   "."            to   w-let-arc-dbt-rag      .
           move      spaces               to   w-let-arc-dbt-via      .
           move      spaces               to   w-let-arc-dbt-loc      .
           move      zero                 to   w-let-arc-dbt-cge      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-arc-dbt-999.
       let-arc-dbt-200.
      *              *-------------------------------------------------*
      *              * Se tipo debitore : 01                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [cli]                      *
      *                  *---------------------------------------------*
           move      w-let-arc-dbt-cod    to   w-let-arc-cli-cod      .
           perform   let-arc-cli-000      thru let-arc-cli-999        .
      *                  *---------------------------------------------*
      *                  * Riporto risultati da lettura                *
      *                  *---------------------------------------------*
           move      w-let-arc-cli-flg    to   w-let-arc-dbt-flg      .
           move      w-let-arc-cli-rag    to   w-let-arc-dbt-rag      .
           move      w-let-arc-cli-via    to   w-let-arc-dbt-via      .
           move      w-let-arc-cli-loc    to   w-let-arc-dbt-loc      .
           move      w-let-arc-cli-cge    to   w-let-arc-dbt-cge      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-dbt-999.
       let-arc-dbt-900.
      *              *-------------------------------------------------*
      *              * Se tipo debitore : non riconosciuto             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del codice debitore    *
      *                  *---------------------------------------------*
           if        w-let-arc-dbt-cod    =    zero
                     go to let-arc-dbt-920
           else      go to let-arc-dbt-940.
       let-arc-dbt-920.
      *                  *---------------------------------------------*
      *                  * Se codice debitore : zero                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione work area               *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-dbt-rag      .
           move      spaces               to   w-let-arc-dbt-via      .
           move      spaces               to   w-let-arc-dbt-loc      .
           move      zero                 to   w-let-arc-dbt-cge      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-arc-dbt-999.
       let-arc-dbt-940.
      *                  *---------------------------------------------*
      *                  * Se codice debitore : diverso da zero        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Marker di uscita ad errore              *
      *                      *-----------------------------------------*
           move      "#"                  to   w-let-arc-dbt-flg      .
           move      all   "."            to   w-let-arc-dbt-rag      .
           move      spaces               to   w-let-arc-dbt-via      .
           move      spaces               to   w-let-arc-dbt-loc      .
           move      zero                 to   w-let-arc-dbt-cge      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-arc-dbt-999.
       let-arc-dbt-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [dbt]                         *
      *    *-----------------------------------------------------------*
       let-dpz-dbt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-dpz-dbt-flg      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo debitore          *
      *              *-------------------------------------------------*
           if        w-let-dpz-dbt-tip    =    zero
                     go to let-dpz-dbt-100
           else if   w-let-dpz-dbt-tip    =    01
                     go to let-dpz-dbt-200
           else      go to let-dpz-dbt-900.
       let-dpz-dbt-100.
      *              *-------------------------------------------------*
      *              * Se tipo debitore : zero                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del codice debitore e  *
      *                  * della dipendenza                            *
      *                  *---------------------------------------------*
           if        w-let-dpz-dbt-cod    =    zero   and
                     w-let-dpz-dbt-dpz    =    spaces
                     go to let-dpz-dbt-120
           else      go to let-dpz-dbt-140.
       let-dpz-dbt-120.
      *                  *---------------------------------------------*
      *                  * Se codice debitore e dipendenza a zero ed a *
      *                  * spaces                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione work area               *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-dpz-dbt-rag      .
           move      spaces               to   w-let-dpz-dbt-via      .
           move      spaces               to   w-let-dpz-dbt-loc      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-dpz-dbt-999.
       let-dpz-dbt-140.
      *                  *---------------------------------------------*
      *                  * Se codice debitore e dipendenza diversi da  *
      *                  * zero e spaces                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Marker di uscita ad errore              *
      *                      *-----------------------------------------*
           move      "#"                  to   w-let-dpz-dbt-flg      .
           move      all   "."            to   w-let-dpz-dbt-rag      .
           move      spaces               to   w-let-dpz-dbt-via      .
           move      spaces               to   w-let-dpz-dbt-loc      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-dpz-dbt-999.
       let-dpz-dbt-200.
      *              *-------------------------------------------------*
      *              * Se tipo debitore : 01                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [dcc]                      *
      *                  *---------------------------------------------*
           if        w-let-dpz-dbt-dpz    =    spaces
                     move  "C"            to   w-let-arc-dcc-tle
           else      move  "D"            to   w-let-arc-dcc-tle      .
           move      w-let-dpz-dbt-cod    to   w-let-arc-dcc-cod      .
           move      w-let-dpz-dbt-dpz    to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                  *---------------------------------------------*
      *                  * Riporto risultati da lettura                *
      *                  *---------------------------------------------*
           move      w-let-arc-dcc-flg    to   w-let-dpz-dbt-flg      .
           move      w-let-arc-dcc-rag    to   w-let-dpz-dbt-rag      .
           move      w-let-arc-dcc-via    to   w-let-dpz-dbt-via      .
           move      w-let-arc-dcc-loc    to   w-let-dpz-dbt-loc      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-dpz-dbt-999.
       let-dpz-dbt-900.
      *              *-------------------------------------------------*
      *              * Se tipo debitore : non riconosciuto             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del codice debitore e  *
      *                  * della dipendenza                            *
      *                  *---------------------------------------------*
           if        w-let-dpz-dbt-cod    =    zero   and
                     w-let-dpz-dbt-dpz    =    spaces
                     go to let-dpz-dbt-920
           else      go to let-dpz-dbt-940.
       let-dpz-dbt-920.
      *                  *---------------------------------------------*
      *                  * Se codice debitore e dipendenza a zero ed a *
      *                  * spaces                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione work area               *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-dpz-dbt-rag      .
           move      spaces               to   w-let-dpz-dbt-via      .
           move      spaces               to   w-let-dpz-dbt-loc      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-dpz-dbt-999.
       let-dpz-dbt-940.
      *                  *---------------------------------------------*
      *                  * Se codice debitore e dipendenza diversi da  *
      *                  * zero e spaces                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Marker di uscita ad errore              *
      *                      *-----------------------------------------*
           move      "#"                  to   w-let-dpz-dbt-flg      .
           move      all   "."            to   w-let-dpz-dbt-rag      .
           move      spaces               to   w-let-dpz-dbt-via      .
           move      spaces               to   w-let-dpz-dbt-loc      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-dpz-dbt-999.
       let-dpz-dbt-999.
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
      *              * Test se codice cliente a zero                   *
      *              *-------------------------------------------------*
           if        w-let-arc-cli-cod    =    zero
                     go to let-arc-cli-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
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
           move      rf-cli-cod-cge       to   w-let-arc-cli-cge      .
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
           go to     let-arc-cli-600.
       let-arc-cli-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cli-rag      .
       let-arc-cli-600.
           move      spaces               to   w-let-arc-cli-via      .
           move      spaces               to   w-let-arc-cli-loc      .
           move      zero                 to   w-let-arc-cli-cge      .
       let-arc-cli-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio dipendenza cliente in [dcc]      *
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
      *              * Test se codice dipendenza a spaces, solo se ri- *
      *              * chiesta di lettura specificamente di una dipen- *
      *              * denza                                           *
      *              *-------------------------------------------------*
           if        w-let-arc-dcc-dpz    =    spaces and
                     w-let-arc-dcc-tle    =    "D"
                     go to let-arc-dcc-500.
      *              *-------------------------------------------------*
      *              * Test se codice dipendenza a "*   ", solo se ri- *
      *              * chiesta di lettura specificamente di una dipen- *
      *              * denza                                           *
      *              *-------------------------------------------------*
           if        w-let-arc-dcc-dpz    =    "*   " and
                     w-let-arc-dcc-tle    =    "D"
                     go to let-arc-dcc-450.
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
           move      rf-dcc-cod-abi       to   w-let-arc-dcc-abi      .
           move      rf-dcc-cod-cab       to   w-let-arc-dcc-cab      .
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
       let-arc-dcc-450.
      *              *-------------------------------------------------*
      *              * Normalizzazione per codice dipendenza "*   "    *
      *              *-------------------------------------------------*
           move      "Sia la sede che tutte le dipendenze     "
                                          to   w-let-arc-dcc-rag      .
           go to     let-arc-dcc-600.
       let-arc-dcc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcc-rag      .
       let-arc-dcc-600.
           move      spaces               to   w-let-arc-dcc-via      .
           move      spaces               to   w-let-arc-dcc-loc      .
           move      zero                 to   w-let-arc-dcc-abi      .
           move      zero                 to   w-let-arc-dcc-cab      .
       let-arc-dcc-999.
           exit.

      *    *===========================================================*
      *    * Espansione su archivio [sdb]                              *
      *    *-----------------------------------------------------------*
       esp-arc-sdb-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di selezione               *
      *              *-------------------------------------------------*
           move      "N"                  to   w-esp-arc-sdb-sel      .
       esp-arc-sdb-050.
      *              *-------------------------------------------------*
      *              * Se programma di espansione gia' attivo : uscita *
      *              * senza alcuna azione                             *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pgep3010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to esp-arc-sdb-999.
       esp-arc-sdb-100.
      *              *-------------------------------------------------*
      *              * Scrittura variabile di i.p.c. 'tip-int' per il  *
      *              * livello successivo per il tipo di interrogazio- *
      *              * ne                                              *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "tip-int"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      10                   to   s-car                  .
           move      "ESPSDB    "         to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       esp-arc-sdb-150.
      *              *-------------------------------------------------*
      *              * Scrittura variabile di i.p.c. 'num-sdb' per il  *
      *              * livello successivo per il numero scadenza su    *
      *              * cui eseguire l'espansione                       *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "num-sdb"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      11                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      w-esp-arc-sdb-num    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       esp-arc-sdb-200.
      *              *-------------------------------------------------*
      *              * Scrittura variabile di i.p.c. 'snx-slc' per il  *
      *              * livello successivo per l'ammissibilita' del ta- *
      *              * sto Slct                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non ammesso : no scrittura               *
      *                  *---------------------------------------------*
           if        w-esp-arc-sdb-sns    not  = "S"
                     go to esp-arc-sdb-250.
      *                  *---------------------------------------------*
      *                  * Scrittura effettiva                         *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "snx-slc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "S"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       esp-arc-sdb-250.
      *              *-------------------------------------------------*
      *              * Richiamo del programma di interrogazione        *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep3010"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
       esp-arc-sdb-300.
      *              *-------------------------------------------------*
      *              * Cancellazione del programma di interrogazione   *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       esp-arc-sdb-350.
      *              *-------------------------------------------------*
      *              * Determinazione selezione avvenuta               *
      *              *-------------------------------------------------*
       esp-arc-sdb-355.
      *                  *---------------------------------------------*
      *                  * Se non era ammessa la selezioone : uscita   *
      *                  *---------------------------------------------*
           if        w-esp-arc-sdb-sns    not  = "S"
                     go to esp-arc-sdb-999.
       esp-arc-sdb-360.
      *                  *---------------------------------------------*
      *                  * Lettura variabile di i.p.c. 'num-sdb' dal   *
      *                  * livello successivo per numero scadenza se-  *
      *                  * lezionato                                   *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "num-sdb"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito della let-  *
      *                  * tura                                        *
      *                  *---------------------------------------------*
           if        s-ves                =    spaces
                     go to esp-arc-sdb-365
           else      go to esp-arc-sdb-370.
       esp-arc-sdb-365.
      *                  *---------------------------------------------*
      *                  * Se variabile esistente                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di selezione avvenuta              *
      *                      *-----------------------------------------*
           move      spaces               to   w-esp-arc-sdb-sel      .
      *                      *-----------------------------------------*
      *                      * Numero scadenza selezionata             *
      *                      *-----------------------------------------*
           move      s-num                to   w-esp-arc-sdb-num      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     esp-arc-sdb-999.
       esp-arc-sdb-370.
      *                  *---------------------------------------------*
      *                  * Se variabile non esistente                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     esp-arc-sdb-999.
       esp-arc-sdb-999.
           exit.

      *    *===========================================================*
      *    * Caricamento iniziale scadenze per mezzo della selezione   *
      *    * sui parametri di selezione                                *
      *    *-----------------------------------------------------------*
       loa-sdb-sel-000.
      *              *-------------------------------------------------*
      *              * Preparazioni preliminari                        *
      *              *-------------------------------------------------*
       loa-sdb-sel-010.
      *                  *---------------------------------------------*
      *                  * Messaggio di caricamento in esecuzione      *
      *                  *---------------------------------------------*
           move      "                Selezione scadenze in esecuzione  
      -              "               "
                                          to   w-err-box-err-msg      .
           perform   msg-cnt-box-000      thru msg-cnt-box-999        .
       loa-sdb-sel-020.
      *                  *---------------------------------------------*
      *                  * Normalizzazione parametri in uscita         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag fondamentale di uscita a Ok        *
      *                      *-----------------------------------------*
           move      zero                 to   w-loa-sdb-out-flg      .
      *                      *-----------------------------------------*
      *                      * Numero di scadenze selezionate : a zero *
      *                      *-----------------------------------------*
           move      zero                 to   w-loa-sdb-out-nss      .
      *                      *-----------------------------------------*
      *                      * Importo globale selezionato : a zero    *
      *                      *-----------------------------------------*
           move      zero                 to   w-loa-sdb-out-igs      .
       loa-sdb-sel-500.
      *              *-------------------------------------------------*
      *              * Selezione scadenze                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su scadenze                           *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DTSNRS    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      zero                 to   rf-sdb-dts-sdb         .
           move      zero                 to   rf-sdb-num-sdb         .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *                  *---------------------------------------------*
      *                  * Se start errata si va' alla fine caricamen- *
      *                  * to                                          *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to loa-sdb-sel-800.
       loa-sdb-sel-550.
      *                  *---------------------------------------------*
      *                  * Next su scadenze                            *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *                  *---------------------------------------------*
      *                  * Se at end si va' alla fine caricamento sca- *
      *                  * denze                                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to loa-sdb-sel-800.
       loa-sdb-sel-575.
      *                  *---------------------------------------------*
      *                  * Max su scadenze                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se data scadenza massima da selezionare *
      *                      * a zero : test sempre superato           *
      *                      *-----------------------------------------*
           if        w-loa-sdb-inp-sma    =    zero
                     go to loa-sdb-sel-600.
      *                      *-----------------------------------------*
      *                      * Se data scadenza letta superiore alla   *
      *                      * data scadenza massima da selezionare :  *
      *                      * a fine caricamento scadenze             *
      *                      *-----------------------------------------*
           if        rf-sdb-dts-sdb       >    w-loa-sdb-inp-sma
                     go to loa-sdb-sel-800.
       loa-sdb-sel-600.
      *                  *---------------------------------------------*
      *                  * Selezioni su scadenza letta, se non supera- *
      *                  * te si va' a Next su scadenze                *
      *                  *---------------------------------------------*
       loa-sdb-sel-605.
      *                      *-----------------------------------------*
      *                      * Se data scadenza a vista : selezione su *
      *                      * data registrazione movimento di emis-   *
      *                      * sione                                   *
      *                      *-----------------------------------------*
           if        rf-sdb-dts-sdb       not  = zero
                     go to loa-sdb-sel-610.
           if        rf-sdb-dtr-emi       >    w-loa-sdb-inp-sma
                     go to loa-sdb-sel-550.
       loa-sdb-sel-610.
      *                      *-----------------------------------------*
      *                      * Controllo che la scadenza non abbia a-  *
      *                      * vuto movimenti precedenti a quello di   *
      *                      * inclusione in distinta di presentazione *
      *                      * che impediscano l'operazione in atto.   *
      *                      * Se anomalie : no selezione              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test su storno scadenza             *
      *                          *-------------------------------------*
           if        rf-sdb-dtr-sto       not  = zero
                     go to loa-sdb-sel-550.
      *                          *-------------------------------------*
      *                          * Test su riscossione scadenza        *
      *                          *-------------------------------------*
           if        rf-sdb-dtr-ris       not  = zero
                     go to loa-sdb-sel-550.
       rou-let-reg-615.
      *                      *-----------------------------------------*
      *                      * Controllo che la scadenza non abbia a-  *
      *                      * vuto movimenti successivi a quello di   *
      *                      * inclusione in distinta di presentazione *
      *                      * che impediscano l'operazione in atto.   *
      *                      * Se anomalie : no selezione              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test su richiamo scadenza           *
      *                          *-------------------------------------*
           if        rf-sdb-dtr-rsp       not  = zero
                     go to loa-sdb-sel-550.
      *                          *-------------------------------------*
      *                          * Test su insoluto                    *
      *                          *-------------------------------------*
           if        rf-sdb-dtr-isp       not  = zero
                     go to loa-sdb-sel-550.
      *                          *-------------------------------------*
      *                          * Test su accredito al dopo incasso   *
      *                          *-------------------------------------*
           if        rf-sdb-dtr-acs       not  = zero
                     go to loa-sdb-sel-550.
      *                          *-------------------------------------*
      *                          * Test su notizia di buon esito       *
      *                          *-------------------------------------*
           if        rf-sdb-dtr-nbe       not  = zero
                     go to loa-sdb-sel-550.
      *                          *-------------------------------------*
      *                          * Test su presunto buon esito         *
      *                          *-------------------------------------*
           if        rf-sdb-dtr-pbe       not  = zero
                     go to loa-sdb-sel-550.
       rou-let-reg-620.
      *                      *-----------------------------------------*
      *                      * Controllo flags di elaborazione bloc-   *
      *                      * canti                                   *
      *                      *-----------------------------------------*
           if        rf-sdb-flg-blo       not  = spaces
                     go to loa-sdb-sel-550.
       rou-let-reg-625.
      *                      *-----------------------------------------*
      *                      * Controllo flag di sottoponibilita' a    *
      *                      * pulizia                                 *
      *                      *-----------------------------------------*
           if        rf-sdb-flg-pul       not  = spaces
                     go to loa-sdb-sel-550.
       rou-let-reg-630.
      *                      *-----------------------------------------*
      *                      * Controllo che la scadenza sia gia' sta- *
      *                      * ta inclusa in una distinta di presenta- *
      *                      * zione                                   *
      *                      *-----------------------------------------*
           if        rf-sdb-num-ddp       =    zero
                     go to loa-sdb-sel-550.
       rou-let-reg-635.
      *                      *-----------------------------------------*
      *                      * Lettura della distinta di presentazione *
      *                      * in cui la scadenza e' stata inclusa; se *
      *                      * non esistente : no selezione            *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMDDP    "         to   f-key                  .
           move      rf-sdb-num-ddp       to   rf-ddp-num-ddp         .
           move      "pgm/gep/fls/ioc/obj/iofddp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ddp                 .
           if        f-sts                not  = e-not-err
                     go to loa-sdb-sel-550.
       rou-let-reg-640.
      *                      *-----------------------------------------*
      *                      * Se la distinta di presentazione non era *
      *                      * stata presentata : no selezione         *
      *                      *-----------------------------------------*
           if        rf-ddp-dtr-pre       =    zero
                     go to loa-sdb-sel-550.
      *                      *-----------------------------------------*
      *                      * Se la distinta di presentazione non era *
      *                      * stata presentata ne' al SBF, ne' allo   *
      *                      * Sconto : no selezione                   *
      *                      *-----------------------------------------*
           if        rf-ddp-tip-pre       not  = 01 and
                     rf-ddp-tip-pre       not  = 02 and
                     rf-ddp-tip-pre       not  = 04
                     go to loa-sdb-sel-550.
       rou-let-reg-645.
      *                      *-----------------------------------------*
      *                      * Se la distinta di presentazione non e'  *
      *                      * ancora stata accettata dalla banca : no *
      *                      * selezione                               *
      *                      *-----------------------------------------*
           if        rf-ddp-dtr-act       =    zero
                     go to loa-sdb-sel-550.
       rou-let-reg-650.
      *                      *-----------------------------------------*
      *                      * Selezione su tipo scadenza, che sia ri- *
      *                      * conosciuto                              *
      *                      *-----------------------------------------*
           if        rf-sdb-tip-sdb       not  = 01 and
                     rf-sdb-tip-sdb       not  = 02 and
                     rf-sdb-tip-sdb       not  = 03 and
                     rf-sdb-tip-sdb       not  = 04 and
                     rf-sdb-tip-sdb       not  = 05 and
                     rf-sdb-tip-sdb       not  = 06 and
                     rf-sdb-tip-sdb       not  = 07 and
                     rf-sdb-tip-sdb       not  = 08 and
                     rf-sdb-tip-sdb       not  = 09 and
                     rf-sdb-tip-sdb       not  = 10 and
                     rf-sdb-tip-sdb       not  = 11
                     go to loa-sdb-sel-550.
       rou-let-reg-655.
      *                      *-----------------------------------------*
      *                      * Selezione su tipo distinta, che sia ri- *
      *                      * conosciuto                              *
      *                      *-----------------------------------------*
           if        rf-ddp-tip-ddp       not  = 01 and
                     rf-ddp-tip-ddp       not  = 02 and
                     rf-ddp-tip-ddp       not  = 03 and
                     rf-ddp-tip-ddp       not  = 04
                     go to loa-sdb-sel-550.
       rou-let-reg-655.
      *                      *-----------------------------------------*
      *                      * Selezione su tipo avviso richiesto per  *
      *                      * la distinta , che sia riconosciuto      *
      *                      *-----------------------------------------*
           if        rf-ddp-tip-ddp       =    01
                     if     rf-ddp-tav-ric
                                          not  = 01 and
                            rf-ddp-tav-ric
                                          not  = 02 and
                            rf-ddp-tav-ric
                                          not  = 03 and
                            rf-ddp-tav-ric
                                          not  = 04 and
                            rf-ddp-tav-ric
                                          not  = 05
                            go to loa-sdb-sel-550.
       rou-let-reg-660.
      *                      *-----------------------------------------*
      *                      * Selezione sul tipo scadenza, che sia    *
      *                      * ammissibile per il tipo di distinta     *
      *                      *-----------------------------------------*
           if        rf-ddp-tip-ddp       =    01
                     if     rf-sdb-tip-sdb
                                          not  = 02 and
                            rf-sdb-tip-sdb
                                          not  = 03 and
                            rf-sdb-tip-sdb
                                          not  = 04 and
                            rf-sdb-tip-sdb
                                          not  = 05 and
                            rf-sdb-tip-sdb
                                          not  = 06
                            go to loa-sdb-sel-550.
           if        rf-ddp-tip-ddp       =    02
                     if     rf-sdb-tip-sdb
                                          not  = 09 and
                            rf-sdb-tip-sdb
                                          not  = 10
                            go to loa-sdb-sel-550.
           if        rf-ddp-tip-ddp       =    03
                     if     rf-sdb-tip-sdb
                                          not  = 11
                            go to loa-sdb-sel-550.
           if        rf-ddp-tip-ddp       =    04
                     if     rf-sdb-tip-sdb
                                          not  = 11
                            go to loa-sdb-sel-550.
       rou-let-reg-665.
      *                      *-----------------------------------------*
      *                      * Selezione sul tipo scadenza, che sia    *
      *                      * ammissibile per il tipo di avviso ri-   *
      *                      * chiesto                                 *
      *                      *-----------------------------------------*
           if        rf-ddp-tip-ddp       not  = 01
                     go to rou-let-reg-670.
           if        rf-ddp-tav-ric       =    01 or
                     rf-ddp-tav-ric       =    02 or
                     rf-ddp-tav-ric       =    03 or
                     rf-ddp-tav-ric       =    04
                     if     rf-sdb-tip-sdb
                                          not  = 02 and
                            rf-sdb-tip-sdb
                                          not  = 03 and
                            rf-sdb-tip-sdb
                                          not  = 04 and
                            rf-sdb-tip-sdb
                                          not  = 05
                            go to loa-sdb-sel-550.
           if        rf-ddp-tav-ric       =    05
                     if     rf-sdb-tip-sdb
                                          not  = 06
                            go to loa-sdb-sel-550.
       rou-let-reg-670.
      *                      *-----------------------------------------*
      *                      * Selezione sul tipo acquisizione scaden- *
      *                      * za, che sia ammissibile per il tipo di  *
      *                      * distinta                                *
      *                      *-----------------------------------------*
           if        rf-ddp-tip-ddp       =    03
                     if     rf-sdb-tac-sdb
                                          =    02
                            go to loa-sdb-sel-550.
           if        rf-ddp-tip-ddp       =    04
                     if     rf-sdb-tac-sdb
                                          not  = 02
                            go to loa-sdb-sel-550.
       rou-let-reg-675.
      *                      *-----------------------------------------*
      *                      * Selezione sul importo scadenza, che sia *
      *                      * superiore a zero                        *
      *                      *-----------------------------------------*
           if        rf-sdb-imp-sdb       not  > zero
                     go to loa-sdb-sel-550.
       loa-sdb-sel-700.
      *                  *---------------------------------------------*
      *                  * Selezioni sulla globalita' del caricamento  *
      *                  *---------------------------------------------*
       loa-sdb-sel-705.
      *                      *-----------------------------------------*
      *                      * Selezione su numero massimo di scadenze *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Simulazione numero di scadenze se-  *
      *                          * lezionate                           *
      *                          *-------------------------------------*
           move      w-loa-sdb-out-nss    to   w-loa-sdb-wrk-nss      .
           add       1                    to   w-loa-sdb-wrk-nss      .
      *                          *-------------------------------------*
      *                          * Se oltre il numero massimo di sca-  *
      *                          * denze massimo consentito : a fine   *
      *                          * caricamento                         *
      *                          *-------------------------------------*
           if        w-loa-sdb-wrk-nss    >    w-loa-sdb-inp-nsm
                     go to loa-sdb-sel-800.
       loa-sdb-sel-710.
      *                      *-----------------------------------------*
      *                      * Selezione su importo massimo per il     *
      *                      * caricamento                             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se importo massimo a zero : nessuna *
      *                          * selezione                           *
      *                          *-------------------------------------*
           if        w-loa-sdb-inp-igm    =    zero
                     go to loa-sdb-sel-715.
      *                          *-------------------------------------*
      *                          * Simulazione di accumulo per l'im-   *
      *                          * porto della scadenza letta          *
      *                          *-------------------------------------*
           move      w-loa-sdb-out-igs    to   w-loa-sdb-wrk-igs      .
           add       rf-sdb-imp-sdb       to   w-loa-sdb-wrk-igs      .
      *                          *-------------------------------------*
      *                          * Se oltre l'importo massimo da sele- *
      *                          * zionare : no selezione              *
      *                          *-------------------------------------*
           if        w-loa-sdb-wrk-igs    >    w-loa-sdb-inp-igm
                     go to loa-sdb-sel-550.
       loa-sdb-sel-715.
      *                          *-------------------------------------*
      *                          * Ad inclusione scadenza              *
      *                          *-------------------------------------*
           go to     loa-sdb-sel-750.
       loa-sdb-sel-750.
      *                  *---------------------------------------------*
      *                  * Inclusione scadenza nel caricamento         *
      *                  *---------------------------------------------*
       loa-sdb-sel-755.
      *                      *-----------------------------------------*
      *                      * Aggiornamento numero di scadenze sele-  *
      *                      * zionate                                 *
      *                      *-----------------------------------------*
           add       1                    to   w-loa-sdb-out-nss      .
       loa-sdb-sel-760.
      *                      *-----------------------------------------*
      *                      * Aggiornamento importo globale selezio-  *
      *                      * nato                                    *
      *                      *-----------------------------------------*
           add       rf-sdb-imp-sdb       to   w-loa-sdb-out-igs      .
       loa-sdb-sel-765.
      *                      *-----------------------------------------*
      *                      * Put su catena movimenti [rig]           *
      *                      *-----------------------------------------*
           perform   loa-sdb-pwr-000      thru loa-sdb-pwr-999        .
       loa-sdb-sel-770.
      *                      *-----------------------------------------*
      *                      * Riciclo a Next su scadenze              *
      *                      *-----------------------------------------*
           go to     loa-sdb-sel-550.
       loa-sdb-sel-800.
      *              *-------------------------------------------------*
      *              * Fine caricamento scadenze                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Cancellazione messaggio di caricamento in   *
      *                  * esecuzione                                  *
      *                  *---------------------------------------------*
           move      spaces               to   w-err-box-err-msg      .
           perform   msg-cnt-box-000      thru msg-cnt-box-999        .
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del numero di sca-   *
      *                  * denze selezionate                           *
      *                  *---------------------------------------------*
           if        w-loa-sdb-out-nss    =    zero
                     go to loa-sdb-sel-850
           else      go to loa-sdb-sel-900.
       loa-sdb-sel-850.
      *                  *---------------------------------------------*
      *                  * Se numero di scadenze selezionate pari a    *
      *                  * zero                                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag fondamentale di uscita a Ko        *
      *                      *-----------------------------------------*
           move      01                   to   w-loa-sdb-out-flg      .
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Nessuna scadenza da selezionare per il Presunto bu
      -              "on esito       "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     loa-sdb-sel-999.
       loa-sdb-sel-900.
      *                  *---------------------------------------------*
      *                  * Se numero di scadenze selezionate maggiore  *
      *                  * di zero                                     *
      *                  *---------------------------------------------*
       loa-sdb-sel-920.
      *                      *-----------------------------------------*
      *                      * Flag fondamentale di uscita a Ok        *
      *                      *-----------------------------------------*
           move      zero                 to   w-loa-sdb-out-flg      .
       loa-sdb-sel-940.
      *                      *-----------------------------------------*
      *                      * Eventuale messaggio di ulteriori sca-   *
      *                      * denze non incluse                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se sono state caricate tutte le     *
      *                          * scadenze possibili : no messaggio   *
      *                          *-------------------------------------*
           if        w-loa-sdb-out-nss    =    w-loa-sdb-wrk-nss
                     go to loa-sdb-sel-960.
      *                          *-------------------------------------*
      *                          * Altrimenti : emissione messaggio    *
      *                          *-------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "BX"                 to   v-ope                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      21                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Attenzione : Le scadenze che verranno visualizzate
      -              " non corrispondono alla   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "             totalita' delle scadenze sottoponibil
      -              "i all'operazione di Pre-  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "             sunto buon esito fino alla data scade
      -              "nza massima dichiarata.   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "             Pertanto sara' necessario poi ripeter
      -              "e questa operazione ri-   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "             dichiarando la stessa data scadenza m
      -              "assima.                   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      75                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      20                   to   v-lin                  .
           move      76                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       loa-sdb-sel-960.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     loa-sdb-sel-999.
       loa-sdb-sel-999.
           exit.

      *    *===========================================================*
      *    * Caricamento iniziale scadenze per mezzo della selezione   *
      *    * sui parametri impostati. Subroutine per la composizione   *
      *    * dell'area w-rig e la successiva immissione della stessa   *
      *    * nella catena di appoggio                                  *
      *    *-----------------------------------------------------------*
       loa-sdb-pwr-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione dati non chiave riga corpo      *
      *              *-------------------------------------------------*
           perform   nor-nok-rig-000      thru nor-nok-rig-999        .
      *              *-------------------------------------------------*
      *              * Duplicazione dati attuali su dati precedenti    *
      *              *-------------------------------------------------*
           move      w-rig-val-aep (1)    to   w-rig-val-aep (2)      .
      *              *-------------------------------------------------*
      *              * Preparazione area w-rig, indice 1, da scadenza  *
      *              * letta in area record [sdb], ad esclusione del   *
      *              * numero progressivo                              *
      *              *-------------------------------------------------*
           perform   pre-wri-sdb-000      thru pre-wri-sdb-999        .
      *              *-------------------------------------------------*
      *              * Append di un record vuoto a fine catena         *
      *              *-------------------------------------------------*
           move      "AP"                 to   w-cat-rig-ope          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
      *              *-------------------------------------------------*
      *              * Aggiornamento del numero progressivo            *
      *              *-------------------------------------------------*
           perform   prg-rig-cor-000      thru prg-rig-cor-999        .
      *              *-------------------------------------------------*
      *              * Update del record vuoto di fine catena          *
      *              *-------------------------------------------------*
           move      "UP"                 to   w-cat-rig-ope          .
           move      w-cat-rig-max        to   w-cat-rig-num          .
           move      w-rig                to   w-cat-rig-buf          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
       loa-sdb-pwr-999.
           exit.

      *    *===========================================================*
      *    * Preparazione area w-rig, indice 1, da scadenza letta in   *
      *    * area record [sdb], ad esclusione del numero progressivo   *
      *    *-----------------------------------------------------------*
       pre-wri-sdb-000.
      *              *-------------------------------------------------*
      *              * Valori presi direttamente dal record [sdb]      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero scadenza                             *
      *                  *---------------------------------------------*
           move      rf-sdb-num-sdb       to   w-rig-num-sdb (1)      .
      *                  *---------------------------------------------*
      *                  * Tipo scadenza                               *
      *                  *---------------------------------------------*
           move      rf-sdb-tip-sdb       to   w-rig-tip-sdb (1)      .
      *                  *---------------------------------------------*
      *                  * Data scadenza                               *
      *                  *---------------------------------------------*
           move      rf-sdb-dts-sdb       to   w-rig-dts-sdb (1)      .
      *                  *---------------------------------------------*
      *                  * Tipo debitore                               *
      *                  *---------------------------------------------*
           move      rf-sdb-tip-dbt       to   w-rig-tip-dbt (1)      .
      *                  *---------------------------------------------*
      *                  * Codice debitore                             *
      *                  *---------------------------------------------*
           move      rf-sdb-cod-dbt       to   w-rig-cod-dbt (1)      .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza del debitore              *
      *                  *---------------------------------------------*
           move      rf-sdb-dpz-dbt       to   w-rig-dpz-dbt (1)      .
      *                  *---------------------------------------------*
      *                  * Inoltro scadenza al debitore                *
      *                  *---------------------------------------------*
           move      rf-sdb-inl-sdb       to   w-rig-inl-sdb (1)      .
      *                  *---------------------------------------------*
      *                  * Importo scadenza                            *
      *                  *---------------------------------------------*
           move      rf-sdb-imp-sdb       to   w-rig-imp-sdb (1)      .
      *                  *---------------------------------------------*
      *                  * Data documento di riferimento               *
      *                  *---------------------------------------------*
           move      rf-sdb-dat-ddr       to   w-rig-dat-ddr (1)      .
      *                  *---------------------------------------------*
      *                  * Numero documento di riferimento             *
      *                  *---------------------------------------------*
           move      rf-sdb-num-ddr       to   w-rig-num-ddr (1)      .
       pre-wri-sdb-200.
      *              *-------------------------------------------------*
      *              * Valori ottenuti indirettamente dal record [sdb] *
      *              *-------------------------------------------------*
       pre-wri-sdb-225.
      *                  *---------------------------------------------*
      *                  * Codice debitore, sottoconto contabile e ra- *
      *                  * gione sociale                               *
      *                  *---------------------------------------------*
           move      w-rig-tip-dbt (1)    to   w-let-arc-dbt-tip      .
           move      w-rig-cod-dbt (1)    to   w-let-arc-dbt-cod      .
           perform   let-arc-dbt-000      thru let-arc-dbt-999        .
           move      w-let-arc-dbt-cge    to   w-rig-cod-dbt-cge (1)  .
           move      w-let-arc-dbt-rag    to   w-rig-cod-dbt-rag (1)  .
       pre-wri-sdb-250.
      *                  *---------------------------------------------*
      *                  * Codice dipendenza del debitore, ragione so- *
      *                  * ciale                                       *
      *                  *---------------------------------------------*
           move      w-rig-tip-dbt (1)    to   w-let-dpz-dbt-tip      .
           move      w-rig-cod-dbt (1)    to   w-let-dpz-dbt-cod      .
           move      w-rig-dpz-dbt (1)    to   w-let-dpz-dbt-dpz      .
           perform   let-dpz-dbt-000      thru let-dpz-dbt-999        .
           move      w-let-dpz-dbt-rag    to   w-rig-dpz-dbt-rag (1)  .
       pre-wri-sdb-400.
      *              *-------------------------------------------------*
      *              * Valori relativi agli aggiornamenti contabili    *
      *              *-------------------------------------------------*
       pre-wri-sdb-425.
      *                  *---------------------------------------------*
      *                  * Determinazione se la scadenza aveva pro-    *
      *                  * dotto oppure no un aggiornamento contabile  *
      *                  * al momento dell'emissione                   *
      *                  *---------------------------------------------*
           move      rf-sdb-tip-sdb       to   w-det-agg-emi-tsc      .
           move      rf-sdb-tac-sdb       to   w-det-agg-emi-tas      .
           move      rf-sdb-drc-emi       to   w-det-agg-emi-drc
           move      rf-sdb-npc-emi       to   w-det-agg-emi-npc
           perform   det-agg-emi-000      thru det-agg-emi-999        .
      *                  *---------------------------------------------*
      *                  * Indicatore di aggiornamento contabile ese-  *
      *                  * guito per la scadenza al momento dell'emis- *
      *                  * sione                                       *
      *                  *---------------------------------------------*
           move      w-det-agg-emi-snx    to   w-rig-agg-emi-snx (1)  .
      *                  *---------------------------------------------*
      *                  * Codice contropartita contabile in caso di   *
      *                  * aggiornamento non eseguito in fase di emis- *
      *                  * sione                                       *
      *                  *---------------------------------------------*
           move      w-det-agg-emi-ctp    to   w-rig-agg-emi-ctp (1)  .
       pre-wri-sdb-450.
      *                  *---------------------------------------------*
      *                  * Determinazione se la distinta nella quale   *
      *                  * la scadenza e' stata inclusa aveva prodot-  *
      *                  * to oppure no un aggiornamento contabile al  *
      *                  * momento della presentazione                 *
      *                  *---------------------------------------------*
           move      rf-ddp-tip-pre       to   w-det-agg-pre-tdp      .
           perform   det-agg-pre-000      thru det-agg-pre-999        .
      *                  *---------------------------------------------*
      *                  * Indicatore di aggiornamento contabile ese-  *
      *                  * guito per la scadenza al momento della      *
      *                  * presentazione della distinta                *
      *                  *---------------------------------------------*
           move      w-det-agg-pre-snx    to   w-rig-agg-pre-snx (1)  .
       pre-wri-sdb-500.
      *                  *---------------------------------------------*
      *                  * Indicatore di aggiornamento contabile da e- *
      *                  * seguire o no a fronte della scadenza        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se le personalizzazioni indicano che in *
      *                      * ogni caso gli aggiornamenti contabili   *
      *                      * non devono essere eseguiti : 'N'        *
      *                      *-----------------------------------------*
           if        w-prs-gep-snx-cge    =    "N"
                     move  "N"            to   w-rig-snx-agg-cge (1)
                     go to pre-wri-sdb-550.
      *                      *-----------------------------------------*
      *                      * Se il codice causale contabile per il   *
      *                      * tipo operazione di presunto buon esito  *
      *                      * per scadenze presentate al SBF o allo   *
      *                      * sconto e' a zero  : 'N'                 *
      *                      *-----------------------------------------*
           if        w-top-cau-cge
                    (w-det-inx-pbe-top)   =    zero
                     move  "N"            to   w-rig-snx-agg-cge (1)
                     go to pre-wri-sdb-550.
      *                      *-----------------------------------------*
      *                      * Se l'operazione di presentazione di-    *
      *                      * stinta non aveva prodotto un aggior-    *
      *                      * namento contabile : 'N'                 *
      *                      *-----------------------------------------*
           if        w-rig-agg-pre-snx (1)
                                          not  = "S"
                     move  "N"            to   w-rig-snx-agg-cge (1)
                     go to pre-wri-sdb-550.
      *                      *-----------------------------------------*
      *                      * Se l'importo scadenza e' a zero : 'N'   *
      *                      *-----------------------------------------*
           if        w-rig-imp-sdb (1)    =    zero
                     move  "N"            to   w-rig-snx-agg-cge (1)
                     go to pre-wri-sdb-550.
      *                      *-----------------------------------------*
      *                      * Altrimenti : 'S'                        *
      *                      *-----------------------------------------*
           move      "S"                  to   w-rig-snx-agg-cge (1)  .
       pre-wri-sdb-550.
      *                  *---------------------------------------------*
      *                  * Indicatore di errori sulla scadenza         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se indicatore di aggiornamento contabi- *
      *                      * le eseguito per la scadenza al momento  *
      *                      * dell'emissione al valore '?' : errore   *
      *                      *-----------------------------------------*
           if        w-rig-agg-emi-snx (1)
                                          =    "?"
                     move  "#"            to   w-rig-flg-err-sdb (1)
                     go to pre-wri-sdb-600.
      *                      *-----------------------------------------*
      *                      * Se indicatore di aggiornamento contabi- *
      *                      * le eseguito per la scadenza al momento  *
      *                      * della presentazione della distinta al   *
      *                      * valore '?' : errore                     *
      *                      *-----------------------------------------*
           if        w-rig-agg-pre-snx (1)
                                          =    "?"
                     move  "#"            to   w-rig-flg-err-sdb (1)
                     go to pre-wri-sdb-600.
      *                      *-----------------------------------------*
      *                      * Se indicatore di aggiornamento contabi- *
      *                      * le da eseguire sulla scadenza a fronte  *
      *                      * del Presunto buon esito a 'S', ma man-  *
      *                      * ca il sottoconto contabile per il cli-  *
      *                      * ente : errore                           *
      *                      *-----------------------------------------*
           if        w-rig-agg-pre-snx (1)
                                          =    "S"  and
                     w-rig-cod-dbt-cge (1)
                                          =    zero
                     move  "#"            to   w-rig-flg-err-sdb (1)
                     go to pre-wri-sdb-600.
       pre-wri-sdb-600.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pre-wri-sdb-999.
       pre-wri-sdb-999.
           exit.

      *    *===========================================================*
      *    * Determinazione se la scadenza trattata aveva prodotto op- *
      *    * pure no un aggiornamento contabile al momento dell'emis-  *
      *    * sione                                                     *
      *    *-----------------------------------------------------------*
       det-agg-emi-000.
      *              *-------------------------------------------------*
      *              * Determinazione del tipo operazione di emissione *
      *              * corrispondente al tipo scadenza su cui si sta   *
      *              * operando                                        *
      *              *-------------------------------------------------*
           if        w-det-agg-emi-tsc    =    01
                     move  0101           to   w-det-agg-emi-woe
           else if   w-det-agg-emi-tsc    =    02
                     move  0102           to   w-det-agg-emi-woe
           else if   w-det-agg-emi-tsc    =    03
                     move  0103           to   w-det-agg-emi-woe
           else if   w-det-agg-emi-tsc    =    04
                     move  0104           to   w-det-agg-emi-woe
           else if   w-det-agg-emi-tsc    =    05
                     move  0105           to   w-det-agg-emi-woe
           else if   w-det-agg-emi-tsc    =    06
                     move  0106           to   w-det-agg-emi-woe
           else if   w-det-agg-emi-tsc    =    07
                     move  0107           to   w-det-agg-emi-woe
           else if   w-det-agg-emi-tsc    =    08
                     move  0108           to   w-det-agg-emi-woe
           else if   w-det-agg-emi-tsc    =    09
                     move  0109           to   w-det-agg-emi-woe
           else if   w-det-agg-emi-tsc    =    10
                     move  0110           to   w-det-agg-emi-woe
           else if   w-det-agg-emi-tsc    =    11
                     move  0111           to   w-det-agg-emi-woe
           else      move  zero           to   w-det-agg-emi-woe      .
      *              *-------------------------------------------------*
      *              * Se tipo operazione non determinabile si esce    *
      *              * con un punto interrogativo e con contropartita  *
      *              * a zero                                          *
      *              *-------------------------------------------------*
           if        w-det-agg-emi-woe    =    zero
                     move  "?"            to   w-det-agg-emi-snx
                     move  zero           to   w-det-agg-emi-ctp
                     go to det-agg-emi-999.
       det-agg-emi-100.
      *              *-------------------------------------------------*
      *              * Se tipo operazione 0111, e tipo acquisizione    *
      *              * scadenza per cessione da terzi, si porta il     *
      *              * tipo operazione a 0161                          *
      *              *-------------------------------------------------*
           if        w-det-agg-emi-woe    =    0111 and
                     w-det-agg-emi-tas    =    02
                     move  0161           to   w-det-agg-emi-woe      .
       det-agg-emi-200.
      *              *-------------------------------------------------*
      *              * Determinazione indice su tabella tipi operazio- *
      *              * ne, relativo al tipo operazione di emissione    *
      *              * corrispondente al tipo scadenza da trattare.    *
      *              * Se indice non determinabile si esce con un pun- *
      *              * to interrogativo e con contropartita a zero     *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-agg-emi-inx      .
       det-agg-emi-225.
           add       1                    to   w-det-agg-emi-inx      .
           if        w-det-agg-emi-inx    >    w-top-ele-num
                     move  "?"            to   w-det-agg-emi-snx
                     move  zero           to   w-det-agg-emi-ctp
                     go to det-agg-emi-999.
           if        w-top-cod-top
                    (w-det-agg-emi-inx)   not  = w-det-agg-emi-woe
                     go to det-agg-emi-225.
       det-agg-emi-300.
      *              *-------------------------------------------------*
      *              * Se data registrazione e numero protocollo per   *
      *              * contabilita' esistenti, si salta il test sul    *
      *              * codice causale contabile per l'emissione        *
      *              *-------------------------------------------------*
           if        w-det-agg-emi-drc    not  = zero and
                     w-det-agg-emi-npc    not  = zero
                     go to det-agg-emi-350.
       det-agg-emi-325.
      *              *-------------------------------------------------*
      *              * Se il codice causale contabile per il tipo ope- *
      *              * razione di emissione e' a zero, si esce con 'N' *
      *              * e con il codice della contropartita             *
      *              *-------------------------------------------------*
           if        w-top-cau-cge
                    (w-det-agg-emi-inx)   =    zero
                     move  "N"            to   w-det-agg-emi-snx
                     move  w-top-ctp-cge
                          (w-det-agg-emi-inx)
                                          to   w-det-agg-emi-ctp
                     go to det-agg-emi-999.
       det-agg-emi-350.
      *              *-------------------------------------------------*
      *              * Se il codice sottoconto contabile per il tipo   *
      *              * operazione di emissione e' a zero, si esce con  *
      *              * 'N' e con il codice della contropartita         *
      *              *-------------------------------------------------*
           if        w-top-stc-cge
                    (w-det-agg-emi-inx)   =    zero
                     move  "N"            to   w-det-agg-emi-snx
                     move  w-top-ctp-cge
                          (w-det-agg-emi-inx)
                                          to   w-det-agg-emi-ctp
                     go to det-agg-emi-999.
       det-agg-emi-400.
      *              *-------------------------------------------------*
      *              * Altrimenti : si esce con 'S' e con controparti- *
      *              * ta a zero                                       *
      *              *-------------------------------------------------*
           move      "S"                  to   w-det-agg-emi-snx      .
           move      zero                 to   w-det-agg-emi-ctp      .
       det-agg-emi-999.
           exit.

      *    *===========================================================*
      *    * Determinazione se la distinta trattata aveva prodotto op- *
      *    * pure no un aggiornamento contabile al momento della pre-  *
      *    * sentazione                                                *
      *    *-----------------------------------------------------------*
       det-agg-pre-000.
      *              *-------------------------------------------------*
      *              * Determinazione del tipo operazione corrispon-   *
      *              * dente al tipo di presentazione su cui si sta    *
      *              * operando                                        *
      *              *-------------------------------------------------*
           if        w-det-agg-pre-tdp    =    01
                     move  0501           to   w-det-agg-pre-wop
           else if   w-det-agg-pre-tdp    =    02
                     move  0502           to   w-det-agg-pre-wop
           else if   w-det-agg-pre-tdp    =    03
                     move  0503           to   w-det-agg-pre-wop
           else if   w-det-agg-pre-tdp    =    04
                     move  0504           to   w-det-agg-pre-wop
           else      move  zero           to   w-det-agg-pre-wop      .
      *              *-------------------------------------------------*
      *              * Se tipo operazione non determinabile si esce    *
      *              * con un punto interrogativo                      *
      *              *-------------------------------------------------*
           if        w-det-agg-pre-wop    =    zero
                     move  "?"            to   w-det-agg-pre-snx
                     go to det-agg-pre-999.
       det-agg-pre-200.
      *              *-------------------------------------------------*
      *              * Determinazione indice su tabella tipi operazio- *
      *              * ne, relativo al tipo operazione di presentazio  *
      *              * corrispondente al tipo distinta da trattare.    *
      *              * Se indice non determinabile si esce con un pun- *
      *              * to interrogativo                                *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-agg-pre-inx      .
       det-agg-pre-225.
           add       1                    to   w-det-agg-pre-inx      .
           if        w-det-agg-pre-inx    >    w-top-ele-num
                     move  "?"            to   w-det-agg-pre-snx
                     go to det-agg-pre-999.
           if        w-top-cod-top
                    (w-det-agg-pre-inx)   not  = w-det-agg-pre-wop
                     go to det-agg-pre-225.
       det-agg-pre-300.
      *              *-------------------------------------------------*
      *              * Se il codice della causale contabile per il ti- *
      *              * po operazione di presentazione che si sta trat- *
      *              * tando e' diverso da zero si esce con 'S', al-   *
      *              * trimenti si esce con 'N'                        *
      *              *-------------------------------------------------*
           if        w-top-cau-cge
                    (w-det-agg-pre-inx)   not  = zero
                     move  "S"            to   w-det-agg-pre-snx
           else      move  "N"            to   w-det-agg-pre-snx      .
       det-agg-pre-999.
           exit.

      *    *===========================================================*
      *    * Aggiornamenti contabili generati dalla gestione  portafo- *
      *    * glio a fronte del movimento di presunto buon esito per    *
      *    * scadenze presentate al SBF o allo sconto                  *
      *    *                                                           *
      *    * Nota : Vedere i commenti per il modulo 'pgep3000' per le  *
      *    *        generalita' sugli aggiornamenti contabili riguar-  *
      *    *        danti la gestione portafoglio.                     *
      *    *-----------------------------------------------------------*
       cge-gep-pbe-000.
      *              *-------------------------------------------------*
      *              * Test se aggiornamento da eseguire               *
      *              *-------------------------------------------------*
           if        w-rig-snx-agg-cge (1)
                                          =    "S"
                     go to cge-gep-pbe-200.
       cge-gep-pbe-100.
      *              *-------------------------------------------------*
      *              * Se No aggiornamento                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data registrazione movimento contabile : a  *
      *                  * zero                                        *
      *                  *---------------------------------------------*
           move      zero                 to   l-cge-300-dat-reg      .
      *                  *---------------------------------------------*
      *                  * Numero protocollo movimento contabile : a   *
      *                  * zero                                        *
      *                  *---------------------------------------------*
           move      zero                 to   l-cge-300-num-prt      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cge-gep-pbe-999.
       cge-gep-pbe-200.
      *              *-------------------------------------------------*
      *              * Se Si' aggiornamento                            *
      *              *-------------------------------------------------*
       cge-gep-pbe-300.
      *                  *---------------------------------------------*
      *                  * Preparazione area per aggiornamento         *
      *                  *---------------------------------------------*
       cge-gep-pbe-325.
      *                      *-----------------------------------------*
      *                      * Causale contabile che deve essere uti-  *
      *                      * lizzata                                 *
      *                      *-----------------------------------------*
           move      w-top-cau-cge
                    (w-det-inx-pbe-top)   to   w-mov-cge-cau-cge      .
       cge-gep-pbe-350.
      *                      *-----------------------------------------*
      *                      * Preparazione righe per la registraiozne *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Contatore numero di righe della re- *
      *                          * gistrazione : a zero                *
      *                          *-------------------------------------*
           move      zero                 to   w-mov-cge-ctr-rig      .
      *                          *-------------------------------------*
      *                          * Deviazione a seconda se la scadenza *
      *                          * ha prodotto oppure no un aggiorna-  *
      *                          * mento contabile al momento della e- *
      *                          * missione                            *
      *                          *-------------------------------------*
           if        w-rig-agg-emi-snx (1)
                                          =    "S"
                     go to cge-gep-pbe-375
           else      go to cge-gep-pbe-400.
       cge-gep-pbe-375.
      *                          *-------------------------------------*
      *                          * Se la scadenza ha prodotto un ag-   *
      *                          * giornamento contabile al momento    *
      *                          * della emissione                     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * A : no aggiornamento            *
      *                              *---------------------------------*
           go to     cge-gep-pbe-100.
       cge-gep-pbe-400.
      *                          *-------------------------------------*
      *                          * Se la scadenza non ha prodotto un   *
      *                          * aggiornamento contabile al momento  *
      *                          * della emissione                     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Contropartita contabile per la  *
      *                              * presentazione distinta associa- *
      *                              * ta al tipo movimento di emis-   *
      *                              * sione della scadenza trattata,  *
      *                              * che rappresenta gli effetti in  *
      *                              * circolazione, in Dare per l'im- *
      *                              * porto scadenza                  *
      *                              *---------------------------------*
           move      "G"                  to   w-mov-cge-acc-tip      .
           move      zero                 to   w-mov-cge-acc-arc      .
           move      w-rig-agg-emi-ctp (1)
                                          to   w-mov-cge-acc-stc      .
           move      "D"                  to   w-mov-cge-acc-dav      .
           move      w-rig-imp-sdb (1)    to   w-mov-cge-acc-imp      .
           perform   cge-gep-acr-000      thru cge-gep-acr-999        .
      *                                  *-----------------------------*
      *                                  * Cliente in Avere per l'im-  *
      *                                  * porto scadenza              *
      *                                  *-----------------------------*
           move      "C"                  to   w-mov-cge-acc-tip      .
           move      w-rig-cod-dbt (1)    to   w-mov-cge-acc-arc      .
           move      w-rig-cod-dbt-cge (1)
                                          to   w-mov-cge-acc-stc      .
           move      "A"                  to   w-mov-cge-acc-dav      .
           move      w-rig-imp-sdb (1)    to   w-mov-cge-acc-imp      .
           perform   cge-gep-acr-000      thru cge-gep-acr-999        .
      *                              *---------------------------------*
      *                              * Ad esecuzione aggiornamenti     *
      *                              *---------------------------------*
           go to     cge-gep-pbe-600.
       cge-gep-pbe-600.
      *                  *---------------------------------------------*
      *                  * Esecuzione effettiva aggiornamenti da area  *
      *                  * per aggiornamento preparata                 *
      *                  *---------------------------------------------*
       cge-gep-pbe-625.
      *                      *-----------------------------------------*
      *                      * Se zero elementi nella tabella : a no   *
      *                      * aggiornamento                           *
      *                      *-----------------------------------------*
           if        w-mov-cge-ctr-rig    =    zero
                     go to cge-gep-pbe-100.
       cge-gep-pbe-650.
      *                      *-----------------------------------------*
      *                      * Se gli elementi della tabella hanno     *
      *                      * tutti l'importo a zero : a no aggior-   *
      *                      * namento                                 *
      *                      *-----------------------------------------*
           move      zero                 to   w-mov-cge-inx-001      .
       cge-gep-pbe-655.
           add       1                    to   w-mov-cge-inx-001      .
           if        w-mov-cge-inx-001    >    w-mov-cge-ctr-rig
                     go to cge-gep-pbe-100.
           if        w-mov-cge-imp-rig
                    (w-mov-cge-inx-001)   =    zero
                     go to cge-gep-pbe-655.
       cge-gep-pbe-675.
      *                      *-----------------------------------------*
      *                      * Ricerca della prima riga in Dare, se    *
      *                      * non trovata : a no aggiornamento        *
      *                      *-----------------------------------------*
           move      zero                 to   w-mov-cge-inx-rig      .
       cge-gep-pbe-680.
           add       1                    to   w-mov-cge-inx-rig      .
           if        w-mov-cge-inx-rig    >    w-mov-cge-ctr-rig
                     go to cge-gep-pbe-100.
           if        w-mov-cge-imp-rig
                    (w-mov-cge-inx-rig)   not  > zero
                     go to cge-gep-pbe-680.
       cge-gep-pbe-700.
      *                      *-----------------------------------------*
      *                      * Normalizzazione work-area testata e     *
      *                      * prima riga movimento per aggiornamenti  *
      *                      * contabili                               *
      *                      *-----------------------------------------*
           move      "NO"                 to   l-cge-300-tip-ope      .
           perform   mdl-agg-cge-cll-000  thru mdl-agg-cge-cll-999    .
       cge-gep-pbe-725.
      *                      *-----------------------------------------*
      *                      * Preparazione link-area per testata e    *
      *                      * prima riga movimento in Dare            *
      *                      *                                         *
      *                      * Nota : i seguenti campi non vengono     *
      *                      *        preparati in quanto gia' nor-    *
      *                      *        malizzati :                      *
      *                      *                                         *
      *                      *        - Numero protocollo              *
      *                      *        - Descrizione per codice causale *
      *                      *        - Codice numerazione             *
      *                      *        - Numero protocollo Iva          *
      *                      *        - Commento in riga               *
      *                      *        - Castelletto Iva                *
      *                      *        - Totale documento Iva           *
      *                      *                                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Tipo operazione                     *
      *                          *-------------------------------------*
           move      "WR"                 to   l-cge-300-tip-ope      .
      *                          *-------------------------------------*
      *                          * Data di registrazione               *
      *                          *-------------------------------------*
           move      w-tes-dat-reg        to   l-cge-300-dat-reg      .
      *                          *-------------------------------------*
      *                          * Codice causale contabile            *
      *                          *-------------------------------------*
           move      w-mov-cge-cau-cge    to   l-cge-300-cod-cau      .
      *                          *-------------------------------------*
      *                          * Descrizione causale                 *
      *                          *-------------------------------------*
           move      spaces               to   l-cge-300-des-cau      .
      *                          *-------------------------------------*
      *                          * Si/No data scadenza                 *
      *                          *-------------------------------------*
           move      "N"                  to   l-cge-300-snx-dts      .
      *                          *-------------------------------------*
      *                          * Data scadenza                       *
      *                          *-------------------------------------*
           move      zero                 to   l-cge-300-dat-sca      .
      *                          *-------------------------------------*
      *                          * Data documento                      *
      *                          *-------------------------------------*
           move      zero                 to   l-cge-300-dat-doc      .
      *                          *-------------------------------------*
      *                          * Numero documento                    *
      *                          *-------------------------------------*
           move      spaces               to   l-cge-300-num-doc      .
      *                          *-------------------------------------*
      *                          * Codice sottoconto                   *
      *                          *-------------------------------------*
           move      w-mov-cge-cod-stc
                    (w-mov-cge-inx-rig)   to   l-cge-300-cod-pdc      .
      *                          *-------------------------------------*
      *                          * Tipo archivio                       *
      *                          *-------------------------------------*
           move      w-mov-cge-tip-arc
                    (w-mov-cge-inx-rig)   to   l-cge-300-tip-arc      .
      *                          *-------------------------------------*
      *                          * Codice archivio                     *
      *                          *-------------------------------------*
           move      w-mov-cge-cod-arc
                    (w-mov-cge-inx-rig)   to   l-cge-300-cod-arc      .
      *                          *-------------------------------------*
      *                          * Data riferimento                    *
      *                          *-------------------------------------*
           if        w-mov-cge-tip-arc
                    (w-mov-cge-inx-rig)   =    "C"
                     move  w-rig-dat-ddr (1)
                                          to   l-cge-300-dat-rif
           else      move  zero           to   l-cge-300-dat-rif      .
      *                          *-------------------------------------*
      *                          * Numero riferimento                  *
      *                          *-------------------------------------*
           if        w-mov-cge-tip-arc
                    (w-mov-cge-inx-rig)   =    "C"
                     move  w-rig-num-ddr (1)
                                          to   l-cge-300-num-rif
           else      move  spaces         to   l-cge-300-num-rif      .
      *                          *-------------------------------------*
      *                          * Dare/Avere                          *
      *                          *-------------------------------------*
           move      "D"                  to   l-cge-300-dar-ave      .
      *                          *-------------------------------------*
      *                          * Importo movimento                   *
      *                          *-------------------------------------*
           move      w-mov-cge-imp-rig
                    (w-mov-cge-inx-rig)   to   l-cge-300-imp-mov      .
      *                      *-----------------------------------------*
      *                      * Richiamo sottoprogramma per aggiorna-   *
      *                      * menti contabili                         *
      *                      *-----------------------------------------*
           perform   mdl-agg-cge-cll-000  thru mdl-agg-cge-cll-999    .
       cge-gep-pbe-750.
      *                      *-----------------------------------------*
      *                      * Altre righe in Dare                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Ricerca della prossima riga in Da-  *
      *                          * re; se non trovata : a righe in A-  *
      *                          * vere                                *
      *                          *-------------------------------------*
           add       1                    to   w-mov-cge-inx-rig      .
           if        w-mov-cge-inx-rig    >    w-mov-cge-ctr-rig
                     go to cge-gep-pbe-775.
           if        w-mov-cge-imp-rig
                    (w-mov-cge-inx-rig)   not  > zero
                     go to cge-gep-pbe-750.
      *                          *-------------------------------------*
      *                          * Normalizzazione work-area riga mo-  *
      *                          * vimento per aggiornamenti contabili *
      *                          *-------------------------------------*
           move      "N+"                 to   l-cge-300-tip-ope      .
           perform   mdl-agg-cge-cll-000  thru mdl-agg-cge-cll-999    .
      *                          *-------------------------------------*
      *                          * Preparazione link-area per riga mo- *
      *                          * vimento successiva alla prima       *
      *                          *                                     *
      *                          * Nota : i seguenti campi non vengono *
      *                          *        preparati in quanto gia'     *
      *                          *        normalizzati                 *
      *                          *                                     *
      *                          *        - Commento in riga           *
      *                          *                                     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Tipo operazione                 *
      *                              *---------------------------------*
           move      "W+"                 to   l-cge-300-tip-ope      .
      *                              *---------------------------------*
      *                              * Codice sottoconto               *
      *                              *---------------------------------*
           move      w-mov-cge-cod-stc
                    (w-mov-cge-inx-rig)   to   l-cge-300-cod-pdc      .
      *                              *---------------------------------*
      *                              * Tipo archivio                   *
      *                              *---------------------------------*
           move      w-mov-cge-tip-arc
                    (w-mov-cge-inx-rig)   to   l-cge-300-tip-arc      .
      *                              *---------------------------------*
      *                              * Codice archivio                 *
      *                              *---------------------------------*
           move      w-mov-cge-cod-arc
                    (w-mov-cge-inx-rig)   to   l-cge-300-cod-arc      .
      *                              *---------------------------------*
      *                              * Data riferimento                *
      *                              *---------------------------------*
           if        w-mov-cge-tip-arc
                    (w-mov-cge-inx-rig)   =    "C"
                     move  w-rig-dat-ddr (1)
                                          to   l-cge-300-dat-rif
           else      move  zero           to   l-cge-300-dat-rif      .
      *                              *---------------------------------*
      *                              * Numero riferimento              *
      *                              *---------------------------------*
           if        w-mov-cge-tip-arc
                    (w-mov-cge-inx-rig)   =    "C"
                     move  w-rig-num-ddr (1)
                                          to   l-cge-300-num-rif
           else      move  spaces         to   l-cge-300-num-rif      .
      *                              *---------------------------------*
      *                              * Dare/Avere                      *
      *                              *---------------------------------*
           move      "D"                  to   l-cge-300-dar-ave      .
      *                              *---------------------------------*
      *                              * Importo movimento               *
      *                              *---------------------------------*
           move      w-mov-cge-imp-rig
                    (w-mov-cge-inx-rig)   to   l-cge-300-imp-mov      .
      *                          *-------------------------------------*
      *                          * Richiamo sottoprogramma per aggior- *
      *                          * namenti contabili                   *
      *                          *-------------------------------------*
           perform   mdl-agg-cge-cll-000  thru mdl-agg-cge-cll-999    .
      *                          *-------------------------------------*
      *                          * Riciclo a riga in Dare successiva   *
      *                          *-------------------------------------*
           go to     cge-gep-pbe-750.
       cge-gep-pbe-775.
      *                      *-----------------------------------------*
      *                      * Righe in Avere                          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Re-inizializzazione indice per      *
      *                          * scansione sulle righe               *
      *                          *-------------------------------------*
           move      zero                 to   w-mov-cge-inx-rig      .
       cge-gep-pbe-800.
      *                          *-------------------------------------*
      *                          * Ricerca della prossima riga in A-   *
      *                          * vere; se non trovata : fine aggior- *
      *                          * namento                             *
      *                          *-------------------------------------*
           add       1                    to   w-mov-cge-inx-rig      .
           if        w-mov-cge-inx-rig    >    w-mov-cge-ctr-rig
                     go to cge-gep-pbe-900.
           if        w-mov-cge-imp-rig
                    (w-mov-cge-inx-rig)   not  < zero
                     go to cge-gep-pbe-800.
      *                          *-------------------------------------*
      *                          * Normalizzazione work-area riga mo-  *
      *                          * vimento per aggiornamenti contabili *
      *                          *-------------------------------------*
           move      "N+"                 to   l-cge-300-tip-ope      .
           perform   mdl-agg-cge-cll-000  thru mdl-agg-cge-cll-999    .
      *                          *-------------------------------------*
      *                          * Preparazione link-area per riga mo- *
      *                          * vimento successiva alla prima       *
      *                          *                                     *
      *                          * Nota : i seguenti campi non vengono *
      *                          *        preparati in quanto gia'     *
      *                          *        normalizzati                 *
      *                          *                                     *
      *                          *        - Commento in riga           *
      *                          *                                     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Tipo operazione                 *
      *                              *---------------------------------*
           move      "W+"                 to   l-cge-300-tip-ope      .
      *                              *---------------------------------*
      *                              * Codice sottoconto               *
      *                              *---------------------------------*
           move      w-mov-cge-cod-stc
                    (w-mov-cge-inx-rig)   to   l-cge-300-cod-pdc      .
      *                              *---------------------------------*
      *                              * Tipo archivio                   *
      *                              *---------------------------------*
           move      w-mov-cge-tip-arc
                    (w-mov-cge-inx-rig)   to   l-cge-300-tip-arc      .
      *                              *---------------------------------*
      *                              * Codice archivio                 *
      *                              *---------------------------------*
           move      w-mov-cge-cod-arc
                    (w-mov-cge-inx-rig)   to   l-cge-300-cod-arc      .
      *                              *---------------------------------*
      *                              * Data riferimento                *
      *                              *---------------------------------*
           if        w-mov-cge-tip-arc
                    (w-mov-cge-inx-rig)   =    "C"
                     move  w-rig-dat-ddr (1)
                                          to   l-cge-300-dat-rif
           else      move  zero           to   l-cge-300-dat-rif      .
      *                              *---------------------------------*
      *                              * Numero riferimento              *
      *                              *---------------------------------*
           if        w-mov-cge-tip-arc
                    (w-mov-cge-inx-rig)   =    "C"
                     move  w-rig-num-ddr (1)
                                          to   l-cge-300-num-rif
           else      move  spaces         to   l-cge-300-num-rif      .
      *                              *---------------------------------*
      *                              * Dare/Avere                      *
      *                              *---------------------------------*
           move      "A"                  to   l-cge-300-dar-ave      .
      *                              *---------------------------------*
      *                              * Importo movimento               *
      *                              *---------------------------------*
           move      w-mov-cge-imp-rig
                    (w-mov-cge-inx-rig)   to   l-cge-300-imp-mov      .
           multiply  -1                   by   l-cge-300-imp-mov      .
      *                          *-------------------------------------*
      *                          * Richiamo sottoprogramma per aggior- *
      *                          * namenti contabili                   *
      *                          *-------------------------------------*
           perform   mdl-agg-cge-cll-000  thru mdl-agg-cge-cll-999    .
      *                          *-------------------------------------*
      *                          * Riciclo a riga in Avere successiva  *
      *                          *-------------------------------------*
           go to     cge-gep-pbe-800.
       cge-gep-pbe-900.
      *                  *---------------------------------------------*
      *                  * Uscita dopo aggiornamenti                   *
      *                  *---------------------------------------------*
           go to     cge-gep-pbe-999.
       cge-gep-pbe-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per l'accumulo delle righe per gli aggiorna-   *
      *    * menti contabili generati dalla gestione portafoglio       *
      *    *-----------------------------------------------------------*
       cge-gep-acr-000.
      *              *-------------------------------------------------*
      *              * Se importo a zero : uscita senza alcuna azione  *
      *              *-------------------------------------------------*
           if        w-mov-cge-acc-imp    =    zero
                     go to cge-gep-acr-999.
       cge-gep-acr-100.
      *              *-------------------------------------------------*
      *              * Indice per scansione su righe a zero            *
      *              *-------------------------------------------------*
           move      zero                 to   w-mov-cge-inx-001      .
       cge-gep-acr-200.
      *              *-------------------------------------------------*
      *              * Incremento indice per scansione su righe        *
      *              *-------------------------------------------------*
           add       1                    to   w-mov-cge-inx-001      .
      *              *-------------------------------------------------*
      *              * Se oltre numero attuale di righe memorizzate :  *
      *              * a inserimento nuova riga                        *
      *              *-------------------------------------------------*
           if        w-mov-cge-inx-001    >    w-mov-cge-ctr-rig
                     go to cge-gep-acr-400.
      *              *-------------------------------------------------*
      *              * Se tipo archivio in esame diverso dal tipo ar-  *
      *              * chivio da trovare : a riga successiva           *
      *              *-------------------------------------------------*
           if        w-mov-cge-tip-arc
                    (w-mov-cge-inx-001)   not  = w-mov-cge-acc-tip
                     go to cge-gep-acr-200.
      *              *-------------------------------------------------*
      *              * Se codice archivio in esame diverso dal codice  *
      *              * archivio da trovare : a riga successiva         *
      *              *-------------------------------------------------*
           if        w-mov-cge-cod-arc
                    (w-mov-cge-inx-001)   not  = w-mov-cge-acc-arc
                     go to cge-gep-acr-200.
      *              *-------------------------------------------------*
      *              * Se codice sottoconto in esame diverso dal codi- *
      *              * ce sottoconto da trovare : a riga successiva    *
      *              *-------------------------------------------------*
           if        w-mov-cge-cod-stc
                    (w-mov-cge-inx-001)   not  = w-mov-cge-acc-stc
                     go to cge-gep-acr-200.
       cge-gep-acr-300.
      *              *-------------------------------------------------*
      *              * Se trovata riga pari a quella cercata           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Aggiornamento in piu' o in meno a seconda   *
      *                  * se Dare o Avere                             *
      *                  *---------------------------------------------*
           if        w-mov-cge-acc-dav    =    "A"
                     subtract  w-mov-cge-acc-imp
                                          from w-mov-cge-imp-rig
                                              (w-mov-cge-inx-001)
           else      add       w-mov-cge-acc-imp
                                          to   w-mov-cge-imp-rig
                                              (w-mov-cge-inx-001)     .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cge-gep-acr-999.
       cge-gep-acr-400.
      *              *-------------------------------------------------*
      *              * Se inserimento nuova riga                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento numero righe memorizzate         *
      *                  *---------------------------------------------*
           add       1                    to   w-mov-cge-ctr-rig      .
      *                  *---------------------------------------------*
      *                  * Memorizzazione valori iniziali per la riga  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo archivio                           *
      *                      *-----------------------------------------*
           move      w-mov-cge-acc-tip    to   w-mov-cge-tip-arc
                                              (w-mov-cge-ctr-rig)     .
      *                      *-----------------------------------------*
      *                      * Codice archivio                         *
      *                      *-----------------------------------------*
           move      w-mov-cge-acc-arc    to   w-mov-cge-cod-arc
                                              (w-mov-cge-ctr-rig)     .
      *                      *-----------------------------------------*
      *                      * Codice sottoconto                       *
      *                      *-----------------------------------------*
           move      w-mov-cge-acc-stc    to   w-mov-cge-cod-stc
                                              (w-mov-cge-ctr-rig)     .
      *                      *-----------------------------------------*
      *                      * Importo                                 *
      *                      *-----------------------------------------*
           move      zero                 to   w-mov-cge-imp-rig
                                              (w-mov-cge-ctr-rig)     .
      *                  *---------------------------------------------*
      *                  * Preparazione indice per aggiornamento riga  *
      *                  *---------------------------------------------*
           move      w-mov-cge-ctr-rig    to   w-mov-cge-inx-001      .
      *                  *---------------------------------------------*
      *                  * Ad aggiornamento riga                       *
      *                  *---------------------------------------------*
           go to     cge-gep-acr-300.
       cge-gep-acr-999.
           exit.

      *    *===========================================================*
      *    * Determinazione status scadenza                            *
      *    *-----------------------------------------------------------*
       det-dat-chs-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione data scadenza chiusa a : Aper- *
      *              * ta                                              *
      *              *-------------------------------------------------*
           move      9999999              to   rf-sdb-dat-chs         .
       det-dat-chs-020.
      *              *-------------------------------------------------*
      *              * Controllo su : Insoluto                         *
      *              *-------------------------------------------------*
           if        rf-sdb-dtr-isp       =    zero
                     go to det-dat-chs-040.
      *                  *---------------------------------------------*
      *                  * Se la scadenza e' stata riscossa : oltre    *
      *                  *---------------------------------------------*
           if        rf-sdb-num-ris       not  = zero
                     go to det-dat-chs-040.
      *                  *---------------------------------------------*
      *                  * Se a fronte dell'insoluto non e' stata e-   *
      *                  * messa una nuova scadenza, essa e' da inten- *
      *                  * dersi ancora aperta                         *
      *                  *---------------------------------------------*
           if        rf-sdb-ens-isp       =    02   and
                     rf-sdb-nns-isp       =    zero
                     go to det-dat-chs-999.
      *                  *---------------------------------------------*
      *                  * Memorizzazione data chiusura scadenza       *
      *                  *---------------------------------------------*
           move      rf-sdb-dtr-isp       to   rf-sdb-dat-chs         .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-dat-chs-999.
       det-dat-chs-040.
      *              *-------------------------------------------------*
      *              * Controllo su : Presunto buon esito              *
      *              *-------------------------------------------------*
           if        rf-sdb-dtr-pbe       =     zero
                     go to det-dat-chs-060.
      *                  *---------------------------------------------*
      *                  * Memorizzazione data chiusura scadenza       *
      *                  *---------------------------------------------*
           move      rf-sdb-dtr-pbe       to   rf-sdb-dat-chs         .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-dat-chs-999.
       det-dat-chs-060.
      *              *-------------------------------------------------*
      *              * Controllo su : Presunto buon esito              *
      *              *-------------------------------------------------*
           if        rf-sdb-dtr-nbe       =     zero
                     go to det-dat-chs-080.
      *                  *---------------------------------------------*
      *                  * Memorizzazione data chiusura scadenza       *
      *                  *---------------------------------------------*
           move      rf-sdb-dtr-nbe       to   rf-sdb-dat-chs         .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-dat-chs-999.
       det-dat-chs-080.
      *              *-------------------------------------------------*
      *              * Controllo su : Accredito scadenza al dopo in-   *
      *              *                casso                            *
      *              *-------------------------------------------------*
           if        rf-sdb-dtr-acs       =     zero
                     go to det-dat-chs-100.
      *                  *---------------------------------------------*
      *                  * Memorizzazione data chiusura scadenza       *
      *                  *---------------------------------------------*
           move      rf-sdb-dtr-acs       to   rf-sdb-dat-chs         .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-dat-chs-999.
       det-dat-chs-100.
      *              *-------------------------------------------------*
      *              * Controllo su : Richiamo della scadenza presen-  *
      *              *                tata                             *
      *              *-------------------------------------------------*
           if        rf-sdb-dtr-rsp       =     zero
                     go to det-dat-chs-120.
      *                  *---------------------------------------------*
      *                  * Memorizzazione data chiusura scadenza       *
      *                  *---------------------------------------------*
           move      rf-sdb-dtr-rsp       to   rf-sdb-dat-chs         .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-dat-chs-999.
       det-dat-chs-120.
      *              *-------------------------------------------------*
      *              * Controllo su : Inclusione in distinta           *
      *              *-------------------------------------------------*
           if        rf-sdb-num-ddp       =     zero
                     go to det-dat-chs-140.
      *                  *---------------------------------------------*
      *                  * Nessuna azione                              *
      *                  *---------------------------------------------*
           go to     det-dat-chs-140.
       det-dat-chs-140.
      *              *-------------------------------------------------*
      *              * Controllo su : Riscossione                      *
      *              *-------------------------------------------------*
           if        rf-sdb-dtr-ris       =     zero
                     go to det-dat-chs-160.
      *                  *---------------------------------------------*
      *                  * Memorizzazione data chiusura scadenza       *
      *                  *---------------------------------------------*
           move      rf-sdb-dtr-ris       to   rf-sdb-dat-chs         .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-dat-chs-999.
       det-dat-chs-160.
      *              *-------------------------------------------------*
      *              * Controllo su : Storno                           *
      *              *-------------------------------------------------*
           if        rf-sdb-dtr-sto       =     zero
                     go to det-dat-chs-180.
      *                  *---------------------------------------------*
      *                  * Memorizzazione data chiusura scadenza       *
      *                  *---------------------------------------------*
           move      rf-sdb-dtr-sto       to   rf-sdb-dat-chs         .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-dat-chs-999.
       det-dat-chs-180.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-dat-chs-999.
       det-dat-chs-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per modulo aggiornamento contabilita' genera- *
      *    * le, clienti, fornitori, iva                               *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/pcge300z.pgs"                   .
