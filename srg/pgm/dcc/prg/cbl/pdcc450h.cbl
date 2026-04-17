       Identification Division.
       Program-Id.                                 pdcc450h           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dcc                 *
      *                                Settore:    com                 *
      *                                   Fase:    dcc450              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 24/06/93    *
      *                       Ultima revisione:    NdK del 07/06/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Manutenzioni anagrafiche clienti            *
      *                                                                *
      *                    Sottoprogramma per esecuzione funzione di : *
      *                                                                *
      *                    Informazioni relative al gruppo d'acquisto  *
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
      *    * Sort record                                               *
      *    *-----------------------------------------------------------*
       01  srt-rec.
      *        *-------------------------------------------------------*
      *        * Chiave di ordinamento                                 *
      *        *-------------------------------------------------------*
           05  srt-key.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  srt-dat.
               10  filler                 pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Descrizione tipo interrogazione per l'overlay             *
      *    *-----------------------------------------------------------*
       01  w-des-tit-pgm-ovy              pic  x(40)       value
                     "INFORMAZIONI RELATIVE GRUPPO D'ACQUISTO "       .

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
      *            *---------------------------------------------------*
      *            * Work per numero pagina riga corpo                 *
      *            *---------------------------------------------------*
               10  w-cnt-cor-pag-max      pic  9(03)                  .
               10  w-cnt-cor-pag-att      pic  9(03)                  .
               10  w-cnt-cor-pag-rem      pic  9(02)                  .
               10  w-cnt-cor-pag-ep1      pic  x(03)                  .
               10  w-cnt-cor-pag-ep2      pic  x(03)                  .
               10  w-cnt-cor-pag-npa      pic  x(09)                  .
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
      *    * Link-area per accettazione codice cliente commerciale     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmndcc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice dipendenza del cliente  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoddcc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per modulo dell'area 'svf'           'msetsvf0' *
      *    *-----------------------------------------------------------*
           copy      "pgm/svf/prg/cpy/msetsvf0.mdl"                   .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
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
      *            * Selettori cliente per le chiavi                   *
      *            *---------------------------------------------------*
               10  w-tes-rag-min          pic  x(20)                  .
               10  w-tes-rag-max          pic  x(20)                  .
               10  w-tes-cod-min          pic  9(07)                  .
               10  w-tes-cod-max          pic  9(07)                  .
               10  w-tes-mne-min          pic  x(10)                  .
               10  w-tes-mne-max          pic  x(10)                  .
      *            *---------------------------------------------------*
      *            * Selettori cliente per i dati                      *
      *            *---------------------------------------------------*
               10  w-tes-tip-frn          pic  9(02)                  .
               10  w-tes-arc-plf          pic  9(07)                  .
               10  w-tes-arc-plf-rag      pic  x(40)                  .
               10  w-tes-arc-plf-via      pic  x(40)                  .
               10  w-tes-arc-plf-loc      pic  x(40)                  .
               10  w-tes-arc-plf-cge      pic  9(07)                  .
               10  w-tes-dpz-plf          pic  x(04)                  .
               10  w-tes-dpz-plf-rag      pic  x(40)                  .
               10  w-tes-dpz-plf-via      pic  x(40)                  .
               10  w-tes-dpz-plf-loc      pic  x(40)                  .
               10  w-tes-tip-ftz          pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Conferma impostazioni                             *
      *            *---------------------------------------------------*
               10  w-tes-cnf-imp          pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per bufferizzazione riga corpo                  *
      *    *-----------------------------------------------------------*
       01  w-rig.
           05  w-rig-001.
               10  w-rig-num-prg          pic  9(05)                  .
               10  w-rig-cod-cli          pic  9(07)                  .
               10  w-rig-dpz-cli          pic  x(04)                  .
               10  w-rig-cod-cli-rag      pic  x(40)                  .
               10  w-rig-cod-cli-mne      pic  x(10)                  .
               10  w-rig-tip-frn          pic  9(02)                  .
               10  w-rig-arc-plf          pic  9(07)                  .
               10  w-rig-arc-plf-rag      pic  x(40)                  .
               10  w-rig-arc-plf-via      pic  x(40)                  .
               10  w-rig-arc-plf-loc      pic  x(40)                  .
               10  w-rig-arc-plf-cge      pic  9(07)                  .
               10  w-rig-dpz-plf          pic  x(04)                  .
               10  w-rig-dpz-plf-rag      pic  x(40)                  .
               10  w-rig-dpz-plf-via      pic  x(40)                  .
               10  w-rig-dpz-plf-loc      pic  x(40)                  .
               10  w-rig-tip-ftz          pic  9(02)                  .
           05  w-rig-002.
               10  filler occurs 512      pic  x(01)                  .

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
               10  filler occurs 1024     pic  x(01)                  .

      *    *===========================================================*
      *    * Work per linea corpo a video                              *
      *    *-----------------------------------------------------------*
       01  w-lin.
      *        *-------------------------------------------------------*
      *        * Numero righe di corpo effettive visibili contempora-  *
      *        * neamente in una pagina di corpo nell'area di scroll   *
      *        *-------------------------------------------------------*
           05  w-lin-num-lin-vis          pic  9(02)       value 08   .
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
                   15  filler             pic  x(02)                  .
                   15  w-lin-imm-cod-cli  pic  x(07)                  .
                   15  filler             pic  x(02)                  .
                   15  w-lin-imm-rag-cli.
                       20  w-lin-imm-rag-rag
                                          pic  x(35)                  .
                       20  w-lin-imm-rag-fil
                                          pic  x(01)                  .
                       20  w-lin-imm-rag-dpz
                                          pic  x(04)                  .
                   15  filler             pic  x(02)                  .
                   15  w-lin-imm-tip-frn  pic  x(09)                  .
                   15  filler             pic  x(01)                  .
                   15  w-lin-imm-arc-plf  pic  x(12)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Salvataggio per codice cliente                        *
      *        *-------------------------------------------------------*
           05  w-sav-cod-cli              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Tipo fornitura abituale                               *
      *        *-------------------------------------------------------*
           05  w-sav-tip-frn              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Codice archivio per fatturazione                      *
      *        *-------------------------------------------------------*
           05  w-sav-arc-plf              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Dipendenza archivio per fatturazione                  *
      *        *-------------------------------------------------------*
           05  w-sav-dpz-plf              pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Tipo fatturazione                                     *
      *        *-------------------------------------------------------*
           05  w-sav-tip-ftz              pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per selezioni sul record [dcc]                  *
      *    *-----------------------------------------------------------*
       01  w-sel-rec-dcc.
      *        *-------------------------------------------------------*
      *        * Flag di selezione superata                            *
      *        * - spaces : selezione superata                         *
      *        * - "#"    : selezione non superata                     *
      *        *-------------------------------------------------------*
           05  w-sel-rec-dcc-flg          pic  x(01)                  .

      *    *===========================================================*
      *    * Work per routine di Expand anagrafica cliente             *
      *    *-----------------------------------------------------------*
       01  w-rou-exp-ana.
      *        *-------------------------------------------------------*
      *        * Codice cliente                                        *
      *        *-------------------------------------------------------*
           05  w-rou-exp-ana-cod          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice dipendenza cliente                             *
      *        *-------------------------------------------------------*
           05  w-rou-exp-ana-dpz          pic  x(04)                  .

      *    *===========================================================*
      *    * Work per routine di caricamento righe                     *
      *    *-----------------------------------------------------------*
       01  w-car-eff-rig.
      *        *-------------------------------------------------------*
      *        * Contatore elementi caricati                           *
      *        *-------------------------------------------------------*
           05  w-car-eff-rig-ctr          pic  9(06)                  .
      *        *-------------------------------------------------------*
      *        * Numero massimo elementi da caricare                   *
      *        *-------------------------------------------------------*
           05  w-car-eff-rig-max          pic  9(06)   value 099999   .
      *        *-------------------------------------------------------*
      *        * Flag di superamento numero massimo elementi           *
      *        *-------------------------------------------------------*
           05  w-car-eff-rig-flg          pic  x(01)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo fornitura abituale                    *
      *        *-------------------------------------------------------*
           05  w-exp-tip-frn.
               10  w-exp-tip-frn-num      pic  9(02)       value 3    .
               10  w-exp-tip-frn-lun      pic  9(02)       value 25   .
               10  w-exp-tip-frn-tbl.
                   15  filler             pic  x(25) value
                            "Tutte                    "               .
                   15  filler             pic  x(25) value
                            "Diretta                  "               .
                   15  filler             pic  x(25) value
                            "Tramite gruppo d'acquisto"               .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo fatturazione                          *
      *        *-------------------------------------------------------*
           05  w-exp-tip-ftz.
               10  w-exp-tip-ftz-num      pic  9(02)       value 3    .
               10  w-exp-tip-ftz-lun      pic  9(02)       value 40   .
               10  w-exp-tip-ftz-tbl.
                   15  filler             pic  x(40) value
                            "Tutti                                   ".               .
                   15  filler             pic  x(40) value
                            "Separata dagli altri associati          ".               .
                   15  filler             pic  x(40) value
                            "Cumulativa, assieme agli altri associati".               .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo fornitura abituale - per le righe     *
      *        *-------------------------------------------------------*
           05  w-exp-rig-frn.
               10  w-exp-rig-frn-num      pic  9(02)       value 2    .
               10  w-exp-rig-frn-lun      pic  9(02)       value 25   .
               10  w-exp-rig-frn-tbl.
                   15  filler             pic  x(25) value
                            "Diretta                  "               .
                   15  filler             pic  x(25) value
                            "Tramite gruppo d'acquisto"               .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo fatturazione - per le righe           *
      *        *-------------------------------------------------------*
           05  w-exp-rig-ftz.
               10  w-exp-rig-ftz-num      pic  9(02)       value 2    .
               10  w-exp-rig-ftz-lun      pic  9(02)       value 40   .
               10  w-exp-rig-ftz-tbl.
                   15  filler             pic  x(40) value
                            "Separata dagli altri associati          ".               .
                   15  filler             pic  x(40) value
                            "Cumulativa, assieme agli altri associati".               .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
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

      *    *===========================================================*
      *    * Work-area per variabili di i.p.c. eventualmente passate   *
      *    * dal chiamante                                             *
      *    *-----------------------------------------------------------*
       01  w-ipc.
      *        *-------------------------------------------------------*
      *        * Per tipo di chiamante del sottoprogramma              *
      *        *-------------------------------------------------------*
           05  w-ipc-tdc-mos.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c. relativa al tipo di     *
      *            * chiamante del sottoprogramma                      *
      *            *---------------------------------------------------*
               10  w-ipc-tdc-mos-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c. relativa al tipo *
      *            * di chiamante del sottoprogramma                   *
      *            * - M : Il main                                     *
      *            * - S : Un altro sottoprogramma                     *
      *            *---------------------------------------------------*
               10  w-ipc-tdc-mos-val      pic  x(01)                  .

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
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Link-area comune per programmi della serie pdcc4500       *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/pdcc4500.pgl"                   .

      ******************************************************************
       Procedure Division                using i-ide
                                               w-ovy-exe
                                               w-tmn
                                               w-spg
                                               w-prs                  .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
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
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
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
                     go to exe-acc-cmp-999.
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
      *                  * Altrimenti : uscita                         *
      *                  *---------------------------------------------*
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
      *              * Se in impostazione altri campi riga corpo : si  *
      *              * ignora                                          *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    "R"
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
      *              * Deviazione in funzione della fase di accetta-   *
      *              * zione                                           *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    "C" or
                     w-cnt-mfu-tip-imp    =    "R" or
                     w-cnt-mfu-tip-imp    =    "T"
                     go to vis-tit-pgm-des-500.
       vis-tit-pgm-des-200.
      *              *-------------------------------------------------*
      *              * Prima pagina                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se programma richiamato                *
      *                  *---------------------------------------------*
           if        w-ipc-tdc-mos-val    not  = "S"
                     move  w-des-tit-pgm-ovy
                                          to   w-all-str-alf
                     go to vis-tit-pgm-des-250.
      *                  *---------------------------------------------*
      *                  * Concatenamento                              *
      *                  *---------------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      w-des-tit-pgm-ovy    to   w-all-str-cat (1)      .
           move      "[SET]"              to   w-all-str-cat (2)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                  *---------------------------------------------*
      *                  * Allineamento al centro descrizione          *
      *                  *---------------------------------------------*
           move      40                   to   w-all-str-lun          .
           perform   all-str-cen-000      thru all-str-cen-999        .
       vis-tit-pgm-des-250.
      *                  *---------------------------------------------*
      *                  * Descrizione del programma                   *
      *                  *---------------------------------------------*
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
       vis-tit-pgm-des-500.
      *              *-------------------------------------------------*
      *              * Pagine successive                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Concatenamento con numero di record         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing numero record                   *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-car-eff-rig-ctr    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Concatenamento                          *
      *                      *-----------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      w-des-tit-pgm-ovy    to   w-all-str-cat (1)      .
           move      "{"                  to   w-all-str-cat (2)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *
           move      03                   to   w-all-str-num          .
           move      w-all-str-alf        to   w-all-str-cat (1)      .
           move      v-edt                to   w-all-str-cat (2)      .
           move      "}"                  to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *                      *-----------------------------------------*
      *                      * Allineamento al centro descrizione      *
      *                      *-----------------------------------------*
           move      40                   to   w-all-str-lun          .
           perform   all-str-cen-000      thru all-str-cen-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione dati documento          *
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
      *              * Lettura della variabile eventuale di i.p.c. per *
      *              * il tipo di chiamante del sottoprogramma, se il  *
      *              * main oppure se un altro sottoprogramma          *
      *              *-------------------------------------------------*
           perform   ipc-tdc-mos-000      thru ipc-tdc-mos-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione titolo programma                *
      *              *-------------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
           move      "#"                  to   w-cnt-sts-vis-tit      .
      *              *-------------------------------------------------*
      *              * Open dei moduli di accettazione                 *
      *              *-------------------------------------------------*
           perform   opn-mdl-acc-000      thru opn-mdl-acc-999        .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura della variabile eventuale di i.p.c. per il tipo   *
      *    * di chiamante del sottoprogramma, se il main o un altro    *
      *    * sottoprogramma                                            *
      *    *-----------------------------------------------------------*
       ipc-tdc-mos-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'tdc-mos' dallo      *
      *              * stesso livello di profondita'                   *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "tdc-mos"            to   s-var                  .
           move      "="                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda dell'esito dell'operazione *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     go to ipc-tdc-mos-200
           else      go to ipc-tdc-mos-400.
       ipc-tdc-mos-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-tdc-mos-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-alf                to   w-ipc-tdc-mos-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-tdc-mos-999.
       ipc-tdc-mos-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-tdc-mos-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile a spaces             *
      *                  *---------------------------------------------*
           move      spaces               to   w-ipc-tdc-mos-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-tdc-mos-999.
       ipc-tdc-mos-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Close dei moduli di accettazione                *
      *              *-------------------------------------------------*
           perform   cls-mdl-acc-000      thru cls-mdl-acc-999        .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Open dei moduli di accettazione                           *
      *    *-----------------------------------------------------------*
       opn-mdl-acc-000.
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice cliente commer- *
      *              * ciale                                           *
      *              *-------------------------------------------------*
           perform   cod-mne-dcc-opn-000  thru cod-mne-dcc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice dipendenza del  *
      *              * cliente                                         *
      *              *-------------------------------------------------*
           perform   cod-cod-dcc-opn-000  thru cod-cod-dcc-opn-999    .
       opn-mdl-acc-999.
           exit.

      *    *===========================================================*
      *    * Close dei moduli di accettazione                          *
      *    *-----------------------------------------------------------*
       cls-mdl-acc-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice cliente com-   *
      *              * merciale                                        *
      *              *-------------------------------------------------*
           perform   cod-mne-dcc-cls-000  thru cod-mne-dcc-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice dipendenza del *
      *              * cliente                                         *
      *              *-------------------------------------------------*
           perform   cod-cod-dcc-cls-000  thru cod-cod-dcc-cls-999    .
       cls-mdl-acc-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
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
      *              * Open modulo per la generazione set              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        w-ipc-tdc-mos-val    not  = "S"
                     go to rou-opn-fls-800.
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           perform   mdl-gen-set-opn-000  thru mdl-gen-set-opn-999    .
       rou-opn-fls-800.
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
      *              * Close modulo per la generazione set             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        w-ipc-tdc-mos-val    not  = "S"
                     go to rou-cls-fls-800.
      *                  *---------------------------------------------*
      *                  * Chiusura modulo                             *
      *                  *---------------------------------------------*
           perform   mdl-gen-set-cls-000  thru mdl-gen-set-cls-999    .
       rou-cls-fls-800.
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
      *                  * Ordinatore minimo e massimo                 *
      *                  *---------------------------------------------*
           perform   acc-ord-mem-000      thru acc-ord-mem-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
       acc-key-reg-200.
      *                  *---------------------------------------------*
      *                  * Tipo fornitura abituale                     *
      *                  *---------------------------------------------*
           perform   acc-tip-frn-000      thru acc-tip-frn-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
           if        v-key                =    "UP  "
                     go to acc-key-reg-100.
       acc-key-reg-250.
      *                  *---------------------------------------------*
      *                  * Codice archivio per la fatturazione         *
      *                  *---------------------------------------------*
           perform   acc-arc-plf-000      thru acc-arc-plf-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
           if        v-key                =    "UP  "
                     go to acc-key-reg-200.
       acc-key-reg-300.
      *                  *---------------------------------------------*
      *                  * Codice dipendenza archivio per la fattura-  *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           perform   acc-dpz-plf-000      thru acc-dpz-plf-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
           if        v-key                =    "UP  "
                     go to acc-key-reg-250.
       acc-key-reg-350.
      *                  *---------------------------------------------*
      *                  * Tipo fatturazione                           *
      *                  *---------------------------------------------*
           perform   acc-tip-ftz-000      thru acc-tip-ftz-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
           if        v-key                =    "UP  "
                     go to acc-key-reg-300.
       acc-key-reg-800.
      *                  *---------------------------------------------*
      *                  * Accettazione conferma impostazioni          *
      *                  *---------------------------------------------*
           perform   acc-cnf-imp-000      thru acc-cnf-imp-999        .
           if        w-cnt-tus-acc-key    =    "E"
                     move  spaces         to   w-cnt-tus-acc-key
                     go to acc-key-reg-100.
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
           if        v-key                =    "UP  "
                     go to acc-key-reg-250.
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
      *              * Ordinatore minimo e massimo                     *
      *              *-------------------------------------------------*
           perform   vis-ord-mem-000      thru vis-ord-mem-999        .
      *              *-------------------------------------------------*
      *              * Tipo fornitura abituale                         *
      *              *-------------------------------------------------*
           perform   vis-tip-frn-000      thru vis-tip-frn-999        .
      *              *-------------------------------------------------*
      *              * Codice archivio per la fatturazione             *
      *              *-------------------------------------------------*
           perform   vis-arc-plf-000      thru vis-arc-plf-999        .
           perform   vis-arc-plf-rag-000  thru vis-arc-plf-rag-999    .
      *              *-------------------------------------------------*
      *              * Codice dipendenza archivio per la fatturazione  *
      *              *-------------------------------------------------*
           perform   vis-dpz-plf-000      thru vis-dpz-plf-999        .
           perform   vis-dpz-plf-rag-000  thru vis-dpz-plf-rag-999    .
           perform   vis-dpz-plf-via-000  thru vis-dpz-plf-via-999    .
           perform   vis-dpz-plf-loc-000  thru vis-dpz-plf-loc-999    .
      *              *-------------------------------------------------*
      *              * Tipo fatturazione                               *
      *              *-------------------------------------------------*
           perform   vis-tip-ftz-000      thru vis-tip-ftz-999        .
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
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-key-reg-100.
      *              *-------------------------------------------------*
      *              * Visualizzazioni prompts                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ordinatore minimo e massimo                 *
      *                  *---------------------------------------------*
           perform   pmt-ord-mem-000      thru pmt-ord-mem-999        .
      *                  *---------------------------------------------*
      *                  * Linea di trattini di separazione            *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "------------------------------- Altre selezioni --
      -              "------------------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-key-reg-200.
      *              *-------------------------------------------------*
      *              * Tipo fornitura abituale                         *
      *              *-------------------------------------------------*
           perform   pmt-tip-frn-000      thru pmt-tip-frn-999        .
      *              *-------------------------------------------------*
      *              * Codice archivio per la fatturazione             *
      *              *-------------------------------------------------*
           perform   pmt-arc-plf-000      thru pmt-arc-plf-999        .
      *              *-------------------------------------------------*
      *              * Codice dipendenza archivio per la fatturazione  *
      *              *-------------------------------------------------*
           perform   pmt-dpz-plf-000      thru pmt-dpz-plf-999        .
      *              *-------------------------------------------------*
      *              * Tipo fatturazione                               *
      *              *-------------------------------------------------*
           perform   pmt-tip-ftz-000      thru pmt-tip-ftz-999        .
       pmt-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Ordinatore minimo e massimo                      *
      *    *-----------------------------------------------------------*
       pmt-ord-mem-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo ordinamento da  *
      *              * personalizzazione                               *
      *              *-------------------------------------------------*
           if        w-prs-tip-ord-tip    =    "R"
                     go to pmt-ord-mem-200
           else if   w-prs-tip-ord-tip    =    "C"
                     go to pmt-ord-mem-400
           else if   w-prs-tip-ord-tip    =    "M"
                     go to pmt-ord-mem-600.
       pmt-ord-mem-200.
      *                  *---------------------------------------------*
      *                  * Ordinamento per Ragione sociale             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ragione sociale minima                  *
      *                      *-----------------------------------------*
           perform   pmt-rag-min-000      thru pmt-rag-min-999        .
      *                      *-----------------------------------------*
      *                      * Ragione sociale massima                 *
      *                      *-----------------------------------------*
           perform   pmt-rag-max-000      thru pmt-rag-max-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pmt-ord-mem-999.
       pmt-ord-mem-400.
      *                  *---------------------------------------------*
      *                  * Ordinamento per Codice                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice minimo                           *
      *                      *-----------------------------------------*
           perform   pmt-cod-min-000      thru pmt-cod-min-999        .
      *                      *-----------------------------------------*
      *                      * Codice massimo                          *
      *                      *-----------------------------------------*
           perform   pmt-cod-max-000      thru pmt-cod-max-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pmt-ord-mem-999.
       pmt-ord-mem-600.
      *                  *---------------------------------------------*
      *                  * Ordinamento per Mnemonico                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Mnemonico minimo                        *
      *                      *-----------------------------------------*
           perform   pmt-mne-min-000      thru pmt-mne-min-999        .
      *                      *-----------------------------------------*
      *                      * Mnemonico massimo                       *
      *                      *-----------------------------------------*
           perform   pmt-mne-max-000      thru pmt-mne-max-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pmt-ord-mem-999.
       pmt-ord-mem-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Ragione sociale minima                           *
      *    *-----------------------------------------------------------*
       pmt-rag-min-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Ragione sociale minima  da selezionare :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-rag-min-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Ragione sociale massima                          *
      *    *-----------------------------------------------------------*
       pmt-rag-max-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Ragione sociale massima da selezionare :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-rag-max-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Codice minimo                                    *
      *    *-----------------------------------------------------------*
       pmt-cod-min-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      31                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice minimo  da selezionare :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-min-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Codice massimo                                   *
      *    *-----------------------------------------------------------*
       pmt-cod-max-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      31                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice massimo da selezionare :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-max-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Mnemonico minimo                                 *
      *    *-----------------------------------------------------------*
       pmt-mne-min-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      34                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Mnemonico minimo  da selezionare :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-mne-min-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Mnemonico massimo                                *
      *    *-----------------------------------------------------------*
       pmt-mne-max-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      34                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Mnemonico massimo da selezionare :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-mne-max-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tipo fornitura abituale          *
      *    *-----------------------------------------------------------*
       pmt-tip-frn-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo fornitura abituale    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-frn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice archivio per fatturazione *
      *    *-----------------------------------------------------------*
       pmt-arc-plf-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Cliente capogruppo         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-arc-plf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Dipendenza archivio per fattura- *
      *    * zione                                                     *
      *    *-----------------------------------------------------------*
       pmt-dpz-plf-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Sua dipendenza             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dpz-plf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tipo fatturazione                *
      *    *-----------------------------------------------------------*
       pmt-tip-ftz-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo fatturazione          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-ftz-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Ordinatore minimo e massimo          *
      *    *-----------------------------------------------------------*
       acc-ord-mem-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo ordinamento da  *
      *              * personalizzazione                               *
      *              *-------------------------------------------------*
           if        w-prs-tip-ord-tip    =    "R"
                     go to acc-ord-mem-200
           else if   w-prs-tip-ord-tip    =    "C"
                     go to acc-ord-mem-400
           else if   w-prs-tip-ord-tip    =    "M"
                     go to acc-ord-mem-600.
       acc-ord-mem-200.
      *                  *---------------------------------------------*
      *                  * Ordinamento per Ragione sociale             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ragione sociale minima                  *
      *                      *-----------------------------------------*
           perform   acc-rag-min-000      thru acc-rag-min-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-ord-mem-999.
      *                      *-----------------------------------------*
      *                      * Ragione sociale massima                 *
      *                      *-----------------------------------------*
           perform   acc-rag-max-000      thru acc-rag-max-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-ord-mem-999.
           if        v-key                =    "UP  "
                     go to acc-ord-mem-200.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     acc-ord-mem-999.
       acc-ord-mem-400.
      *                  *---------------------------------------------*
      *                  * Ordinamento per Codice                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice minimo                           *
      *                      *-----------------------------------------*
           perform   acc-cod-min-000      thru acc-cod-min-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-ord-mem-999.
      *                      *-----------------------------------------*
      *                      * Codice massimo                          *
      *                      *-----------------------------------------*
           perform   acc-cod-max-000      thru acc-cod-max-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-ord-mem-999.
           if        v-key                =    "UP  "
                     go to acc-ord-mem-400.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     acc-ord-mem-999.
       acc-ord-mem-600.
      *                  *---------------------------------------------*
      *                  * Ordinamento per Mnemonico                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Mnemonico minimo                        *
      *                      *-----------------------------------------*
           perform   acc-mne-min-000      thru acc-mne-min-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-ord-mem-999.
      *                      *-----------------------------------------*
      *                      * Mnemonico massimo                       *
      *                      *-----------------------------------------*
           perform   acc-mne-max-000      thru acc-mne-max-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-ord-mem-999.
           if        v-key                =    "UP  "
                     go to acc-ord-mem-600.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     acc-ord-mem-999.
       acc-ord-mem-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Ordinatore minimo e massimo       *
      *    *-----------------------------------------------------------*
       vis-ord-mem-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo ordinamento da  *
      *              * personalizzazione                               *
      *              *-------------------------------------------------*
           if        w-prs-tip-ord-tip    =    "R"
                     go to vis-ord-mem-200
           else if   w-prs-tip-ord-tip    =    "C"
                     go to vis-ord-mem-400
           else if   w-prs-tip-ord-tip    =    "M"
                     go to vis-ord-mem-600.
       vis-ord-mem-200.
      *                  *---------------------------------------------*
      *                  * Ordinamento per Ragione sociale             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ragione sociale minima                  *
      *                      *-----------------------------------------*
           perform   vis-rag-min-000      thru vis-rag-min-999        .
      *                      *-----------------------------------------*
      *                      * Ragione sociale massima                 *
      *                      *-----------------------------------------*
           perform   vis-rag-max-000      thru vis-rag-max-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     vis-ord-mem-999.
       vis-ord-mem-400.
      *                  *---------------------------------------------*
      *                  * Ordinamento per Codice                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice minimo                           *
      *                      *-----------------------------------------*
           perform   vis-cod-min-000      thru vis-cod-min-999        .
      *                      *-----------------------------------------*
      *                      * Codice massimo                          *
      *                      *-----------------------------------------*
           perform   vis-cod-max-000      thru vis-cod-max-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     vis-ord-mem-999.
       vis-ord-mem-600.
      *                  *---------------------------------------------*
      *                  * Ordinamento per Mnemonico                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Mnemonico minimo                        *
      *                      *-----------------------------------------*
           perform   vis-mne-min-000      thru vis-mne-min-999        .
      *                      *-----------------------------------------*
      *                      * Mnemonico massimo                       *
      *                      *-----------------------------------------*
           perform   vis-mne-max-000      thru vis-mne-max-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     vis-ord-mem-999.
       vis-ord-mem-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Ragione sociale minima     *
      *    *-----------------------------------------------------------*
       acc-rag-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-rag-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      42                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-rag-min        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-rag-min-999.
       acc-rag-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-rag-min          .
       acc-rag-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-rag-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-rag-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-rag-min-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-rag-min-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-rag-min-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-rag-min-999.
       acc-rag-min-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Ragione sociale minima            *
      *    *-----------------------------------------------------------*
       vis-rag-min-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-rag-min        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rag-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Ragione sociale massima    *
      *    *-----------------------------------------------------------*
       acc-rag-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-rag-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      42                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-rag-max        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-rag-max-999.
       acc-rag-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-rag-max          .
       acc-rag-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-rag-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-rag-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-rag-max-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-rag-max-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-rag-max-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-rag-max-999.
       acc-rag-max-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Ragione sociale massima           *
      *    *-----------------------------------------------------------*
       vis-rag-max-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-rag-max        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rag-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Codice cliente minimo      *
      *    *-----------------------------------------------------------*
       acc-cod-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-dcc-ope      .
           move      w-tes-cod-min        to   w-cod-mne-dcc-cod      .
           move      05                   to   w-cod-mne-dcc-lin      .
           move      33                   to   w-cod-mne-dcc-pos      .
           move      zero                 to   w-cod-mne-dcc-rln      .
           move      zero                 to   w-cod-mne-dcc-rps      .
           move      zero                 to   w-cod-mne-dcc-vln      .
           move      zero                 to   w-cod-mne-dcc-vps      .
           move      zero                 to   w-cod-mne-dcc-lln      .
           move      zero                 to   w-cod-mne-dcc-lps      .
           move      "<B"                 to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-dcc-cll-000  thru cod-mne-dcc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-dcc-foi-000  thru cod-mne-dcc-foi-999    .
       acc-cod-min-110.
           perform   cod-mne-dcc-cll-000  thru cod-mne-dcc-cll-999    .
           if        w-cod-mne-dcc-ope    =    "F+"
                     go to acc-cod-min-115.
           if        w-cod-mne-dcc-ope    =    "AC"
                     go to acc-cod-min-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-min-115.
           perform   cod-mne-dcc-foi-000  thru cod-mne-dcc-foi-999    .
           go to     acc-cod-min-110.
       acc-cod-min-120.
           move      w-cod-mne-dcc-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-min-999.
       acc-cod-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-min          .
       acc-cod-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cod-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-min-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-min-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-min-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-min-999.
       acc-cod-min-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice cliente minimo             *
      *    *-----------------------------------------------------------*
       vis-cod-min-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      05                   to   v-lin                  .
           move      33                   to   v-pos                  .
           move      w-tes-cod-min        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Codice massimo             *
      *    *-----------------------------------------------------------*
       acc-cod-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-dcc-ope      .
           move      w-tes-cod-max        to   w-cod-mne-dcc-cod      .
           move      06                   to   w-cod-mne-dcc-lin      .
           move      33                   to   w-cod-mne-dcc-pos      .
           move      zero                 to   w-cod-mne-dcc-rln      .
           move      zero                 to   w-cod-mne-dcc-rps      .
           move      zero                 to   w-cod-mne-dcc-vln      .
           move      zero                 to   w-cod-mne-dcc-vps      .
           move      zero                 to   w-cod-mne-dcc-lln      .
           move      zero                 to   w-cod-mne-dcc-lps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-dcc-cll-000  thru cod-mne-dcc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-dcc-foi-000  thru cod-mne-dcc-foi-999    .
       acc-cod-max-110.
           perform   cod-mne-dcc-cll-000  thru cod-mne-dcc-cll-999    .
           if        w-cod-mne-dcc-ope    =    "F+"
                     go to acc-cod-max-115.
           if        w-cod-mne-dcc-ope    =    "AC"
                     go to acc-cod-max-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-max-115.
           perform   cod-mne-dcc-foi-000  thru cod-mne-dcc-foi-999    .
           go to     acc-cod-max-110.
       acc-cod-max-120.
           move      w-cod-mne-dcc-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-max-999.
       acc-cod-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-max          .
       acc-cod-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cod-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-max-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-max-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-max-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-max-999.
       acc-cod-max-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice cliente massimo            *
      *    *-----------------------------------------------------------*
       vis-cod-max-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      06                   to   v-lin                  .
           move      33                   to   v-pos                  .
           move      w-tes-cod-max        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Mnemonico minimo           *
      *    *-----------------------------------------------------------*
       acc-mne-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-mne-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-mne-min        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-mne-min-999.
       acc-mne-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-mne-min          .
       acc-mne-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-mne-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-mne-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-mne-min-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-mne-min-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-mne-min-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-mne-min-999.
       acc-mne-min-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Mnemonico minimo                  *
      *    *-----------------------------------------------------------*
       vis-mne-min-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-tes-mne-min        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-mne-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Mnemonico massimo          *
      *    *-----------------------------------------------------------*
       acc-mne-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-mne-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-mne-max        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-mne-max-999.
       acc-mne-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-mne-max          .
       acc-mne-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-mne-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-mne-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-mne-max-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-mne-max-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-mne-max-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-mne-max-999.
       acc-mne-max-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Mnemonico massimo                 *
      *    *-----------------------------------------------------------*
       vis-mne-max-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-tes-mne-max        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-mne-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo fornitura abituale              *
      *    *-----------------------------------------------------------*
       acc-tip-frn-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-tip-frn        to   w-sav-tip-frn          .
       acc-tip-frn-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-frn-lun    to   v-car                  .
           move      w-exp-tip-frn-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-frn-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-tes-tip-frn        =    99
                     move  01             to   v-num
           else if   w-tes-tip-frn        =    01
                     move  02             to   v-num
           else if   w-tes-tip-frn        =    21
                     move  03             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-tip-frn-999.
       acc-tip-frn-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  99             to   w-tes-tip-frn
           else if   v-num                =    02
                     move  01             to   w-tes-tip-frn
           else if   v-num                =    03
                     move  21             to   w-tes-tip-frn
           else      move  zero           to   w-tes-tip-frn          .
       acc-tip-frn-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tip-frn-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-frn-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-tip-frn-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-tip-frn-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-tip-frn-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-tip-frn-999.
       acc-tip-frn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tipo fornitura abituale           *
      *    *-----------------------------------------------------------*
       vis-tip-frn-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-frn-lun    to   v-car                  .
           move      w-exp-tip-frn-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-frn-tbl    to   v-txt                  .
           if        w-tes-tip-frn        =    99
                     move  01             to   v-num
           else if   w-tes-tip-frn        =    01
                     move  02             to   v-num
           else if   w-tes-tip-frn        =    21
                     move  03             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-frn-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice archivio per fatturazione     *
      *    *-----------------------------------------------------------*
       acc-arc-plf-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-arc-plf-100.
      *              *-------------------------------------------------*
      *              * Salvataggio valore precedente                   *
      *              *-------------------------------------------------*
           move      w-tes-arc-plf        to   w-sav-arc-plf          .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-dcc-ope      .
           move      w-tes-arc-plf        to   w-cod-mne-dcc-cod      .
           move      12                   to   w-cod-mne-dcc-lin      .
           move      30                   to   w-cod-mne-dcc-pos      .
           move      12                   to   w-cod-mne-dcc-rln      .
           move      41                   to   w-cod-mne-dcc-rps      .
           move      zero                 to   w-cod-mne-dcc-vln      .
           move      zero                 to   w-cod-mne-dcc-vps      .
           move      zero                 to   w-cod-mne-dcc-lln      .
           move      zero                 to   w-cod-mne-dcc-lps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-dcc-cll-000  thru cod-mne-dcc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-dcc-foi-000  thru cod-mne-dcc-foi-999    .
       acc-arc-plf-110.
           perform   cod-mne-dcc-cll-000  thru cod-mne-dcc-cll-999    .
           if        w-cod-mne-dcc-ope    =    "F+"
                     go to acc-arc-plf-115.
           if        w-cod-mne-dcc-ope    =    "AC"
                     go to acc-arc-plf-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-arc-plf-115.
           perform   cod-mne-dcc-foi-000  thru cod-mne-dcc-foi-999    .
           go to     acc-arc-plf-110.
       acc-arc-plf-120.
           move      w-cod-mne-dcc-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-arc-plf-999.
       acc-arc-plf-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-arc-plf          .
       acc-arc-plf-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-arc-plf-410.
      *                  *---------------------------------------------*
      *                  * Lettura file [cli]                          *
      *                  *---------------------------------------------*
           move      w-tes-arc-plf        to   w-let-arc-cli-cod      .
           perform   let-arc-cli-000      thru let-arc-cli-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione ragione sociale              *
      *                  *---------------------------------------------*
           move      w-let-arc-cli-rag    to   w-tes-arc-plf-rag      .
           move      w-let-arc-cli-via    to   w-tes-arc-plf-via      .
           move      w-let-arc-cli-loc    to   w-tes-arc-plf-loc      .
           move      w-let-arc-cli-cge    to   w-tes-arc-plf-cge      .
       acc-arc-plf-420.
      *                  *---------------------------------------------*
      *                  * Visualizzazione ragione sociale da anagra-  *
      *                  * fica contabile                              *
      *                  *---------------------------------------------*
           perform   vis-arc-plf-rag-000  thru vis-arc-plf-rag-999    .
       acc-arc-plf-430.
      *                  *---------------------------------------------*
      *                  * Se cliente non esistente                    *
      *                  *---------------------------------------------*
           if        w-let-arc-cli-flg    =    spaces
                     go to acc-arc-plf-440.
       acc-arc-plf-432.
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione di-   *
      *                      * pendenza del cliente                    *
      *                      *-----------------------------------------*
           if        w-tes-dpz-plf        =    spaces and
                     w-tes-dpz-plf-rag    =    spaces and
                     w-tes-dpz-plf-via    =    spaces and
                     w-tes-dpz-plf-loc    =    spaces
                     go to acc-arc-plf-434.
           move      spaces               to   w-tes-dpz-plf          .
           move      spaces               to   w-tes-dpz-plf-rag      .
           move      spaces               to   w-tes-dpz-plf-via      .
           move      spaces               to   w-tes-dpz-plf-loc      .
           perform   vis-dpz-plf-000      thru vis-dpz-plf-999        .
           perform   vis-dpz-plf-rag-000  thru vis-dpz-plf-rag-999    .
           perform   vis-dpz-plf-via-000  thru vis-dpz-plf-via-999    .
           perform   vis-dpz-plf-loc-000  thru vis-dpz-plf-loc-999    .
       acc-arc-plf-434.
      *                      *-----------------------------------------*
      *                      * A dipendenze dall'impostazione          *
      *                      *-----------------------------------------*
           go to     acc-arc-plf-600.
       acc-arc-plf-440.
      *                  *---------------------------------------------*
      *                  * Se codice a zero                            *
      *                  *---------------------------------------------*
           if        w-tes-arc-plf        not  = zero
                     go to acc-arc-plf-450.
       acc-arc-plf-442.
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione di-   *
      *                      * pendenza del cliente                    *
      *                      *-----------------------------------------*
           if        w-tes-dpz-plf        =    spaces and
                     w-tes-dpz-plf-rag    =    spaces and
                     w-tes-dpz-plf-via    =    spaces and
                     w-tes-dpz-plf-loc    =    spaces
                     go to acc-arc-plf-444.
           move      spaces               to   w-tes-dpz-plf          .
           move      spaces               to   w-tes-dpz-plf-rag      .
           move      spaces               to   w-tes-dpz-plf-via      .
           move      spaces               to   w-tes-dpz-plf-loc      .
           perform   vis-dpz-plf-000      thru vis-dpz-plf-999        .
           perform   vis-dpz-plf-rag-000  thru vis-dpz-plf-rag-999    .
           perform   vis-dpz-plf-via-000  thru vis-dpz-plf-via-999    .
           perform   vis-dpz-plf-loc-000  thru vis-dpz-plf-loc-999    .
       acc-arc-plf-444.
      *                      *-----------------------------------------*
      *                      * A dipendenze dall'impostazione          *
      *                      *-----------------------------------------*
           go to     acc-arc-plf-600.
       acc-arc-plf-450.
      *                  *---------------------------------------------*
      *                  * Se codice diverso da zero                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del confronto con  *
      *                      * il valore precedente                    *
      *                      *-----------------------------------------*
           if        w-tes-arc-plf        =    w-sav-arc-plf
                     go to acc-arc-plf-455
           else      go to acc-arc-plf-480.
       acc-arc-plf-455.
      *                      *-----------------------------------------*
      *                      * Se valore impostato pari al valore pre- *
      *                      * cedente                                 *
      *                      *-----------------------------------------*
       acc-arc-plf-457.
      *                          *-------------------------------------*
      *                          * Lettura anagrafica commerciale del  *
      *                          * cliente principale                  *
      *                          *-------------------------------------*
           move      "C"                  to   w-let-arc-dcc-tle      .
           move      w-tes-arc-plf        to   w-let-arc-dcc-cod      .
           move      spaces               to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                          *-------------------------------------*
      *                          * Deviazione a seconda dell'esito     *
      *                          * della lettura                       *
      *                          *-------------------------------------*
           if        w-let-arc-dcc-flg    =    spaces
                     go to acc-arc-plf-467.
       acc-arc-plf-459.
      *                          *-------------------------------------*
      *                          * Se anagrafica commerciale del cli-  *
      *                          * ente principale non esistente       *
      *                          *-------------------------------------*
       acc-arc-plf-461.
      *                              *---------------------------------*
      *                              * Normalizzazione e visualizza-   *
      *                              * zione dei dati relativi alla    *
      *                              * dipendenza                      *
      *                              *---------------------------------*
           if        w-tes-dpz-plf        =    spaces and
                     w-tes-dpz-plf-rag    =    spaces and
                     w-tes-dpz-plf-via    =    spaces and
                     w-tes-dpz-plf-loc    =    spaces
                     go to acc-arc-plf-463.
           move      spaces               to   w-tes-dpz-plf          .
           move      spaces               to   w-tes-dpz-plf-rag      .
           move      spaces               to   w-tes-dpz-plf-via      .
           move      spaces               to   w-tes-dpz-plf-loc      .
           perform   vis-dpz-plf-000      thru vis-dpz-plf-999        .
           perform   vis-dpz-plf-rag-000  thru vis-dpz-plf-rag-999    .
           perform   vis-dpz-plf-via-000  thru vis-dpz-plf-via-999    .
           perform   vis-dpz-plf-loc-000  thru vis-dpz-plf-loc-999    .
       acc-arc-plf-463.
      *                              *---------------------------------*
      *                              * Messaggio di errore             *
      *                              *---------------------------------*
           move      "Manca l'anagrafica commerciale per il cliente     
      -              "          "         to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       acc-arc-plf-465.
      *                              *---------------------------------*
      *                              * A dipendenze dall'impostazione  *
      *                              *---------------------------------*
           go to     acc-arc-plf-600.
       acc-arc-plf-467.
      *                          *-------------------------------------*
      *                          * Se anagrafica commerciale del cli-  *
      *                          * ente principale esistente           *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Memorizzazione valori letti da  *
      *                              * anagrafica commerciale cliente  *
      *                              * principale in dati per la di-   *
      *                              * pendenza                        *
      *                              *---------------------------------*
           move      w-let-arc-dcc-rag    to   w-tes-dpz-plf-rag      .
           move      w-let-arc-dcc-via    to   w-tes-dpz-plf-via      .
           move      w-let-arc-dcc-loc    to   w-tes-dpz-plf-loc      .
      *                              *---------------------------------*
      *                              * Deviazione a seconda se il co-  *
      *                              * dice dipendenza e' a spaces op- *
      *                              * pure diverso da spaces          *
      *                              *---------------------------------*
           if        w-tes-dpz-plf        =    spaces
                     go to acc-arc-plf-469
           else      go to acc-arc-plf-471.
       acc-arc-plf-469.
      *                              *---------------------------------*
      *                              * Se codice dipendenza a spaces   *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Visualizzazione valori re-  *
      *                                  * lativi alla dipendenza      *
      *                                  *-----------------------------*
           perform   vis-dpz-plf-000      thru vis-dpz-plf-999        .
           perform   vis-dpz-plf-rag-000  thru vis-dpz-plf-rag-999    .
           perform   vis-dpz-plf-via-000  thru vis-dpz-plf-via-999    .
           perform   vis-dpz-plf-loc-000  thru vis-dpz-plf-loc-999    .
      *                                  *-----------------------------*
      *                                  * A ulteriori controlli su    *
      *                                  * impostazione                *
      *                                  *-----------------------------*
           go to     acc-arc-plf-500.
       acc-arc-plf-471.
      *                              *---------------------------------*
      *                              * Se codice dipendenza diverso da *
      *                              * spaces                          *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Lettura anagrafica commer-  *
      *                                  * ciale della dipendenza      *
      *                                  *-----------------------------*
           move      "D"                  to   w-let-arc-dcc-tle      .
           move      w-tes-arc-plf        to   w-let-arc-dcc-cod      .
           move      w-tes-dpz-plf        to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                                  *-----------------------------*
      *                                  * Deviazione a seconda del-   *
      *                                  * l'esito della lettura       *
      *                                  *-----------------------------*
           if        w-let-arc-dcc-flg    =    spaces
                     go to acc-arc-plf-473
           else      go to acc-arc-plf-475.
       acc-arc-plf-473.
      *                                  *-----------------------------*
      *                                  * Se anagrafica commerciale   *
      *                                  * della dipendenza esistente  *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Memorizzazione valori   *
      *                                      * letti da anagrafica     *
      *                                      * commerciale della di-   *
      *                                      * pendenza                *
      *                                      *-------------------------*
           move      w-let-arc-dcc-rag    to   w-tes-dpz-plf-rag      .
           move      w-let-arc-dcc-via    to   w-tes-dpz-plf-via      .
           move      w-let-arc-dcc-loc    to   w-tes-dpz-plf-loc      .
      *                                      *-------------------------*
      *                                      * Visualizzazione valori  *
      *                                      * relativi alla dipenden- *
      *                                      * za                      *
      *                                      *-------------------------*
           perform   vis-dpz-plf-000      thru vis-dpz-plf-999        .
           perform   vis-dpz-plf-rag-000  thru vis-dpz-plf-rag-999    .
           perform   vis-dpz-plf-via-000  thru vis-dpz-plf-via-999    .
           perform   vis-dpz-plf-loc-000  thru vis-dpz-plf-loc-999    .
      *                                      *-------------------------*
      *                                      * A ulteriori controlli   *
      *                                      * su impostazione         *
      *                                      *-------------------------*
           go to     acc-arc-plf-500.
       acc-arc-plf-475.
      *                                  *-----------------------------*
      *                                  * Se anagrafica commerciale   *
      *                                  * della dipendenza non esi-   *
      *                                  * stente                      *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Memorizzazione valori   *
      *                                      * letti da anagrafica     *
      *                                      * commerciale della di-   *
      *                                      * pendenza                *
      *                                      *-------------------------*
           move      w-let-arc-dcc-rag    to   w-tes-dpz-plf-rag      .
           move      w-let-arc-dcc-via    to   w-tes-dpz-plf-via      .
           move      w-let-arc-dcc-loc    to   w-tes-dpz-plf-loc      .
      *                                      *-------------------------*
      *                                      * Visualizzazione valori  *
      *                                      * relativi alla dipenden- *
      *                                      * za                      *
      *                                      *-------------------------*
           perform   vis-dpz-plf-000      thru vis-dpz-plf-999        .
           perform   vis-dpz-plf-rag-000  thru vis-dpz-plf-rag-999    .
           perform   vis-dpz-plf-via-000  thru vis-dpz-plf-via-999    .
           perform   vis-dpz-plf-loc-000  thru vis-dpz-plf-loc-999    .
      *                                      *-------------------------*
      *                                      * Messaggio di errore     *
      *                                      *-------------------------*
           move      spaces               to   w-err-box-err-msg      .
           string    "Manca l'anagrafica commerciale per la dipendenza "
                                delimited by   size
                     "'"        delimited by   size
                     w-tes-dpz-plf
                                delimited by spaces
                     "'"        delimited by   size
                                          into w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                                      *-------------------------*
      *                                      * A dipendenze dall'impo- *
      *                                      * stazione                *
      *                                      *-------------------------*
           go to     acc-arc-plf-600.
       acc-arc-plf-480.
      *                      *-----------------------------------------*
      *                      * Se valore impostato diverso dal valore  *
      *                      * precedente                              *
      *                      *-----------------------------------------*
       acc-arc-plf-482.
      *                              *---------------------------------*
      *                              * Normalizzazione e visualizza-   *
      *                              * zione dei dati relativi alla    *
      *                              * dipendenza                      *
      *                              *---------------------------------*
           if        w-tes-dpz-plf        =    spaces and
                     w-tes-dpz-plf-rag    =    spaces and
                     w-tes-dpz-plf-via    =    spaces and
                     w-tes-dpz-plf-loc    =    spaces
                     go to acc-arc-plf-484.
           move      spaces               to   w-tes-dpz-plf          .
           move      spaces               to   w-tes-dpz-plf-rag      .
           move      spaces               to   w-tes-dpz-plf-via      .
           move      spaces               to   w-tes-dpz-plf-loc      .
           perform   vis-dpz-plf-000      thru vis-dpz-plf-999        .
           perform   vis-dpz-plf-rag-000  thru vis-dpz-plf-rag-999    .
           perform   vis-dpz-plf-via-000  thru vis-dpz-plf-via-999    .
           perform   vis-dpz-plf-loc-000  thru vis-dpz-plf-loc-999    .
       acc-arc-plf-484.
      *                              *---------------------------------*
      *                              * Riaggancio                      *
      *                              *---------------------------------*
           go to     acc-arc-plf-457.
       acc-arc-plf-500.
      *                  *---------------------------------------------*
      *                  * Nessun ulteriore controllo                  *
      *                  *---------------------------------------------*
       acc-arc-plf-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-arc-plf-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-arc-plf-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-arc-plf-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-arc-plf-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-arc-plf-999.
       acc-arc-plf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice archivio per fatturazione  *
      *    *-----------------------------------------------------------*
       vis-arc-plf-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-arc-plf        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-arc-plf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice archivio per fatturazione, *
      *    *                         ragione sociale                   *
      *    *-----------------------------------------------------------*
       vis-arc-plf-rag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-arc-plf-rag    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-arc-plf-rag-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Dipendenza archivio per fatturazione *
      *    *-----------------------------------------------------------*
       acc-dpz-plf-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-arc-plf        =    zero
                     go to acc-dpz-plf-999.
       acc-dpz-plf-100.
      *              *-------------------------------------------------*
      *              * Salvataggio valore precedente                   *
      *              *-------------------------------------------------*
           move      w-tes-dpz-plf        to   w-sav-dpz-plf          .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-dcc-ope      .
           move      w-tes-arc-plf        to   w-cod-cod-dcc-cli      .
           move      w-tes-dpz-plf        to   w-cod-cod-dcc-cod      .
           move      14                   to   w-cod-cod-dcc-lin      .
           move      30                   to   w-cod-cod-dcc-pos      .
           move      14                   to   w-cod-cod-dcc-rln      .
           move      41                   to   w-cod-cod-dcc-rps      .
           move      15                   to   w-cod-cod-dcc-vln      .
           move      41                   to   w-cod-cod-dcc-vps      .
           move      16                   to   w-cod-cod-dcc-lln      .
           move      41                   to   w-cod-cod-dcc-lps      .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-cod-dcc-cll-000  thru cod-cod-dcc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-dcc-foi-000  thru cod-cod-dcc-foi-999    .
       acc-dpz-plf-110.
           perform   cod-cod-dcc-cll-000  thru cod-cod-dcc-cll-999    .
           if        w-cod-cod-dcc-ope    =    "F+"
                     go to acc-dpz-plf-115.
           if        w-cod-cod-dcc-ope    =    "AC"
                     go to acc-dpz-plf-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-dpz-plf-115.
           perform   cod-cod-dcc-foi-000  thru cod-cod-dcc-foi-999    .
           go to     acc-dpz-plf-110.
       acc-dpz-plf-120.
           move      w-cod-cod-dcc-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-dpz-plf-999.
       acc-dpz-plf-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-dpz-plf          .
       acc-dpz-plf-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dpz-plf-410.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se codice sipendenza   *
      *                  * a spaces oppure no                          *
      *                  *---------------------------------------------*
           if        w-tes-dpz-plf        =    spaces
                     go to acc-dpz-plf-420
           else      go to acc-dpz-plf-440.
       acc-dpz-plf-420.
      *                  *---------------------------------------------*
      *                  * Se codice dipendenza a spaces               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica commerciale del cli- *
      *                      * ente principale                         *
      *                      *-----------------------------------------*
           move      "C"                  to   w-let-arc-dcc-tle      .
           move      w-tes-arc-plf        to   w-let-arc-dcc-cod      .
           move      spaces               to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                      *-----------------------------------------*
      *                      * Memorizzazione valori letti da anagra-  *
      *                      * fica commerciale cliente principale in  *
      *                      * dati per la dipendenza                  *
      *                      *-----------------------------------------*
           move      w-let-arc-dcc-rag    to   w-tes-dpz-plf-rag      .
           move      w-let-arc-dcc-via    to   w-tes-dpz-plf-via      .
           move      w-let-arc-dcc-loc    to   w-tes-dpz-plf-loc      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione valori relativi alla    *
      *                      * dipendenza                              *
      *                      *-----------------------------------------*
           perform   vis-dpz-plf-000      thru vis-dpz-plf-999        .
           perform   vis-dpz-plf-rag-000  thru vis-dpz-plf-rag-999    .
           perform   vis-dpz-plf-via-000  thru vis-dpz-plf-via-999    .
           perform   vis-dpz-plf-loc-000  thru vis-dpz-plf-loc-999    .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda dell'esito della   *
      *                      * lettura                                 *
      *                      *-----------------------------------------*
           if        w-let-arc-dcc-flg    =    spaces
                     go to acc-dpz-plf-425
           else      go to acc-dpz-plf-430.
       acc-dpz-plf-425.
      *                      *-----------------------------------------*
      *                      * Se anagrafica commerciale del cliente   *
      *                      * principale esistente                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A dipendenze dall'impostazione      *
      *                          *-------------------------------------*
           go to     acc-dpz-plf-600.
       acc-dpz-plf-430.
      *                      *-----------------------------------------*
      *                      * Se anagrafica commerciale del cliente   *
      *                      * principale non esistente                *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Messaggio di errore                 *
      *                          *-------------------------------------*
           move      "Manca l'anagrafica commerciale per il cliente     
      -              "          "         to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                          *-------------------------------------*
      *                          * A dipendenze dall'impostazione      *
      *                          *-------------------------------------*
           go to     acc-dpz-plf-600.
       acc-dpz-plf-440.
      *                  *---------------------------------------------*
      *                  * Se codice dipendenza diverso da spaces      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica commerciale per la   *
      *                      * dipendenza                              *
      *                      *-----------------------------------------*
           move      "D"                  to   w-let-arc-dcc-tle      .
           move      w-tes-arc-plf        to   w-let-arc-dcc-cod      .
           move      w-tes-dpz-plf        to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                      *-----------------------------------------*
      *                      * Memorizzazione valori letti da anagra-  *
      *                      * fica commerciale della dipendenza in    *
      *                      * dati per la dipendenza                  *
      *                      *-----------------------------------------*
           move      w-let-arc-dcc-rag    to   w-tes-dpz-plf-rag      .
           move      w-let-arc-dcc-via    to   w-tes-dpz-plf-via      .
           move      w-let-arc-dcc-loc    to   w-tes-dpz-plf-loc      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione valori relativi alla    *
      *                      * dipendenza                              *
      *                      *-----------------------------------------*
           perform   vis-dpz-plf-000      thru vis-dpz-plf-999        .
           perform   vis-dpz-plf-rag-000  thru vis-dpz-plf-rag-999    .
           perform   vis-dpz-plf-via-000  thru vis-dpz-plf-via-999    .
           perform   vis-dpz-plf-loc-000  thru vis-dpz-plf-loc-999    .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda dell'esito della   *
      *                      * lettura                                 *
      *                      *-----------------------------------------*
           if        w-let-arc-dcc-flg    =    spaces
                     go to acc-dpz-plf-445
           else      go to acc-dpz-plf-450.
       acc-dpz-plf-445.
      *                      *-----------------------------------------*
      *                      * Se anagrafica commerciale della dipen-  *
      *                      * denza esistente                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A dipendenze dall'impostazione      *
      *                          *-------------------------------------*
           go to     acc-dpz-plf-600.
       acc-dpz-plf-450.
      *                      *-----------------------------------------*
      *                      * Se anagrafica commerciale della dipen-  *
      *                      * denza non esistente                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Messaggio di errore                 *
      *                          *-------------------------------------*
           move      spaces               to   w-err-box-err-msg      .
           string    "Manca l'anagrafica commerciale per la dipendenza "
                                delimited by   size
                     "'"        delimited by   size
                     w-tes-dpz-plf
                                delimited by spaces
                     "'"        delimited by   size
                                          into w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                          *-------------------------------------*
      *                          * A dipendenze dall'impostazione      *
      *                          *-------------------------------------*
           go to     acc-dpz-plf-600.
       acc-dpz-plf-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dpz-plf-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-dpz-plf-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-dpz-plf-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-dpz-plf-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-dpz-plf-999.
       acc-dpz-plf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Dipendenza archivio per fattura-  *
      *    *                         zione                             *
      *    *-----------------------------------------------------------*
       vis-dpz-plf-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-dpz-plf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dpz-plf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Dipendenza archivio per fattura-  *
      *    *                         zione, ragione sociale            *
      *    *-----------------------------------------------------------*
       vis-dpz-plf-rag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-dpz-plf-rag    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dpz-plf-rag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Dipendenza archivio per fattura-  *
      *    *                         zione, indirizzo                  *
      *    *-----------------------------------------------------------*
       vis-dpz-plf-via-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-dpz-plf-via    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dpz-plf-via-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Dipendenza archivio per fattura-  *
      *    *                         zione, localita'                  *
      *    *-----------------------------------------------------------*
       vis-dpz-plf-loc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-dpz-plf-loc    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dpz-plf-loc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo fatturazione                    *
      *    *-----------------------------------------------------------*
       acc-tip-ftz-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-tip-ftz        to   w-sav-tip-ftz          .
       acc-tip-ftz-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-ftz-lun    to   v-car                  .
           move      w-exp-tip-ftz-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-ftz-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-tes-tip-ftz        =    99
                     move  01             to   v-num
           else if   w-tes-tip-ftz        =    01
                     move  02             to   v-num
           else if   w-tes-tip-ftz        =    02
                     move  03             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-tip-ftz-999.
       acc-tip-ftz-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  99             to   w-tes-tip-ftz
           else if   v-num                =    02
                     move  01             to   w-tes-tip-ftz
           else if   v-num                =    03
                     move  02             to   w-tes-tip-ftz
           else      move  zero           to   w-tes-tip-ftz          .
       acc-tip-ftz-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tip-ftz-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-ftz-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-tip-ftz-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-tip-ftz-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-tip-ftz-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-tip-ftz-999.
       acc-tip-ftz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tipo fatturazione                 *
      *    *-----------------------------------------------------------*
       vis-tip-ftz-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-ftz-lun    to   v-car                  .
           move      w-exp-tip-ftz-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-ftz-tbl    to   v-txt                  .
           if        w-tes-tip-ftz        =    99
                     move  01             to   v-num
           else if   w-tes-tip-ftz        =    01
                     move  02             to   v-num
           else if   w-tes-tip-ftz        =    02
                     move  03             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-ftz-999.
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
           move      "#SAV"               to   v-not                  .
           move      spaces               to   v-alf                  .
           move      "SNE"                to   v-msk                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                not  = spaces
                     go to acc-nok-reg-840.
           if        v-alf                =    "S"
                     move   "DO  "        to   v-key
           else if   v-alf                =    "E"
                     move   "E"           to   w-cnt-tus-acc-nok
                     go to acc-nok-reg-999
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
      *              * Se oltre numero riga massimo si esce con status *
      *              * di uscita "+"                                   *
      *              *-------------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-cor      .
           go to     acc-cor-reg-990.
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
           move      "#"                  to   w-cnt-sts-imp-cor      .
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
      *                  * Accettazione Codice cliente                 *
      *                  *---------------------------------------------*
           perform   acc-cod-cli-000      thru acc-cod-cli-999        .
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
      *                  * Tipo fornitura abituale                     *
      *                  *---------------------------------------------*
           perform   acc-rig-frn-000      thru acc-rig-frn-999        .
           if        w-cnt-tus-acc-rig    not  = spaces
                     go to acc-cor-reg-900.
       acc-cor-reg-250.
      *                  *---------------------------------------------*
      *                  * Codice archivio per la fatturazione         *
      *                  *---------------------------------------------*
           perform   acc-rig-arc-000      thru acc-rig-arc-999        .
           if        w-cnt-tus-acc-rig    not  = spaces
                     go to acc-cor-reg-900.
           if        v-key                =    "UP  "
                     go to acc-cor-reg-200.
       acc-cor-reg-300.
      *                  *---------------------------------------------*
      *                  * Codice dipendenza archivio per la fattura-  *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           perform   acc-rig-dpz-000      thru acc-rig-dpz-999        .
           if        w-cnt-tus-acc-rig    not  = spaces
                     go to acc-cor-reg-900.
           if        v-key                =    "UP  "
                     go to acc-cor-reg-250.
       acc-cor-reg-350.
      *                  *---------------------------------------------*
      *                  * Tipo fatturazione                           *
      *                  *---------------------------------------------*
           perform   acc-rig-ftz-000      thru acc-rig-ftz-999        .
           if        w-cnt-tus-acc-rig    not  = spaces
                     go to acc-cor-reg-900.
           if        v-key                =    "UP  "
                     go to acc-cor-reg-300.
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
      *                      * Decremento numero riga da accettare     *
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
      *                      *-----------------------------------------*
      *                      * Status di uscita a 'S'                  *
      *                      *-----------------------------------------*
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
      *                      * Ad accettazione ultimo record           *
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
      *                      * successiva a quella attuale             *
      *                      *-----------------------------------------*
           move      w-cnt-cor-nrg-dac    to   w-cnt-wrk-ctr-001      .
           add       w-lin-num-lin-vis    to   w-cnt-wrk-ctr-001      .
           subtract  1                    from w-cnt-wrk-ctr-001      .
           divide    w-lin-num-lin-vis    into w-cnt-wrk-ctr-001      .
           multiply  w-lin-num-lin-vis    by   w-cnt-wrk-ctr-001      .
           add       1                    to   w-cnt-wrk-ctr-001      .
      *                      *-----------------------------------------*
      *                      * Se questa e' maggiore del numero riga   *
      *                      * massimo si esce con '+'                 *
      *                      *-----------------------------------------*
           if        w-cnt-wrk-ctr-001    >    w-rlt-sup-max
                     move  "+"            to   w-cnt-tus-acc-cor
                     go to acc-cor-reg-990.
      *                      *-----------------------------------------*
      *                      * Altrimenti : ad accettazione della pri- *
      *                      * ma riga della pagina successiva         *
      *                      *-----------------------------------------*
           move      w-cnt-wrk-ctr-001    to   w-cnt-cor-nrg-dac      .
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
      *                      * Se si e' sulla prima pagina : si ignora *
      *                      *-----------------------------------------*
           if        w-cnt-cor-nrg-dac    not  > w-lin-num-lin-vis
                     go to acc-cor-reg-000.
      *                      *-----------------------------------------*
      *                      * Se non si e' sulla prima pagina : alla  *
      *                      * accettazione della prima riga della pa- *
      *                      * gina precedente                         *
      *                      *-----------------------------------------*
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
      *              * Erase linee impegnate, escluse fincature        *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      w-lin-pri-lin-vid    to   v-lin                  .
           move      w-lin-pri-lin-vid    to   v-lto                  .
           add       w-lin-num-lin-vis    to   v-lto                  .
           subtract  1                    from v-lto                  .
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
      *              * Visualizzazione numero pagina in corso di trat- *
      *              * tamento                                         *
      *              *-------------------------------------------------*
           perform   vis-cor-reg-npg-000  thru vis-cor-reg-npg-999    .
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
      *    * Visualizzazione pagina corpo                              *
      *    *                                                           *
      *    * Numero pagina corpo                                       *
      *    *-----------------------------------------------------------*
       vis-cor-reg-npg-000.
      *              *-------------------------------------------------*
      *              * Numero di pagina                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione pagina attuale               *
      *                  *---------------------------------------------*
           divide    w-lin-num-lin-vis    into w-car-eff-rig-ctr
                                        giving w-cnt-cor-pag-max
                                     remainder w-cnt-cor-pag-rem      .
           if        w-cnt-cor-pag-rem    >    zero
                     add 1                to   w-cnt-cor-pag-max      .
           divide    w-lin-num-lin-vis    into w-cnt-wrk-ctr-009
                                        giving w-cnt-cor-pag-att      .
      *                  *---------------------------------------------*
      *                  * Editing pagina attuale                      *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-cnt-cor-pag-att    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-cnt-cor-pag-ep1      .
      *                  *---------------------------------------------*
      *                  * Editing pagina finale                       *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-cnt-cor-pag-max    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-cnt-cor-pag-ep2      .
      *                  *---------------------------------------------*
      *                  * Composizione                                *
      *                  *---------------------------------------------*
           move      spaces               to   w-cnt-cor-pag-npa      .
           string    "["
                                delimited by   size
                     w-cnt-cor-pag-ep1
                                delimited by   spaces
                     "/"
                                delimited by   size
                     w-cnt-cor-pag-ep2
                                delimited by   spaces
                     "]"
                                delimited by   size
                                          into w-cnt-cor-pag-npa      .
      *                  *---------------------------------------------*
      *                  * Allineamento al centro                      *
      *                  *---------------------------------------------*
           move      09                   to   w-all-str-lun          .
           move      w-cnt-cor-pag-npa    to   w-all-str-alf          .
           perform   all-str-cen-000      thru all-str-cen-999        .
           move      w-all-str-alf        to   w-cnt-cor-pag-npa      .
      *                  *---------------------------------------------*
      *                  * Riempimento con '-'                         *
      *                  *---------------------------------------------*
           inspect   w-cnt-cor-pag-npa
                                     replacing all " " by   "-"       .
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
      *
           move      w-lin-pri-lin-vid    to   v-lin                  .
           add       w-lin-num-lin-vis    to   v-lin                  .
      *
           move      37                   to   v-pos                  .
           move      w-cnt-cor-pag-npa    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-cor-reg-npg-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-cor-reg-npg-999.
       vis-cor-reg-npg-999.
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
      *              *-------------------------------------------------*
      *              * Se visualizzazione numero d'ordine riga         *
      *              *-------------------------------------------------*
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
       vis-lin-cor-440.
      *                  *---------------------------------------------*
      *                  * Editing Codice cliente                      *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      w-rig-cod-cli        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-lin-imm-cod-cli      .
       vis-lin-cor-460.
      *                  *---------------------------------------------*
      *                  * Ragione sociale cliente                     *
      *                  *---------------------------------------------*
           move      w-rig-cod-cli-rag    to   w-lin-imm-rag-cli      .
      *                      *-----------------------------------------*
      *                      * Se si tratta di una dipendenza si so-   *
      *                      * vrascrivono gli ultimi 5 caratteri con  *
      *                      * uno spazio e con il codice dipendenza   *
      *                      *-----------------------------------------*
           if        w-rig-dpz-cli        =    spaces
                     go to vis-lin-cor-480.
           move      spaces               to   w-lin-imm-rag-fil      .
           move      w-rig-dpz-cli        to   w-lin-imm-rag-dpz      .
       vis-lin-cor-480.
      *                  *---------------------------------------------*
      *                  * Tipo fornitura                              *
      *                  *---------------------------------------------*
           if        w-rig-tip-frn        =    01
                     move  "Diretta  "    to   w-lin-imm-tip-frn
           else if   w-rig-tip-frn        =    21
                     move  "Gruppo Ac"    to   w-lin-imm-tip-frn
           else      move  spaces         to   w-lin-imm-tip-frn      .
       vis-lin-cor-500.
      *                  *---------------------------------------------*
      *                  * Codice del capogruppo                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione preliminare             *
      *                      *-----------------------------------------*
           move      spaces               to   w-lin-imm-arc-plf      .
      *                      *-----------------------------------------*
      *                      * Test se codice a zero                   *
      *                      *-----------------------------------------*
           if        w-rig-arc-plf        =    zero
                     go to vis-lin-cor-800.
      *                      *-----------------------------------------*
      *                      * Editing del codice archivio             *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      w-rig-arc-plf        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Test se presente il codice dipendenza   *
      *                      *-----------------------------------------*
           if        w-rig-dpz-plf        not  = spaces
                     go to vis-lin-cor-520.
       vis-lin-cor-510.
      *                          *-------------------------------------*
      *                          * Se non e' presente il codice dipen- *
      *                          * denza                               *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Codice archivio editato in      *
      *                              * campo da visualizzare           *
      *                              *---------------------------------*
           move      v-edt                to   w-lin-imm-arc-plf      .
      *                              *---------------------------------*
      *                              * Allineamento a destra           *
      *                              *---------------------------------*
           move      12                   to   w-all-str-lun          .
           move      w-lin-imm-arc-plf    to   w-all-str-alf          .
           perform   all-str-adx-000      thru all-str-adx-999        .
           move      w-all-str-alf        to   w-lin-imm-arc-plf      .
      *                              *---------------------------------*
      *                              * A parametri per visualizzazione *
      *                              *---------------------------------*
           go to     vis-lin-cor-800.
       vis-lin-cor-520.
      *                          *-------------------------------------*
      *                          * Se presente il codice dipendenza    *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Concatenamento con codice di-   *
      *                              * pendenza                        *
      *                              *---------------------------------*
           move      12                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      v-edt                to   w-all-str-cat (1)      .
           move      "-"                  to   w-all-str-cat (2)      .
           move      w-rig-dpz-plf        to   w-all-str-cat (3)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           move      w-all-str-alf        to   w-lin-imm-arc-plf      .
      *                              *---------------------------------*
      *                              * A parametri per visualizzazione *
      *                              *---------------------------------*
           go to     vis-lin-cor-800.
       vis-lin-cor-800.
      *                  *---------------------------------------------*
      *                  * Parametri per visualizzazione               *
      *                  *---------------------------------------------*
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
           move      14                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Fincatura : Linea 04                            *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      " N.R.          Codice e ragione sociale cliente   
      -              "        Fornitura  Capogruppo "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Fincatura : Linea 05                            *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "-----  -------------------------------------------
      -              "------  --------- ------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Fincatura : Linea 14                            *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      14                   to   v-lin                  .
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
      *              * Codice cliente                                  *
      *              *-------------------------------------------------*
           perform   vis-cod-cli-000      thru vis-cod-cli-999        .
           perform   vis-dpz-cli-000      thru vis-dpz-cli-999        .
           perform   vis-cod-cli-rag-000  thru vis-cod-cli-rag-999    .
      *              *-------------------------------------------------*
      *              * Tipo fornitura abituale                         *
      *              *-------------------------------------------------*
           perform   vis-rig-frn-000      thru vis-rig-frn-999        .
      *              *-------------------------------------------------*
      *              * Codice archivio per la fatturazione             *
      *              *-------------------------------------------------*
           perform   vis-rig-arc-000      thru vis-rig-arc-999        .
           perform   vis-rig-arc-rag-000  thru vis-rig-arc-rag-999    .
      *              *-------------------------------------------------*
      *              * Codice dipendenza archivio per la fatturazione  *
      *              *-------------------------------------------------*
           perform   vis-rig-dpz-000      thru vis-rig-dpz-999        .
           perform   vis-rig-dpz-rag-000  thru vis-rig-dpz-rag-999    .
           perform   vis-rig-dpz-via-000  thru vis-rig-dpz-via-999    .
           perform   vis-rig-dpz-loc-000  thru vis-rig-dpz-loc-999    .
      *              *-------------------------------------------------*
      *              * Tipo fatturazione                               *
      *              *-------------------------------------------------*
           perform   vis-rig-ftz-000      thru vis-rig-ftz-999        .
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
           move      15                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-rig-cor-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts per riga corpo espansa  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice cliente                              *
      *                  *---------------------------------------------*
           perform   pmt-cod-cli-000      thru pmt-cod-cli-999        .
      *                  *---------------------------------------------*
      *                  * Tipo fornitura abituale                     *
      *                  *---------------------------------------------*
           perform   pmt-rig-frn-000      thru pmt-rig-frn-999        .
      *                  *---------------------------------------------*
      *                  * Codice archivio per la fatturazione         *
      *                  *---------------------------------------------*
           perform   pmt-rig-arc-000      thru pmt-rig-arc-999        .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza archivio per la fattura-  *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           perform   pmt-rig-dpz-000      thru pmt-rig-dpz-999        .
      *                  *---------------------------------------------*
      *                  * Tipo fatturazione                           *
      *                  *---------------------------------------------*
           perform   pmt-rig-ftz-000      thru pmt-rig-ftz-999        .
       pmt-rig-cor-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice cliente                   *
      *    *-----------------------------------------------------------*
       pmt-cod-cli-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice cliente     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tipo fornitura abituale          *
      *    *-----------------------------------------------------------*
       pmt-rig-frn-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Fornitura abituale :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-rig-frn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice archivio per fatturazione *
      *    *-----------------------------------------------------------*
       pmt-rig-arc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Cliente capogruppo :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-rig-arc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Dipendenza archivio per fattura- *
      *    * zione                                                     *
      *    *-----------------------------------------------------------*
       pmt-rig-dpz-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Sua dipendenza     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-rig-dpz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tipo fatturazione                *
      *    *-----------------------------------------------------------*
       pmt-rig-ftz-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo fatturazione  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-rig-ftz-999.
           exit.

      *    *===========================================================*
      *    * Accettazione 1. campo riga corpo espansa : Codice cliente *
      *    *-----------------------------------------------------------*
       acc-cod-cli-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-rig-cod-cli        to   w-sav-cod-cli          .
       acc-cod-cli-100.
      *              *-------------------------------------------------*
      *              * Ripristino valore precedente                    *
      *              *-------------------------------------------------*
           move      w-sav-cod-cli        to   w-rig-cod-cli          .
       acc-cod-cli-125.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Parametri generali di accettazione          *
      *                  *---------------------------------------------*
           move      "AC"                 to   w-cod-mne-dcc-ope      .
           move      w-rig-cod-cli        to   w-cod-mne-dcc-cod      .
           move      15                   to   w-cod-mne-dcc-lin      .
           move      22                   to   w-cod-mne-dcc-pos      .
           move      zero                 to   w-cod-mne-dcc-rln      .
           move      zero                 to   w-cod-mne-dcc-rps      .
           move      zero                 to   w-cod-mne-dcc-vln      .
           move      zero                 to   w-cod-mne-dcc-vps      .
           move      zero                 to   w-cod-mne-dcc-lln      .
           move      zero                 to   w-cod-mne-dcc-lps      .
           move      "<B"                 to   v-edm                  .
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
      *                      * Insr : sempre ammesso                   *
      *                      *-----------------------------------------*
           move      "INSR"               to   v-pfk (04)             .
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
      *                      * Nxsc : sempre ammesso                   *
      *                      *-----------------------------------------*
           move      "NXSC"               to   v-pfk (08)             .
      *                      *-----------------------------------------*
      *                      * Back : sempre ammesso, a meno che non   *
      *                      *        si sia gia' sulla prima riga     *
      *                      *-----------------------------------------*
           if        w-cnt-cor-nrg-dac    not  = 1
                     move  "BACK"         to   v-pfk (09)
           else      move  spaces         to   v-pfk (09)             .
      *                      *-----------------------------------------*
      *                      * Tab  : sempre ammesso, a meno che non   *
      *                      *        si sia gia' sull'ultima riga     *
      *                      *-----------------------------------------*
           if        w-cnt-cor-nrg-dac    not  = w-rlt-sup-max
                     move  "TAB "         to   v-pfk (10)
           else      move  spaces         to   v-pfk (10)             .
      *                      *-----------------------------------------*
      *                      * Slct : sempre ammesso, a meno che non   *
      *                      *        si sia gia' sull'unica riga      *
      *                      *-----------------------------------------*
           if        w-rlt-sup-max        not  < 2
                     move  "SLCT"         to   v-pfk (11)
           else      move  spaces         to   v-pfk (11)             .
      *                      *-----------------------------------------*
      *                      * Expd : sempre ammesso                   *
      *                      *-----------------------------------------*
           move      "EXPD"               to   v-pfk (15)             .
       acc-cod-cli-150.
      *                  *---------------------------------------------*
      *                  * Esecuzione accettazione                     *
      *                  *---------------------------------------------*
           perform   cod-mne-dcc-cll-000  thru cod-mne-dcc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-dcc-foi-000  thru cod-mne-dcc-foi-999    .
       acc-cod-cli-152.
           perform   cod-mne-dcc-cll-000  thru cod-mne-dcc-cll-999    .
           if        w-cod-mne-dcc-ope    =    "F+"
                     go to acc-cod-cli-154.
           if        w-cod-mne-dcc-ope    =    "AC"
                     go to acc-cod-cli-156.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-cli-154.
           perform   cod-mne-dcc-foi-000  thru cod-mne-dcc-foi-999    .
           go to     acc-cod-cli-152.
       acc-cod-cli-156.
           move      w-cod-mne-dcc-cod    to   v-num                  .
       acc-cod-cli-200.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-rig
                     go to acc-cod-cli-999.
       acc-cod-cli-225.
      *              *-------------------------------------------------*
      *              * Test se Return                                  *
      *              *-------------------------------------------------*
           if        v-key                =    spaces
                     go to acc-cod-cli-400.
       acc-cod-cli-250.
      *              *-------------------------------------------------*
      *              * Se Slct                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to acc-cod-cli-300.
       acc-cod-cli-255.
      *                      *-----------------------------------------*
      *                      * Comodo per selezione                    *
      *                      *-----------------------------------------*
           move      v-num                to   w-cnt-slc-rap-num      .
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
                     go to acc-cod-cli-100.
      *                          *-------------------------------------*
      *                          * Se numero riga impostato pari a nu- *
      *                          * numero riga attuale : reimpostazio- *
      *                          * ne                                  *
      *                          *-------------------------------------*
           if        w-cnt-slc-rap-num    =    w-cnt-cor-nrg-dac
                     go to acc-cod-cli-100.
      *                          *-------------------------------------*
      *                          * Preparazione parametri e uscita     *
      *                          *-------------------------------------*
           move      w-cnt-slc-rap-num    to   w-cnt-slc-num-rig      .
           move      "."                  to   w-cnt-tus-acc-rig      .
           go to     acc-cod-cli-999.
       acc-cod-cli-300.
      *              *-------------------------------------------------*
      *              * Se Expd                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "EXPD"
                     go to acc-cod-cli-320.
           if        w-rig-cod-cli        =    zero
                     go to acc-cod-cli-320.
      *                  *---------------------------------------------*
      *                  * Box di espansione                           *
      *                  *---------------------------------------------*
           move      w-rig-cod-cli        to   w-rou-exp-ana-cod      .
           move      w-rig-dpz-cli        to   w-rou-exp-ana-dpz      .
           perform   rou-exp-ana-000      thru rou-exp-ana-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-cod-cli-100.
       acc-cod-cli-320.
      *              *-------------------------------------------------*
      *              * Se premuto un altro tasto funzione non deve es- *
      *              * sere avvenuta variazione del campo di imposta-  *
      *              * zione                                           *
      *              *-------------------------------------------------*
           if        v-mod                not  = spaces
                     go to acc-cod-cli-100.
      *              *-------------------------------------------------*
      *              * Se Up                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "UP  "
                     move  "U"            to   w-cnt-tus-acc-rig
                     go to acc-cod-cli-999.
      *              *-------------------------------------------------*
      *              * Se Down                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DOWN"
                     move  "D"            to   w-cnt-tus-acc-rig
                     go to acc-cod-cli-999.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-rig
                             go to acc-cod-cli-999
                     else    go to acc-cod-cli-100.
      *              *-------------------------------------------------*
      *              * Se Tab                                          *
      *              *-------------------------------------------------*
           if        v-key                =    "TAB "
                     move  "T"            to   w-cnt-tus-acc-rig
                     go to acc-cod-cli-999.
      *              *-------------------------------------------------*
      *              * Se Back                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "BACK"
                     move  "B"            to   w-cnt-tus-acc-rig
                     go to acc-cod-cli-999.
      *              *-------------------------------------------------*
      *              * Se Nxsc                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "NXSC"
                     move  "N"            to   w-cnt-tus-acc-rig
                     go to acc-cod-cli-999.
      *              *-------------------------------------------------*
      *              * Se Prsc                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "PRSC"
                     move  "P"            to   w-cnt-tus-acc-rig
                     go to acc-cod-cli-999.
       acc-cod-cli-400.
      *              *-------------------------------------------------*
      *              * Se Return                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se variazione del valore di default : a re- *
      *                  * impostazione                                *
      *                  *---------------------------------------------*
           if        v-mod                not  = spaces
                     go to acc-cod-cli-100.
       acc-cod-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice cliente                          *
      *    *-----------------------------------------------------------*
       vis-cod-cli-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      15                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-rig-cod-cli        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice dipendenza del cliente           *
      *    *-----------------------------------------------------------*
       vis-dpz-cli-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-rig-dpz-cli        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dpz-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice cliente, ragione sociale         *
      *    *-----------------------------------------------------------*
       vis-cod-cli-rag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-rig-cod-cli-rag    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-cli-rag-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo fornitura abituale              *
      *    *-----------------------------------------------------------*
       acc-rig-frn-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-rig-tip-frn        to   w-sav-tip-frn          .
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        w-rig-tip-frn        not  = zero
                     go to acc-rig-frn-100.
           move      01                   to   w-rig-tip-frn          .
       acc-rig-frn-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-rig-frn-lun    to   v-car                  .
           move      w-exp-rig-frn-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      16                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-exp-rig-frn-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-rig-tip-frn        =    01
                     move  01             to   v-num
           else if   w-rig-tip-frn        =    21
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-rig
                     go to acc-rig-frn-999.
       acc-rig-frn-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  01             to   w-rig-tip-frn
           else if   v-num                =    02
                     move  21             to   w-rig-tip-frn
           else      move  zero           to   w-rig-tip-frn          .
       acc-rig-frn-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : reimpostazione, a meno che non si *
      *                  * sia in Up, e che il valore precedente non   *
      *                  * fosse gia' a zero                           *
      *                  *---------------------------------------------*
           if        w-rig-tip-frn        not  = zero
                     go to acc-rig-frn-600.
           if        v-key                not  = "UP  "
                     go to acc-rig-frn-100.
           if        w-sav-tip-frn        =    zero
                     go to acc-rig-frn-600
           else      go to acc-rig-frn-100.
       acc-rig-frn-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-rig-frn-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-rig-000
                                          thru cnt-tdo-rig-999
                     if      w-cnt-tdo-rig-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-rig
                             go to acc-rig-frn-999
                     else    move  spaces to   w-cnt-tdo-rig-flg
                             go to acc-rig-frn-100.
       acc-rig-frn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tipo fornitura abituale           *
      *    *-----------------------------------------------------------*
       vis-rig-frn-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-rig-frn-lun    to   v-car                  .
           move      w-exp-rig-frn-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      16                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-exp-rig-frn-tbl    to   v-txt                  .
           if        w-rig-tip-frn        =    01
                     move  01             to   v-num
           else if   w-rig-tip-frn        =    21
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rig-frn-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice archivio per fatturazione     *
      *    *-----------------------------------------------------------*
       acc-rig-arc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-rig-arc-plf        to   w-sav-arc-plf          .
       acc-rig-arc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-dcc-ope      .
           move      w-rig-arc-plf        to   w-cod-mne-dcc-cod      .
           move      17                   to   w-cod-mne-dcc-lin      .
           move      22                   to   w-cod-mne-dcc-pos      .
           move      17                   to   w-cod-mne-dcc-rln      .
           move      41                   to   w-cod-mne-dcc-rps      .
           move      zero                 to   w-cod-mne-dcc-vln      .
           move      zero                 to   w-cod-mne-dcc-vps      .
           move      zero                 to   w-cod-mne-dcc-lln      .
           move      zero                 to   w-cod-mne-dcc-lps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-dcc-cll-000  thru cod-mne-dcc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-dcc-foi-000  thru cod-mne-dcc-foi-999    .
       acc-rig-arc-110.
           perform   cod-mne-dcc-cll-000  thru cod-mne-dcc-cll-999    .
           if        w-cod-mne-dcc-ope    =    "F+"
                     go to acc-rig-arc-115.
           if        w-cod-mne-dcc-ope    =    "AC"
                     go to acc-rig-arc-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-rig-arc-115.
           perform   cod-mne-dcc-foi-000  thru cod-mne-dcc-foi-999    .
           go to     acc-rig-arc-110.
       acc-rig-arc-120.
           move      w-cod-mne-dcc-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-rig
                     go to acc-rig-arc-999.
       acc-rig-arc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-rig-arc-plf          .
       acc-rig-arc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-rig-arc-410.
      *                  *---------------------------------------------*
      *                  * Lettura file [cli]                          *
      *                  *---------------------------------------------*
           move      w-rig-arc-plf        to   w-let-arc-cli-cod      .
           perform   let-arc-cli-000      thru let-arc-cli-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione ragione sociale              *
      *                  *---------------------------------------------*
           move      w-let-arc-cli-rag    to   w-rig-arc-plf-rag      .
           move      w-let-arc-cli-via    to   w-rig-arc-plf-via      .
           move      w-let-arc-cli-loc    to   w-rig-arc-plf-loc      .
           move      w-let-arc-cli-cge    to   w-rig-arc-plf-cge      .
       acc-rig-arc-420.
      *                  *---------------------------------------------*
      *                  * Visualizzazione ragione sociale da anagra-  *
      *                  * fica contabile                              *
      *                  *---------------------------------------------*
           perform   vis-rig-arc-rag-000  thru vis-rig-arc-rag-999    .
       acc-rig-arc-430.
      *                  *---------------------------------------------*
      *                  * Se cliente non esistente                    *
      *                  *---------------------------------------------*
           if        w-let-arc-cli-flg    =    spaces
                     go to acc-rig-arc-440.
       acc-rig-arc-432.
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione di-   *
      *                      * pendenza del cliente                    *
      *                      *-----------------------------------------*
           if        w-rig-dpz-plf        =    spaces and
                     w-rig-dpz-plf-rag    =    spaces and
                     w-rig-dpz-plf-via    =    spaces and
                     w-rig-dpz-plf-loc    =    spaces
                     go to acc-rig-arc-434.
           move      spaces               to   w-rig-dpz-plf          .
           move      spaces               to   w-rig-dpz-plf-rag      .
           move      spaces               to   w-rig-dpz-plf-via      .
           move      spaces               to   w-rig-dpz-plf-loc      .
           perform   vis-rig-dpz-000      thru vis-rig-dpz-999        .
           perform   vis-rig-dpz-rag-000  thru vis-rig-dpz-rag-999    .
           perform   vis-rig-dpz-via-000  thru vis-rig-dpz-via-999    .
           perform   vis-rig-dpz-loc-000  thru vis-rig-dpz-loc-999    .
       acc-rig-arc-434.
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-rig-arc-100.
       acc-rig-arc-440.
      *                  *---------------------------------------------*
      *                  * Se codice a zero                            *
      *                  *---------------------------------------------*
           if        w-rig-arc-plf        not  = zero
                     go to acc-rig-arc-450.
       acc-rig-arc-442.
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione di-   *
      *                      * pendenza del cliente                    *
      *                      *-----------------------------------------*
           if        w-rig-dpz-plf        =    spaces and
                     w-rig-dpz-plf-rag    =    spaces and
                     w-rig-dpz-plf-via    =    spaces and
                     w-rig-dpz-plf-loc    =    spaces
                     go to acc-rig-arc-444.
           move      spaces               to   w-rig-dpz-plf          .
           move      spaces               to   w-rig-dpz-plf-rag      .
           move      spaces               to   w-rig-dpz-plf-via      .
           move      spaces               to   w-rig-dpz-plf-loc      .
           perform   vis-rig-dpz-000      thru vis-rig-dpz-999        .
           perform   vis-rig-dpz-rag-000  thru vis-rig-dpz-rag-999    .
           perform   vis-rig-dpz-via-000  thru vis-rig-dpz-via-999    .
           perform   vis-rig-dpz-loc-000  thru vis-rig-dpz-loc-999    .
       acc-rig-arc-444.
      *                      *-----------------------------------------*
      *                      * A dipendenze dall'impostazione          *
      *                      *-----------------------------------------*
           go to     acc-rig-arc-600.
       acc-rig-arc-450.
      *                  *---------------------------------------------*
      *                  * Se codice diverso da zero                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del confronto con  *
      *                      * il valore precedente                    *
      *                      *-----------------------------------------*
           if        w-rig-arc-plf        =    w-sav-arc-plf
                     go to acc-rig-arc-455
           else      go to acc-rig-arc-480.
       acc-rig-arc-455.
      *                      *-----------------------------------------*
      *                      * Se valore impostato pari al valore pre- *
      *                      * cedente                                 *
      *                      *-----------------------------------------*
       acc-rig-arc-457.
      *                          *-------------------------------------*
      *                          * Lettura anagrafica commerciale del  *
      *                          * cliente principale                  *
      *                          *-------------------------------------*
           move      "C"                  to   w-let-arc-dcc-tle      .
           move      w-rig-arc-plf        to   w-let-arc-dcc-cod      .
           move      spaces               to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                          *-------------------------------------*
      *                          * Deviazione a seconda dell'esito     *
      *                          * della lettura                       *
      *                          *-------------------------------------*
           if        w-let-arc-dcc-flg    =    spaces
                     go to acc-rig-arc-467.
       acc-rig-arc-459.
      *                          *-------------------------------------*
      *                          * Se anagrafica commerciale del cli-  *
      *                          * ente principale non esistente       *
      *                          *-------------------------------------*
       acc-rig-arc-461.
      *                              *---------------------------------*
      *                              * Normalizzazione e visualizza-   *
      *                              * zione dei dati relativi alla    *
      *                              * dipendenza                      *
      *                              *---------------------------------*
           if        w-rig-dpz-plf        =    spaces and
                     w-rig-dpz-plf-rag    =    spaces and
                     w-rig-dpz-plf-via    =    spaces and
                     w-rig-dpz-plf-loc    =    spaces
                     go to acc-rig-arc-463.
           move      spaces               to   w-rig-dpz-plf          .
           move      spaces               to   w-rig-dpz-plf-rag      .
           move      spaces               to   w-rig-dpz-plf-via      .
           move      spaces               to   w-rig-dpz-plf-loc      .
           perform   vis-rig-dpz-000      thru vis-rig-dpz-999        .
           perform   vis-rig-dpz-rag-000  thru vis-rig-dpz-rag-999    .
           perform   vis-rig-dpz-via-000  thru vis-rig-dpz-via-999    .
           perform   vis-rig-dpz-loc-000  thru vis-rig-dpz-loc-999    .
       acc-rig-arc-463.
      *                              *---------------------------------*
      *                              * Messaggio di errore             *
      *                              *---------------------------------*
           move      "Manca l'anagrafica commerciale per il cliente     
      -              "          "         to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       acc-rig-arc-465.
      *                              *---------------------------------*
      *                              * A reimpostazione                *
      *                              *---------------------------------*
           go to     acc-rig-arc-100.
       acc-rig-arc-467.
      *                          *-------------------------------------*
      *                          * Se anagrafica commerciale del cli-  *
      *                          * ente principale esistente           *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Memorizzazione valori letti da  *
      *                              * anagrafica commerciale cliente  *
      *                              * principale in dati per la di-   *
      *                              * pendenza                        *
      *                              *---------------------------------*
           move      w-let-arc-dcc-rag    to   w-rig-dpz-plf-rag      .
           move      w-let-arc-dcc-via    to   w-rig-dpz-plf-via      .
           move      w-let-arc-dcc-loc    to   w-rig-dpz-plf-loc      .
      *                              *---------------------------------*
      *                              * Deviazione a seconda se il co-  *
      *                              * dice dipendenza e' a spaces op- *
      *                              * pure diverso da spaces          *
      *                              *---------------------------------*
           if        w-rig-dpz-plf        =    spaces
                     go to acc-rig-arc-469
           else      go to acc-rig-arc-471.
       acc-rig-arc-469.
      *                              *---------------------------------*
      *                              * Se codice dipendenza a spaces   *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Visualizzazione valori re-  *
      *                                  * lativi alla dipendenza      *
      *                                  *-----------------------------*
           perform   vis-rig-dpz-000      thru vis-rig-dpz-999        .
           perform   vis-rig-dpz-rag-000  thru vis-rig-dpz-rag-999    .
           perform   vis-rig-dpz-via-000  thru vis-rig-dpz-via-999    .
           perform   vis-rig-dpz-loc-000  thru vis-rig-dpz-loc-999    .
      *                                  *-----------------------------*
      *                                  * A ulteriori controlli su    *
      *                                  * impostazione                *
      *                                  *-----------------------------*
           go to     acc-rig-arc-500.
       acc-rig-arc-471.
      *                              *---------------------------------*
      *                              * Se codice dipendenza diverso da *
      *                              * spaces                          *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Lettura anagrafica commer-  *
      *                                  * ciale della dipendenza      *
      *                                  *-----------------------------*
           move      "D"                  to   w-let-arc-dcc-tle      .
           move      w-rig-arc-plf        to   w-let-arc-dcc-cod      .
           move      w-rig-dpz-plf        to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                                  *-----------------------------*
      *                                  * Deviazione a seconda del-   *
      *                                  * l'esito della lettura       *
      *                                  *-----------------------------*
           if        w-let-arc-dcc-flg    =    spaces
                     go to acc-rig-arc-473
           else      go to acc-rig-arc-475.
       acc-rig-arc-473.
      *                                  *-----------------------------*
      *                                  * Se anagrafica commerciale   *
      *                                  * della dipendenza esistente  *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Memorizzazione valori   *
      *                                      * letti da anagrafica     *
      *                                      * commerciale della di-   *
      *                                      * pendenza                *
      *                                      *-------------------------*
           move      w-let-arc-dcc-rag    to   w-rig-dpz-plf-rag      .
           move      w-let-arc-dcc-via    to   w-rig-dpz-plf-via      .
           move      w-let-arc-dcc-loc    to   w-rig-dpz-plf-loc      .
      *                                      *-------------------------*
      *                                      * Visualizzazione valori  *
      *                                      * relativi alla dipenden- *
      *                                      * za                      *
      *                                      *-------------------------*
           perform   vis-rig-dpz-000      thru vis-rig-dpz-999        .
           perform   vis-rig-dpz-rag-000  thru vis-rig-dpz-rag-999    .
           perform   vis-rig-dpz-via-000  thru vis-rig-dpz-via-999    .
           perform   vis-rig-dpz-loc-000  thru vis-rig-dpz-loc-999    .
      *                                      *-------------------------*
      *                                      * A ulteriori controlli   *
      *                                      * su impostazione         *
      *                                      *-------------------------*
           go to     acc-rig-arc-500.
       acc-rig-arc-475.
      *                                  *-----------------------------*
      *                                  * Se anagrafica commerciale   *
      *                                  * della dipendenza non esi-   *
      *                                  * stente                      *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Memorizzazione valori   *
      *                                      * letti da anagrafica     *
      *                                      * commerciale della di-   *
      *                                      * pendenza                *
      *                                      *-------------------------*
           move      w-let-arc-dcc-rag    to   w-rig-dpz-plf-rag      .
           move      w-let-arc-dcc-via    to   w-rig-dpz-plf-via      .
           move      w-let-arc-dcc-loc    to   w-rig-dpz-plf-loc      .
      *                                      *-------------------------*
      *                                      * Visualizzazione valori  *
      *                                      * relativi alla dipenden- *
      *                                      * za                      *
      *                                      *-------------------------*
           perform   vis-rig-dpz-000      thru vis-rig-dpz-999        .
           perform   vis-rig-dpz-rag-000  thru vis-rig-dpz-rag-999    .
           perform   vis-rig-dpz-via-000  thru vis-rig-dpz-via-999    .
           perform   vis-rig-dpz-loc-000  thru vis-rig-dpz-loc-999    .
      *                                      *-------------------------*
      *                                      * Messaggio di errore     *
      *                                      *-------------------------*
           move      spaces               to   w-err-box-err-msg      .
           string    "Manca l'anagrafica commerciale per la dipendenza "
                                delimited by   size
                     "'"        delimited by   size
                     w-rig-dpz-plf
                                delimited by spaces
                     "'"        delimited by   size
                                          into w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                                      *-------------------------*
      *                                      * A reimpostazione        *
      *                                      *-------------------------*
           go to     acc-rig-arc-100.
       acc-rig-arc-480.
      *                      *-----------------------------------------*
      *                      * Se valore impostato diverso dal valore  *
      *                      * precedente                              *
      *                      *-----------------------------------------*
       acc-rig-arc-482.
      *                              *---------------------------------*
      *                              * Normalizzazione e visualizza-   *
      *                              * zione dei dati relativi alla    *
      *                              * dipendenza                      *
      *                              *---------------------------------*
           if        w-rig-dpz-plf        =    spaces and
                     w-rig-dpz-plf-rag    =    spaces and
                     w-rig-dpz-plf-via    =    spaces and
                     w-rig-dpz-plf-loc    =    spaces
                     go to acc-rig-arc-484.
           move      spaces               to   w-rig-dpz-plf          .
           move      spaces               to   w-rig-dpz-plf-rag      .
           move      spaces               to   w-rig-dpz-plf-via      .
           move      spaces               to   w-rig-dpz-plf-loc      .
           perform   vis-rig-dpz-000      thru vis-rig-dpz-999        .
           perform   vis-rig-dpz-rag-000  thru vis-rig-dpz-rag-999    .
           perform   vis-rig-dpz-via-000  thru vis-rig-dpz-via-999    .
           perform   vis-rig-dpz-loc-000  thru vis-rig-dpz-loc-999    .
       acc-rig-arc-484.
      *                              *---------------------------------*
      *                              * Riaggancio                      *
      *                              *---------------------------------*
           go to     acc-rig-arc-457.
       acc-rig-arc-500.
      *                      *-----------------------------------------*
      *                      * Controllo che il codice cliente impo-   *
      *                      * stato non sia pari a quello di testata  *
      *                      *-----------------------------------------*
           if        w-rig-arc-plf        =    w-rig-cod-cli
                     go to acc-rig-arc-100.
       acc-rig-arc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-rig-arc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-rig-000
                                          thru cnt-tdo-rig-999
                     if      w-cnt-tdo-rig-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-rig
                             go to acc-rig-arc-999
                     else    move  spaces to   w-cnt-tdo-rig-flg
                             go to acc-rig-arc-100.
       acc-rig-arc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice archivio per fatturazione  *
      *    *-----------------------------------------------------------*
       vis-rig-arc-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      17                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-rig-arc-plf        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rig-arc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice archivio per fatturazione, *
      *    *                         ragione sociale                   *
      *    *-----------------------------------------------------------*
       vis-rig-arc-rag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-rig-arc-plf-rag
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rig-arc-rag-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo teststa : Dipendenza archivio per fat- *
      *    * turazione                                                 *
      *    *-----------------------------------------------------------*
       acc-rig-dpz-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-rig-dpz-025.
      *                  *---------------------------------------------*
      *                  * Se dipendenza : uscita                      *
      *                  *---------------------------------------------*
           if        w-rig-dpz-cli        not  = spaces
                     go to acc-rig-dpz-999.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-rig-arc-plf        =    zero
                     go to acc-rig-dpz-999.
       acc-rig-dpz-100.
      *              *-------------------------------------------------*
      *              * Salvataggio valore precedente                   *
      *              *-------------------------------------------------*
           move      w-rig-dpz-plf        to   w-sav-dpz-plf          .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-dcc-ope      .
           move      w-rig-arc-plf        to   w-cod-cod-dcc-cli      .
           move      w-rig-dpz-plf        to   w-cod-cod-dcc-cod      .
           move      18                   to   w-cod-cod-dcc-lin      .
           move      22                   to   w-cod-cod-dcc-pos      .
           move      18                   to   w-cod-cod-dcc-rln      .
           move      41                   to   w-cod-cod-dcc-rps      .
           move      19                   to   w-cod-cod-dcc-vln      .
           move      41                   to   w-cod-cod-dcc-vps      .
           move      20                   to   w-cod-cod-dcc-lln      .
           move      41                   to   w-cod-cod-dcc-lps      .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-cod-dcc-cll-000  thru cod-cod-dcc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-dcc-foi-000  thru cod-cod-dcc-foi-999    .
       acc-rig-dpz-110.
           perform   cod-cod-dcc-cll-000  thru cod-cod-dcc-cll-999    .
           if        w-cod-cod-dcc-ope    =    "F+"
                     go to acc-rig-dpz-115.
           if        w-cod-cod-dcc-ope    =    "AC"
                     go to acc-rig-dpz-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-rig-dpz-115.
           perform   cod-cod-dcc-foi-000  thru cod-cod-dcc-foi-999    .
           go to     acc-rig-dpz-110.
       acc-rig-dpz-120.
           move      w-cod-cod-dcc-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-rig
                     go to acc-rig-dpz-999.
       acc-rig-dpz-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-rig-dpz-plf          .
       acc-rig-dpz-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-rig-dpz-410.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se codice sipendenza   *
      *                  * a spaces oppure no                          *
      *                  *---------------------------------------------*
           if        w-rig-dpz-plf        =    spaces
                     go to acc-rig-dpz-420
           else      go to acc-rig-dpz-440.
       acc-rig-dpz-420.
      *                  *---------------------------------------------*
      *                  * Se codice dipendenza a spaces               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica commerciale del cli- *
      *                      * ente principale                         *
      *                      *-----------------------------------------*
           move      "C"                  to   w-let-arc-dcc-tle      .
           move      w-rig-arc-plf        to   w-let-arc-dcc-cod      .
           move      spaces               to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                      *-----------------------------------------*
      *                      * Memorizzazione valori letti da anagra-  *
      *                      * fica commerciale cliente principale in  *
      *                      * dati per la dipendenza                  *
      *                      *-----------------------------------------*
           move      w-let-arc-dcc-rag    to   w-rig-dpz-plf-rag      .
           move      w-let-arc-dcc-via    to   w-rig-dpz-plf-via      .
           move      w-let-arc-dcc-loc    to   w-rig-dpz-plf-loc      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione valori relativi alla    *
      *                      * dipendenza                              *
      *                      *-----------------------------------------*
           perform   vis-rig-dpz-000      thru vis-rig-dpz-999        .
           perform   vis-rig-dpz-rag-000  thru vis-rig-dpz-rag-999    .
           perform   vis-rig-dpz-via-000  thru vis-rig-dpz-via-999    .
           perform   vis-rig-dpz-loc-000  thru vis-rig-dpz-loc-999    .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda dell'esito della   *
      *                      * lettura                                 *
      *                      *-----------------------------------------*
           if        w-let-arc-dcc-flg    =    spaces
                     go to acc-rig-dpz-425
           else      go to acc-rig-dpz-430.
       acc-rig-dpz-425.
      *                      *-----------------------------------------*
      *                      * Se anagrafica commerciale del cliente   *
      *                      * principale esistente                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A dipendenze dall'impostazione      *
      *                          *-------------------------------------*
           go to     acc-rig-dpz-600.
       acc-rig-dpz-430.
      *                      *-----------------------------------------*
      *                      * Se anagrafica commerciale del cliente   *
      *                      * principale non esistente                *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Messaggio di errore                 *
      *                          *-------------------------------------*
           move      "Manca l'anagrafica commerciale per il cliente     
      -              "          "         to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                          *-------------------------------------*
      *                          * A reimpostazione                    *
      *                          *-------------------------------------*
           go to     acc-rig-dpz-100.
       acc-rig-dpz-440.
      *                  *---------------------------------------------*
      *                  * Se codice dipendenza diverso da spaces      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica commerciale per la   *
      *                      * dipendenza                              *
      *                      *-----------------------------------------*
           move      "D"                  to   w-let-arc-dcc-tle      .
           move      w-rig-arc-plf        to   w-let-arc-dcc-cod      .
           move      w-rig-dpz-plf        to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                      *-----------------------------------------*
      *                      * Memorizzazione valori letti da anagra-  *
      *                      * fica commerciale della dipendenza in    *
      *                      * dati per la dipendenza                  *
      *                      *-----------------------------------------*
           move      w-let-arc-dcc-rag    to   w-rig-dpz-plf-rag      .
           move      w-let-arc-dcc-via    to   w-rig-dpz-plf-via      .
           move      w-let-arc-dcc-loc    to   w-rig-dpz-plf-loc      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione valori relativi alla    *
      *                      * dipendenza                              *
      *                      *-----------------------------------------*
           perform   vis-rig-dpz-000      thru vis-rig-dpz-999        .
           perform   vis-rig-dpz-rag-000  thru vis-rig-dpz-rag-999    .
           perform   vis-rig-dpz-via-000  thru vis-rig-dpz-via-999    .
           perform   vis-rig-dpz-loc-000  thru vis-rig-dpz-loc-999    .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda dell'esito della   *
      *                      * lettura                                 *
      *                      *-----------------------------------------*
           if        w-let-arc-dcc-flg    =    spaces
                     go to acc-rig-dpz-445
           else      go to acc-rig-dpz-450.
       acc-rig-dpz-445.
      *                      *-----------------------------------------*
      *                      * Se anagrafica commerciale della dipen-  *
      *                      * denza esistente                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A dipendenze dall'impostazione      *
      *                          *-------------------------------------*
           go to     acc-rig-dpz-600.
       acc-rig-dpz-450.
      *                      *-----------------------------------------*
      *                      * Se anagrafica commerciale della dipen-  *
      *                      * denza non esistente                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Messaggio di errore                 *
      *                          *-------------------------------------*
           move      spaces               to   w-err-box-err-msg      .
           string    "Manca l'anagrafica commerciale per la dipendenza "
                                delimited by   size
                     "'"        delimited by   size
                     w-rig-dpz-plf
                                delimited by spaces
                     "'"        delimited by   size
                                          into w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                          *-------------------------------------*
      *                          * A reimpostazione                    *
      *                          *-------------------------------------*
           go to     acc-rig-dpz-100.
       acc-rig-dpz-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-rig-dpz-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-rig-000
                                          thru cnt-tdo-rig-999
                     if      w-cnt-tdo-rig-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-rig
                             go to acc-rig-dpz-999
                     else    move  spaces to   w-cnt-tdo-rig-flg
                             go to acc-rig-dpz-100.
       acc-rig-dpz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Dipendenza archivio per fattura-  *
      *    *                         zione                             *
      *    *-----------------------------------------------------------*
       vis-rig-dpz-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-rig-dpz-plf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rig-dpz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Dipendenza archivio per fattura-  *
      *    *                         zione, ragione sociale            *
      *    *-----------------------------------------------------------*
       vis-rig-dpz-rag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-rig-dpz-plf-rag
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rig-dpz-rag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Dipendenza archivio per fattura-  *
      *    *                         zione, indirizzo                  *
      *    *-----------------------------------------------------------*
       vis-rig-dpz-via-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-rig-dpz-plf-via
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rig-dpz-via-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Dipendenza archivio per fattura-  *
      *    *                         zione, localita'                  *
      *    *-----------------------------------------------------------*
       vis-rig-dpz-loc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-rig-dpz-plf-loc
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rig-dpz-loc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo fatturazione                    *
      *    *-----------------------------------------------------------*
       acc-rig-ftz-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-rig-tip-ftz        to   w-sav-tip-ftz          .
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        w-rig-tip-ftz        not  = zero
                     go to acc-rig-ftz-100.
           move      01                   to   w-rig-tip-ftz          .
       acc-rig-ftz-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-rig-ftz-lun    to   v-car                  .
           move      w-exp-rig-ftz-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      21                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-exp-rig-ftz-tbl    to   v-txt                  .
           move      w-rig-tip-ftz        to   v-num                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-rig
                     go to acc-rig-ftz-999.
       acc-rig-ftz-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-rig-tip-ftz          .
       acc-rig-ftz-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : reimpostazione, a meno che non si *
      *                  * sia in Up, e che il valore precedente non   *
      *                  * fosse gia' a zero                           *
      *                  *---------------------------------------------*
           if        w-rig-tip-ftz        not  = zero
                     go to acc-rig-ftz-600.
           if        v-key                not  = "UP  "
                     go to acc-rig-ftz-100.
           if        w-sav-tip-ftz        =    zero
                     go to acc-rig-ftz-600
           else      go to acc-rig-ftz-100.
       acc-rig-ftz-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-rig-ftz-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-rig-000
                                          thru cnt-tdo-rig-999
                     if      w-cnt-tdo-rig-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-rig
                             go to acc-rig-ftz-999
                     else    move  spaces to   w-cnt-tdo-rig-flg
                             go to acc-rig-ftz-100.
       acc-rig-ftz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tipo fatturazione                 *
      *    *-----------------------------------------------------------*
       vis-rig-ftz-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-rig-ftz-lun    to   v-car                  .
           move      w-exp-rig-ftz-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      21                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-exp-rig-ftz-tbl    to   v-txt                  .
           move      w-rig-tip-ftz        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rig-ftz-999.
           exit.

      *    *===========================================================*
      *    * Controllo su impostazione tasto Do campi chiave           *
      *    *-----------------------------------------------------------*
       cnt-tdo-key-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-key-flg      .
       cnt-tdo-key-100.
      *              *-------------------------------------------------*
      *              * Controlli bloccanti                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  *---------------------------------------------*
           if        w-prs-tip-ord-tip    =    "R"
                     go to cnt-tdo-key-120
           else if   w-prs-tip-ord-tip    =    "C"
                     go to cnt-tdo-key-140
           else if   w-prs-tip-ord-tip    =    "M"
                     go to cnt-tdo-key-160.
       cnt-tdo-key-120.
      *                      *-----------------------------------------*
      *                      * Ordinamento per Ragione sociale         *
      *                      *-----------------------------------------*
           if        w-tes-rag-max        =    spaces
                     go to cnt-tdo-key-124.
           if        w-tes-rag-max        not  < w-tes-rag-min
                     go to cnt-tdo-key-124.
           move      "La ragione sociale massima non puo' essere inferio
      -              "re alla minima "    to   w-err-box-err-msg      .
           go to     cnt-tdo-key-900.
       cnt-tdo-key-124.
      *                      *-----------------------------------------*
      *                      * A normalizzazioni                       *
      *                      *-----------------------------------------*
           go to     cnt-tdo-key-500.
       cnt-tdo-key-140.
      *                  *---------------------------------------------*
      *                  * Controllo su codice minimo e massimo        *
      *                  *---------------------------------------------*
           if        w-tes-cod-max        =    zero
                     go to cnt-tdo-key-144.
           if        w-tes-cod-max        not  < w-tes-cod-min
                     go to cnt-tdo-key-144.
           move      "Il codice massimo non puo' essere inferiore al cod
      -              "ice minimo !   "    to   w-err-box-err-msg      .
           go to     cnt-tdo-key-900.
       cnt-tdo-key-144.
      *                      *-----------------------------------------*
      *                      * A normalizzazioni                       *
      *                      *-----------------------------------------*
           go to     cnt-tdo-key-500.
       cnt-tdo-key-160.
      *                  *---------------------------------------------*
      *                  * Controllo su mnemonico minimo e massimo     *
      *                  *---------------------------------------------*
           if        w-tes-mne-max        =    spaces
                     go to cnt-tdo-key-164.
           if        w-tes-mne-max        not  < w-tes-mne-min
                     go to cnt-tdo-key-164.
           move      "Il mnemonico massimo non puo' essere inferiore a q
      -              "uello minimo ! "    to   w-err-box-err-msg      .
           go to     cnt-tdo-key-900.
       cnt-tdo-key-164.
      *                      *-----------------------------------------*
      *                      * A normalizzazioni                       *
      *                      *-----------------------------------------*
           go to     cnt-tdo-key-500.
       cnt-tdo-key-500.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo fornitura                              *
      *                  *---------------------------------------------*
           if        w-tes-tip-frn        =    99
                     move  zero           to   w-tes-tip-frn          .
      *                  *---------------------------------------------*
      *                  * Tipo fatturazione                           *
      *                  *---------------------------------------------*
           if        w-tes-tip-ftz        =    99
                     move  zero           to   w-tes-tip-ftz          .
       cnt-tdo-key-510.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  *---------------------------------------------*
           if        w-prs-tip-ord-tip    =    "R"
                     go to cnt-tdo-key-520
           else if   w-prs-tip-ord-tip    =    "C"
                     go to cnt-tdo-key-540
           else if   w-prs-tip-ord-tip    =    "M"
                     go to cnt-tdo-key-560.
       cnt-tdo-key-520.
      *                      *-----------------------------------------*
      *                      * Ordinamento per Ragione sociale         *
      *                      *-----------------------------------------*
           if        w-tes-rag-max        =    spaces
                     move   w-tes-rag-min to   w-tes-rag-max          .
           move      w-tes-rag-max        to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-str-pad-000      thru all-str-pad-999        .
           perform   all-str-pad-000      thru all-str-pad-999        .
           move      w-all-str-alf        to   w-tes-rag-max          .
      *                          *-------------------------------------*
      *                          * Ad uscita post-normalizzazioni      *
      *                          *-------------------------------------*
           go to     cnt-tdo-key-800.
       cnt-tdo-key-540.
      *                      *-----------------------------------------*
      *                      * Ordinamento per Codice cliente          *
      *                      *-----------------------------------------*
           if        w-tes-cod-min        =    zero and
                     w-tes-cod-max        =    zero
                     move   9999999       to   w-tes-cod-max
           else if   w-tes-cod-min        not  = zero and
                     w-tes-cod-max        =    zero
                     move  w-tes-cod-min  to   w-tes-cod-max          .
      *                          *-------------------------------------*
      *                          * Ad uscita post-normalizzazioni      *
      *                          *-------------------------------------*
           go to     cnt-tdo-key-800.
       cnt-tdo-key-560.
      *                      *-----------------------------------------*
      *                      * Ordinamento per Mnemonico               *
      *                      *-----------------------------------------*
           if        w-tes-mne-max        =    spaces
                     move   w-tes-mne-min to   w-tes-mne-max          .
           move      w-tes-mne-max        to   w-all-str-alf          .
           move      10                   to   w-all-str-lun          .
           perform   all-str-pad-000      thru all-str-pad-999        .
           move      w-all-str-alf        to   w-tes-mne-max          .
      *                          *-------------------------------------*
      *                          * Ad uscita post-normalizzazioni      *
      *                          *-------------------------------------*
           go to     cnt-tdo-key-800.
       cnt-tdo-key-800.
      *              *-------------------------------------------------*
      *              * Uscita post-normalizzazioni                     *
      *              *-------------------------------------------------*
           go to     cnt-tdo-key-999.
       cnt-tdo-key-900.
      *              *-------------------------------------------------*
      *              * Emissione messaggio di errore e set del flag di *
      *              * uscita ad errore                                *
      *              *-------------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di errore in uscita                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-tdo-key-flg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cnt-tdo-key-999.
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
       cnt-tdo-rig-160.
      *              *-------------------------------------------------*
      *              * Controllo su Tipo fornitura abituale            *
      *              *-------------------------------------------------*
           if        w-rig-dpz-cli        not  = spaces
                     go to cnt-tdo-rig-162.
           if        w-prs-ges-apf        not  = 21 and
                     w-prs-ges-apf        not  = 99
                     go to cnt-tdo-rig-162.
           if        w-rig-tip-frn        =    zero
                     go to cnt-tdo-rig-162.
           if        w-rig-tip-frn        =    01 or
                     w-rig-tip-frn        =    21
                     go to cnt-tdo-rig-162.
           move      "Tipo fornitura abituale errato                    
      -              "          "         to   w-err-box-err-msg      .
           go to     cnt-tdo-rig-900.
       cnt-tdo-rig-162.
      *              *-------------------------------------------------*
      *              * Controllo su Codice archivio per fatturazione   *
      *              *-------------------------------------------------*
           if        w-rig-dpz-cli        not  = spaces
                     go to cnt-tdo-rig-164.
           if        w-prs-ges-apf        not  = 21 and
                     w-prs-ges-apf        not  = 99
                     go to cnt-tdo-rig-164.
           if        w-rig-arc-plf        =    zero
                     go to cnt-tdo-rig-164.
           move      "C"                  to   w-let-arc-dcc-tle      .
           move      w-rig-arc-plf        to   w-let-arc-dcc-cod      .
           move      spaces               to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
           if        w-let-arc-dcc-flg    =    spaces
                     go to cnt-tdo-rig-164.
           move      "Codice Cliente capogruppo non esistente           
      -              "          "         to   w-err-box-err-msg      .
           go to     cnt-tdo-rig-900.
       cnt-tdo-rig-164.
      *              *-------------------------------------------------*
      *              * Controllo su Dipendenza archivio per fattura-   *
      *              * zione                                           *
      *              *-------------------------------------------------*
           if        w-rig-dpz-cli        not  = spaces
                     go to cnt-tdo-rig-166.
           if        w-prs-ges-apf        not  = 21 and
                     w-prs-ges-apf        not  = 99
                     go to cnt-tdo-rig-166.
           if        w-rig-arc-plf        =    zero
                     go to cnt-tdo-rig-166.
           if        w-rig-dpz-plf        =    spaces
                     go to cnt-tdo-rig-166.
           move      "D"                  to   w-let-arc-dcc-tle      .
           move      w-rig-arc-plf        to   w-let-arc-dcc-cod      .
           move      w-rig-dpz-plf        to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
           if        w-let-arc-dcc-flg    =    spaces
                     go to cnt-tdo-rig-166.
           move      "Codice Dipendenza Cliente capogruppo non esistente
      -              "          "         to   w-err-box-err-msg      .
           go to     cnt-tdo-rig-900.
       cnt-tdo-rig-166.
      *              *-------------------------------------------------*
      *              * Controllo su Tipo fatturazione                  *
      *              *-------------------------------------------------*
           if        w-rig-dpz-cli        not  = spaces
                     go to cnt-tdo-rig-168.
           if        w-prs-ges-apf        not  = 21 and
                     w-prs-ges-apf        not  = 99
                     go to cnt-tdo-rig-168.
           if        w-rig-tip-ftz        =    zero
                     go to cnt-tdo-rig-168.
           if        w-rig-tip-ftz        =    01 or
                     w-rig-tip-ftz        =    02
                     go to cnt-tdo-rig-168.
           move      "Tipo fatturazione errato                          
      -              "          "         to   w-err-box-err-msg      .
           go to     cnt-tdo-rig-900.
       cnt-tdo-rig-168.
       cnt-tdo-rig-600.
      *              *-------------------------------------------------*
      *              * Normalizzazioni post-controlli bloccanti        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se si tratta del cli-  *
      *                  * ente principale o di una sua dipendenza     *
      *                  *---------------------------------------------*
           if        w-rig-dpz-cli        =    spaces
                     go to cnt-tdo-rig-620
           else      go to cnt-tdo-rig-700.
       cnt-tdo-rig-620.
      *                  *---------------------------------------------*
      *                  * Se cliente principale                       *
      *                  *---------------------------------------------*
       cnt-tdo-rig-625.
      *                      *-----------------------------------------*
      *                      * Tipo fornitura abituale                 *
      *                      *-----------------------------------------*
           if        w-rig-tip-frn        =    zero
                     move  01             to   w-rig-tip-frn          .
      *                      *-----------------------------------------*
      *                      * Tipo fatturazione                       *
      *                      *-----------------------------------------*
           if        w-rig-tip-ftz        not  = zero
                     go to cnt-tdo-rig-640.
           if        w-rig-tip-frn        not  = 21
                     go to cnt-tdo-rig-640.
           move      01                   to   w-rig-tip-ftz          .
       cnt-tdo-rig-640.
      *                      *-----------------------------------------*
      *                      * Uscita post-normalizzazione             *
      *                      *-----------------------------------------*
           go to     cnt-tdo-rig-999.
       cnt-tdo-rig-700.
      *                  *---------------------------------------------*
      *                  * Se dipendenza                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Eliminazione valori non significativi   *
      *                      * per la dipendenza                       *
      *                      *-----------------------------------------*
           move      zero                 to   w-rig-tip-frn          .
           move      zero                 to   w-rig-arc-plf          .
           move      spaces               to   w-rig-dpz-plf          .
           move      zero                 to   w-rig-tip-ftz          .
      *                      *-----------------------------------------*
      *                      * Uscita post-normalizzazione             *
      *                      *-----------------------------------------*
           go to     cnt-tdo-rig-999.
       cnt-tdo-rig-900.
      *              *-------------------------------------------------*
      *              * Emissione messaggio di errore e set del flag di *
      *              * uscita ad errore                                *
      *              *-------------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di errore in uscita                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-tdo-rig-flg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cnt-tdo-rig-999.
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
      *              *-------------------------------------------------*
      *              * Normalizzazione selettori sulle chiavi          *
      *              *-------------------------------------------------*
           move      zero                 to   w-tes-cod-min          .
           move      zero                 to   w-tes-cod-max          .
           move      spaces               to   w-tes-rag-min          .
           move      spaces               to   w-tes-rag-max          .
           move      spaces               to   w-tes-mne-min          .
           move      spaces               to   w-tes-mne-max          .
      *              *-------------------------------------------------*
      *              * Normalizzazione selettori sui dati              *
      *              *-------------------------------------------------*
           move      zero                 to   w-tes-tip-frn          .
           move      zero                 to   w-tes-arc-plf          .
           move      spaces               to   w-tes-arc-plf-rag      .
           move      spaces               to   w-tes-arc-plf-via      .
           move      spaces               to   w-tes-arc-plf-loc      .
           move      spaces               to   w-tes-dpz-plf          .
           move      spaces               to   w-tes-dpz-plf-rag      .
           move      spaces               to   w-tes-dpz-plf-via      .
           move      spaces               to   w-tes-dpz-plf-loc      .
           move      zero                 to   w-tes-tip-ftz          .
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
           move      zero                 to   w-rig-cod-cli          .
           move      spaces               to   w-rig-dpz-cli          .
           move      spaces               to   w-rig-cod-cli-rag      .
           move      spaces               to   w-rig-cod-cli-mne      .
           move      zero                 to   w-rig-tip-frn          .
           move      zero                 to   w-rig-arc-plf          .
           move      spaces               to   w-rig-arc-plf-rag      .
           move      spaces               to   w-rig-arc-plf-via      .
           move      spaces               to   w-rig-arc-plf-loc      .
           move      spaces               to   w-rig-dpz-plf          .
           move      spaces               to   w-rig-dpz-plf-rag      .
           move      spaces               to   w-rig-dpz-plf-via      .
           move      spaces               to   w-rig-dpz-plf-loc      .
           move      zero                 to   w-rig-tip-ftz          .
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
       rou-let-reg-100.
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
           move      "                       --- Caricamento in esecuzio
      -              "ne ---                        "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Routine di caricamento                          *
      *              *-------------------------------------------------*
           perform   car-fil-rap-000      thru car-fil-rap-999        .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del numero di records ca-  *
      *              * ricati nel file relative di appoggio            *
      *              *-------------------------------------------------*
           if        w-rlt-sup-max        =    zero
                     go to rou-let-reg-200
           else      go to rou-let-reg-300.
       rou-let-reg-200.
      *              *-------------------------------------------------*
      *              * Se numero di records caricati : zero            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Nessun cliente da trattare !                      
      -              "          "         to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Status di uscita ad errore                  *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-rou-let-reg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rou-let-reg-999.
       rou-let-reg-300.
      *              *-------------------------------------------------*
      *              * Se numero di records caricati maggiore di zero  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Modifica                                    *
      *                  *---------------------------------------------*
           move      "M"                  to   w-cnt-mfu-tip-fun      .
      *                  *---------------------------------------------*
      *                  * Test per visualizzazione                    *
      *                  *---------------------------------------------*
           if        w-ovy-exe-vis        =    "V"
                     move  "V"            to   w-cnt-mfu-tip-fun      .
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
       pos-cnf-ins-999.
           exit.

      *    *===========================================================*
      *    * Routine post-conferma di modifica                         *
      *    *-----------------------------------------------------------*
       pos-cnf-mod-000.
      *              *-------------------------------------------------*
      *              * Scaricamento file relative di appoggio          *
      *              *-------------------------------------------------*
           perform   sca-fil-rap-000      thru sca-fil-rap-999        .
       pos-cnf-mod-999.
           exit.

      *    *===========================================================*
      *    * Routine post-conferma di annullamento                     *
      *    *-----------------------------------------------------------*
       pos-cnf-ann-000.
       pos-cnf-ann-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione caricamento file relative di appoggio          *
      *    *-----------------------------------------------------------*
       car-fil-rap-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore righe da caricare     *
      *              *-------------------------------------------------*
           move      zero                 to   w-car-eff-rig-ctr      .
      *              *-------------------------------------------------*
      *              * Normalizzazione segnale di superamento massimo  *
      *              * righe da caricare                               *
      *              *-------------------------------------------------*
           move      spaces               to   w-car-eff-rig-flg      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo ordinamento da  *
      *              * personalizzazione                               *
      *              *-------------------------------------------------*
           if        w-prs-tip-ord-tip    =    "R"
                     go to car-fil-rap-020
           else if   w-prs-tip-ord-tip    =    "C"
                     go to car-fil-rap-040
           else if   w-prs-tip-ord-tip    =    "M"
                     go to car-fil-rap-060.
       car-fil-rap-020.
      *                  *---------------------------------------------*
      *                  * Ordinamento per Ragione sociale             *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "RAGKEY    "         to   f-key                  .
           move      w-tes-rag-min        to   rf-dcc-rag-key         .
           move      zero                 to   rf-dcc-cod-cli         .
           move      spaces               to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to car-fil-rap-900.
      *                  *---------------------------------------------*
      *                  * A Read-next                                 *
      *                  *---------------------------------------------*
           go to     car-fil-rap-200.
       car-fil-rap-040.
      *                  *---------------------------------------------*
      *                  * Ordinamento per Codice                      *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODCLI    "         to   f-key                  .
           move      w-tes-cod-min        to   rf-dcc-cod-cli         .
           move      spaces               to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to car-fil-rap-900.
      *                  *---------------------------------------------*
      *                  * A Read-next                                 *
      *                  *---------------------------------------------*
           go to     car-fil-rap-200.
       car-fil-rap-060.
      *                  *---------------------------------------------*
      *                  * Ordinamento per Mnemonico                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Dal momento che il record [dcc] non ha  *
      *                      * una chiave sul codice mnemonico, si     *
      *                      * deve utilizzare quella del file [cli]   *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODMNE    "         to   f-key                  .
           move      w-tes-mne-min        to   rf-cli-cod-mne         .
           move      zero                 to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to car-fil-rap-900.
      *                  *---------------------------------------------*
      *                  * A Read-next                                 *
      *                  *---------------------------------------------*
           go to     car-fil-rap-200.
       car-fil-rap-200.
      *              *-------------------------------------------------*
      *              * Read-next                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Indicatore di programma in esecuzione       *
      *                  *---------------------------------------------*
           move      "IE"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  *---------------------------------------------*
           if        w-prs-tip-ord-tip    =    "R"
                     go to car-fil-rap-220
           else if   w-prs-tip-ord-tip    =    "C"
                     go to car-fil-rap-240
           else if   w-prs-tip-ord-tip    =    "M"
                     go to car-fil-rap-260.
       car-fil-rap-220.
      *                  *---------------------------------------------*
      *                  * Ordinamento per Ragione sociale             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Read-next su archivio [dcc]             *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                      *-----------------------------------------*
      *                      * Se at end : uscita                      *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to car-fil-rap-900.
      *                      *-----------------------------------------*
      *                      * A test sul massimo                      *
      *                      *-----------------------------------------*
           go to     car-fil-rap-300.
       car-fil-rap-240.
      *                  *---------------------------------------------*
      *                  * Ordinamento per Codice                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Read-next su archivio [dcc]             *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                      *-----------------------------------------*
      *                      * Se at end : uscita                      *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to car-fil-rap-900.
      *                      *-----------------------------------------*
      *                      * A test sul massimo                      *
      *                      *-----------------------------------------*
           go to     car-fil-rap-300.
       car-fil-rap-260.
      *                  *---------------------------------------------*
      *                  * Ordinamento per Mnemonico                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Read-next su archivio [cli]             *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                      *-----------------------------------------*
      *                      * Se at end : uscita                      *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to car-fil-rap-900.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
           if        rf-cli-cod-mne       >    w-tes-mne-max
                     go to car-fil-rap-900.
       car-fil-rap-262.
      *                      *-----------------------------------------*
      *                      * Start sul file [dcc] per codice         *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODCLI    "         to   f-key                  .
           move      rf-cli-cod-cli       to   rf-dcc-cod-cli         .
           move      spaces               to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                          *-------------------------------------*
      *                          * Se Start errata : a riciclo         *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to car-fil-rap-600.
       car-fil-rap-264.
      *                      *-----------------------------------------*
      *                      * Read-next su archivio [dcc]             *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                      *-----------------------------------------*
      *                      * Se at end : a riciclo                   *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to car-fil-rap-600.
       car-fil-rap-266.
      *                      *-----------------------------------------*
      *                      * Test sul massimo file [dcc]             *
      *                      *-----------------------------------------*
           if        rf-dcc-cod-cli       not  = rf-cli-cod-cli
                     go to car-fil-rap-600.
       car-fil-rap-268.
      *                      *-----------------------------------------*
      *                      * Selezioni sul record                    *
      *                      *-----------------------------------------*
           perform   sel-rec-dcc-000      thru sel-rec-dcc-999        .
      *                          *-------------------------------------*
      *                          * Test su flag di selezione           *
      *                          *-------------------------------------*
           if        w-sel-rec-dcc-flg    not  = spaces
                     go to car-fil-rap-264.
       car-fil-rap-270.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione codice mnemonico        *
      *                      *-----------------------------------------*
           move      rf-cli-cod-mne       to   w-rig-cod-cli-mne      .
      *                      *-----------------------------------------*
      *                      * Caricamento effettivo del record nella  *
      *                      * catena movimenti per la registrazione   *
      *                      *-----------------------------------------*
           perform   car-eff-rig-000      thru car-eff-rig-999        .
      *                          *-------------------------------------*
      *                          * Test su flag di uscita              *
      *                          *-------------------------------------*
           if        w-car-eff-rig-flg    =    spaces
                     go to car-fil-rap-272.
      *                          *-------------------------------------*
      *                          * Box di superamento numero elementi  *
      *                          * massimo da caricare                 *
      *                          *-------------------------------------*
           perform   box-err-rig-000      thru box-err-rig-999        .
      *                          *-------------------------------------*
      *                          * Fine lettura                        *
      *                          *-------------------------------------*
           go to     car-fil-rap-900.
       car-fil-rap-272.
      *                      *-----------------------------------------*
      *                      * Riciclo a Read Next su file [cli]       *
      *                      *-----------------------------------------*
           go to     car-fil-rap-264.
       car-fil-rap-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  *---------------------------------------------*
           if        w-prs-tip-ord-tip    =    "R"
                     go to car-fil-rap-320
           else if   w-prs-tip-ord-tip    =    "C"
                     go to car-fil-rap-340
           else if   w-prs-tip-ord-tip    =    "M"
                     go to car-fil-rap-360.
       car-fil-rap-320.
      *                  *---------------------------------------------*
      *                  * Ordinamento per Ragione sociale             *
      *                  *---------------------------------------------*
           if        rf-dcc-rag-key       >    w-tes-rag-max
                     go to car-fil-rap-900.
      *                      *-----------------------------------------*
      *                      * A selezioni sul record                  *
      *                      *-----------------------------------------*
           go to     car-fil-rap-400.
       car-fil-rap-340.
      *                  *---------------------------------------------*
      *                  * Ordinamento per Codice                      *
      *                  *---------------------------------------------*
           if        rf-dcc-cod-cli       >    w-tes-cod-max
                     go to car-fil-rap-900.
      *                      *-----------------------------------------*
      *                      * A selezioni sul record                  *
      *                      *-----------------------------------------*
           go to     car-fil-rap-400.
       car-fil-rap-360.
      *                  *---------------------------------------------*
      *                  * Ordinamento per Mnemonico                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Nessuna azione                          *
      *                      *-----------------------------------------*
           go to     car-fil-rap-400.
       car-fil-rap-400.
      *              *-------------------------------------------------*
      *              * Selezioni sul record                            *
      *              *-------------------------------------------------*
           perform   sel-rec-dcc-000      thru sel-rec-dcc-999        .
      *                  *---------------------------------------------*
      *                  * Test su flag di selezione                   *
      *                  *---------------------------------------------*
           if        w-sel-rec-dcc-flg    not  = spaces
                     go to car-fil-rap-200.
       car-fil-rap-500.
      *              *-------------------------------------------------*
      *              * Caricamento effettivo del record nella catena   *
      *              * movimenti per la registrazione                  *
      *              *-------------------------------------------------*
           perform   car-eff-rig-000      thru car-eff-rig-999        .
      *                  *---------------------------------------------*
      *                  * Test su flag di uscita                      *
      *                  *---------------------------------------------*
           if        w-car-eff-rig-flg    =    spaces
                     go to car-fil-rap-600.
      *                  *---------------------------------------------*
      *                  * Box di superamento numero elementi massimo  *
      *                  * da caricare                                 *
      *                  *---------------------------------------------*
           perform   box-err-rig-000      thru box-err-rig-999        .
      *                  *---------------------------------------------*
      *                  * Fine lettura                                *
      *                  *---------------------------------------------*
           go to     car-fil-rap-900.
       car-fil-rap-600.
      *              *-------------------------------------------------*
      *              * Riciclo a Read Next su file [dcc]               *
      *              *-------------------------------------------------*
           go to     car-fil-rap-200.
       car-fil-rap-900.
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
       car-fil-rap-950.
      *              *-------------------------------------------------*
      *              * Forzatura flag di visualizzazione prompt e dati *
      *              * campi chiave                                    *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-sts-pmt-key      .
           move      "#"                  to   w-cnt-sts-vis-key      .
       car-fil-rap-999.
           exit.

      *    *===========================================================*
      *    * Selezioni sul record [dcc]                                *
      *    *-----------------------------------------------------------*
       sel-rec-dcc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di selezione               *
      *              *-------------------------------------------------*
           move      spaces               to   w-sel-rec-dcc-flg      .
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
       sel-rec-dcc-010.
      *                  *---------------------------------------------*
      *                  * Per le dipendenze non e' prevista la ges-   *
      *                  * tione di questi campi                       *
      *                  *---------------------------------------------*
           if        rf-dcc-dpz-cli       not  = spaces
                     go to sel-rec-dcc-900.
       sel-rec-dcc-100.
      *                  *---------------------------------------------*
      *                  * Tipo fornitura                              *
      *                  *---------------------------------------------*
           if        w-tes-tip-frn        =    zero
                     go to sel-rec-dcc-200.
           if        rf-dcc-tip-frn       not  = w-tes-tip-frn
                     go to sel-rec-dcc-900.
       sel-rec-dcc-200.
      *                  *---------------------------------------------*
      *                  * Codice archivio per fatturazione            *
      *                  *---------------------------------------------*
           if        w-tes-arc-plf        =    zero
                     go to sel-rec-dcc-300.
           if        rf-dcc-arc-plf       not  = w-tes-arc-plf
                     go to sel-rec-dcc-900.
       sel-rec-dcc-220.
      *                  *---------------------------------------------*
      *                  * Dipendenza archivio per fatturazione        *
      *                  *---------------------------------------------*
           if        w-tes-dpz-plf        =    spaces
                     go to sel-rec-dcc-300.
           if        rf-dcc-dpz-plf       not  = w-tes-dpz-plf
                     go to sel-rec-dcc-900.
       sel-rec-dcc-300.
      *                  *---------------------------------------------*
      *                  * Tipo fatturazione                           *
      *                  *---------------------------------------------*
           if        w-tes-tip-ftz        =    zero
                     go to sel-rec-dcc-800.
           if        rf-dcc-tip-ftz       not  = w-tes-tip-ftz
                     go to sel-rec-dcc-900.
       sel-rec-dcc-800.
      *                  *---------------------------------------------*
      *                  * Uscita per selezione superata               *
      *                  *---------------------------------------------*
           go to     sel-rec-dcc-999.
       sel-rec-dcc-900.
      *              *-------------------------------------------------*
      *              * Se selezione non superata                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di selezione non superata              *
      *                  *---------------------------------------------*
           move      "#"                  to   w-sel-rec-dcc-flg      .
      *                  *---------------------------------------------*
      *                  * Uscita per selezione non superata           *
      *                  *---------------------------------------------*
           go to     sel-rec-dcc-999.
       sel-rec-dcc-999.
           exit.

      *    *===========================================================*
      *    * Caricamento effettivo delle righe                         *
      *    *-----------------------------------------------------------*
       car-eff-rig-000.
      *              *-------------------------------------------------*
      *              * Composizione area 'w-rig'                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento contatore righe da caricare      *
      *                  *---------------------------------------------*
           add       1                    to   w-car-eff-rig-ctr      .
      *                  *---------------------------------------------*
      *                  * Test sul massimo righe caricabili           *
      *                  *---------------------------------------------*
           if        w-car-eff-rig-ctr    >    w-car-eff-rig-max
                     move  "#"            to   w-car-eff-rig-flg
                     go to car-eff-rig-999.
      *                  *---------------------------------------------*
      *                  * Numero progressivo record caricato          *
      *                  *---------------------------------------------*
           move      w-rlt-sup-max        to   w-rig-num-prg          .
           add       1                    to   w-rig-num-prg          .
      *                      *-----------------------------------------*
      *                      * Valori determinati direttamente da      *
      *                      * record [dcc]                            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Dati relativi al cliente            *
      *                          *-------------------------------------*
           move      rf-dcc-cod-cli       to   w-rig-cod-cli          .
           move      rf-dcc-dpz-cli       to   w-rig-dpz-cli          .
           move      rf-dcc-rag-soc       to   w-rig-cod-cli-rag      .
      *                          *-------------------------------------*
      *                          * Dati gestiti dal programma          *
      *                          *-------------------------------------*
           move      rf-dcc-tip-frn       to   w-rig-tip-frn          .
           move      rf-dcc-arc-plf       to   w-rig-arc-plf          .
           move      rf-dcc-dpz-plf       to   w-rig-dpz-plf          .
           move      rf-dcc-tip-ftz       to   w-rig-tip-ftz          .
      *                      *-----------------------------------------*
      *                      * Valori determinati indirettamente da    *
      *                      * record [dcc]                            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Ragione sociale archivio per fattu- *
      *                          * razione                             *
      *                          *-------------------------------------*
           move      "C"                  to   w-let-arc-dcc-tle      .
           move      w-rig-arc-plf        to   w-let-arc-dcc-cod      .
           move      spaces               to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
           move      w-let-arc-dcc-rag    to   w-rig-arc-plf-rag      .
           move      w-let-arc-dcc-via    to   w-rig-arc-plf-via      .
           move      w-let-arc-dcc-loc    to   w-rig-arc-plf-loc      .
      *                          *-------------------------------------*
      *                          * Descrizione dipendenza archivio per *
      *                          * fatturazione                        *
      *                          *-------------------------------------*
           if        w-rig-dpz-plf        =    spaces
                     move  "C"            to   w-let-arc-dcc-tle
           else      move  "D"            to   w-let-arc-dcc-tle      .
           move      w-rig-arc-plf        to   w-let-arc-dcc-cod      .
           move      w-rig-dpz-plf        to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
           move      w-let-arc-dcc-rag    to   w-rig-dpz-plf-rag      .
           move      w-let-arc-dcc-via    to   w-rig-dpz-plf-via      .
           move      w-let-arc-dcc-loc    to   w-rig-dpz-plf-loc      .
       car-eff-rig-800.
      *              *-------------------------------------------------*
      *              * Bufferizzazione riga pre-modifica               *
      *              *-------------------------------------------------*
           move      w-rig-001            to   w-rig-002              .
      *              *-------------------------------------------------*
      *              * Put su file relative di appoggio                *
      *              *-------------------------------------------------*
           move      "PT"                 to   w-rlt-sup-ope          .
           move      w-rig                to   w-rlt-sup-buf          .
           perform   cll-rlt-sup-000      thru cll-rlt-sup-999        .
       car-eff-rig-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione scaricamento da file relative di appoggio      *
      *    *-----------------------------------------------------------*
       sca-fil-rap-000.
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
           move      "            --- Aggiornamento anagrafiche clienti 
      -              "in esecuzione ---             "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       sca-fil-rap-050.
      *              *-------------------------------------------------*
      *              * Contatore records scaricati a zero              *
      *              *-------------------------------------------------*
           move      zero                 to   w-rlt-sup-ctr          .
       sca-fil-rap-070.
      *              *-------------------------------------------------*
      *              * Normalizzazione set                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        w-ipc-tdc-mos-val    not  = "S"
                     go to sca-fil-rap-100.
      *                  *---------------------------------------------*
      *                  * Utente di sistema da segreteria             *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione                             *
      *                  *---------------------------------------------*
           move      "NO"                 to   w-mod-set-tip-ope      .
           move      s-ute                to   w-mod-set-ide-ute      .
           move      i-ide-fas            to   w-mod-set-ide-fas      .
           perform   mdl-gen-set-cll-000  thru mdl-gen-set-cll-999    .
       sca-fil-rap-100.
      *              *-------------------------------------------------*
      *              * Incremento contatore records scaricati          *
      *              *-------------------------------------------------*
           add       1                    to   w-rlt-sup-ctr          .
      *              *-------------------------------------------------*
      *              * Se maggiore del numero di records caricati  :   *
      *              * uscita                                          *
      *              *-------------------------------------------------*
           if        w-rlt-sup-ctr        >    w-rlt-sup-max
                     go to sca-fil-rap-900.
       sca-fil-rap-150.
      *              *-------------------------------------------------*
      *              * Lettura riga da file relative di supporto       *
      *              *-------------------------------------------------*
           move      "RD"                 to   w-rlt-sup-ope          .
           move      w-rlt-sup-ctr        to   w-rlt-sup-prg          .
           perform   cll-rlt-sup-000      thru cll-rlt-sup-999        .
           move      w-rlt-sup-buf        to   w-rig                  .
       sca-fil-rap-160.
      *              *-------------------------------------------------*
      *              * Test preliminare se in fase di generazione set  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su variabile letta                     *
      *                  *---------------------------------------------*
           if        w-ipc-tdc-mos-val    not  = "S"
                     go to sca-fil-rap-180.
      *                  *---------------------------------------------*
      *                  * Subroutine di update set                    *
      *                  *---------------------------------------------*
           perform   sca-fil-rap-set-000  thru   sca-fil-rap-set-999  .
      *                  *---------------------------------------------*
      *                  * A riga successiva                           *
      *                  *---------------------------------------------*
           go to     sca-fil-rap-400.
       sca-fil-rap-180.
      *              *-------------------------------------------------*
      *              * Test se riga modificata                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non e' avvenuta alcuna modifica si rici- *
      *                  * cla alla riga successiva                    *
      *                  *---------------------------------------------*
           if        w-rig-001            =    w-rig-002
                     go to sca-fil-rap-400.
      *              *-------------------------------------------------*
      *              * Lettura, con lock, del record [dcc]             *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
           move      w-rig-cod-cli        to   rf-dcc-cod-cli         .
           move      w-rig-dpz-cli        to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda dell'esito della lettura   *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to sca-fil-rap-250.
       sca-fil-rap-200.
      *              *-------------------------------------------------*
      *              * Se record cliente non esistente                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Riciclo a rilettura riga da file relative   *
      *                  * di supporto                                 *
      *                  *---------------------------------------------*
           go to     sca-fil-rap-100.
       sca-fil-rap-250.
      *              *-------------------------------------------------*
      *              * Se record cliente esistente                     *
      *              *-------------------------------------------------*
       sca-fil-rap-300.
      *                  *---------------------------------------------*
      *                  * Aggiornamento dati nel record               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Data, utente, fase, di ultimo inseri-   *
      *                      * mento o modifica                        *
      *                      *-----------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-dcc-ide-dat         .
           move      s-ute                to   rf-dcc-ide-ute         .
           move      s-fas                to   rf-dcc-ide-fas         .
      *                      *-----------------------------------------*
      *                      * Dati gestiti dal programma              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Tipo fornitura abituale             *
      *                          *-------------------------------------*
           move      w-rig-tip-frn        to   rf-dcc-tip-frn         .
      *                          *-------------------------------------*
      *                          * Codice archivio per fatturazione    *
      *                          *-------------------------------------*
           move      w-rig-arc-plf        to   rf-dcc-arc-plf         .
      *                          *-------------------------------------*
      *                          * Dipendenza archivio di fatturazione *
      *                          *-------------------------------------*
           move      w-rig-dpz-plf        to   rf-dcc-dpz-plf         .
      *                          *-------------------------------------*
      *                          * Tipo fatturazione                   *
      *                          *-------------------------------------*
           move      w-rig-tip-ftz        to   rf-dcc-tip-ftz         .
       sca-fil-rap-350.
      *                  *---------------------------------------------*
      *                  * Update record                               *
      *                  *---------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * Unlock record                               *
      *                  *---------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
       sca-fil-rap-400.
      *                  *---------------------------------------------*
      *                  * Riciclo a rilettura riga da file relative   *
      *                  * di supporto                                 *
      *                  *---------------------------------------------*
           go to     sca-fil-rap-100.
       sca-fil-rap-900.
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
       sca-fil-rap-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione scaricamento da file relative di appoggio      *
      *    *                                                           *
      *    * Subroutine di generazione set                             *
      *    *-----------------------------------------------------------*
       sca-fil-rap-set-000.
      *              *-------------------------------------------------*
      *              * Utente di sistema da segreteria                 *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Update                                          *
      *              *-------------------------------------------------*
           move      "UP"                 to   w-mod-set-tip-ope      .
           move      s-ute                to   w-mod-set-ide-ute      .
           move      i-ide-fas            to   w-mod-set-ide-fas      .
           move      i-ide-des            to   w-mod-set-ide-des      .
           move      01                   to   w-mod-set-tip-arc      .
           move      w-rig-cod-cli        to   w-mod-set-cod-arc      .
           move      w-rig-dpz-cli        to   w-mod-set-alf-arc      .
           move      spaces               to   w-mod-set-dat-ac1      .
           move      spaces               to   w-mod-set-dat-ac2      .
           move      spaces               to   w-mod-set-dat-ac3      .
           perform   mdl-gen-set-cll-000  thru mdl-gen-set-cll-999    .
       sca-fil-rap-set-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     sca-fil-rap-set-999.
       sca-fil-rap-set-999.
           exit.

      *    *===========================================================*
      *    * Routine per Expand anagrafica commerciale cliente in riga *
      *    *-----------------------------------------------------------*
       rou-exp-ana-000.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Video in 'OFF'                                  *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
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
       rou-exp-ana-100.
      *              *-------------------------------------------------*
      *              * Costruzione box di Expand                       *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      w-cnt-wrk-ctr-002    to   v-lin                  .
           subtract  1                    from v-lin                  .
           move      15                   to   v-pos                  .
           move      w-cnt-wrk-ctr-002    to   v-lto                  .
           add       04                   to   v-lto                  .
           move      58                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       rou-exp-ana-200.
      *              *-------------------------------------------------*
      *              * Lettura dati per Expand                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Anagrafica commerciale cliente              *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      w-rou-exp-ana-cod    to   rf-dcc-cod-cli         .
           move      w-rou-exp-ana-dpz    to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-dcc-rag-soc
                     move  all "."        to   rf-dcc-via-dcc
                     move  all "."        to   rf-dcc-loc-dcc         .
      *                  *---------------------------------------------*
      *                  * Anagrafica contabile cliente                *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      w-rou-exp-ana-cod    to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-cli-cod-mne         .
       rou-exp-ana-300.
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts per Expand              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt per Partita Iva                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-cnt-wrk-ctr-002    to   v-lin                  .
           add       03                   to   v-lin                  .
           move      17                   to   v-pos                  .
           move      "P.I.:"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Prompt per Mnemonico                        *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      w-cnt-wrk-ctr-002    to   v-lin                  .
           add       03                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      "Mnem.:"             to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       rou-exp-ana-400.
      *              *-------------------------------------------------*
      *              * Visualizzazione dati per Expand                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione Ragione sociale             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cnt-wrk-ctr-002    to   v-lin                  .
           move      17                   to   v-pos                  .
           move      rf-dcc-rag-soc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione Indirizzo                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cnt-wrk-ctr-002    to   v-lin                  .
           add       01                   to   v-lin                  .
           move      17                   to   v-pos                  .
           move      rf-dcc-via-dcc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione localita'                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cnt-wrk-ctr-002    to   v-lin                  .
           add       02                   to   v-lin                  .
           move      17                   to   v-pos                  .
           move      rf-dcc-loc-dcc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione Partita Iva                 *
      *                  *---------------------------------------------*
           if        rf-cli-prt-iva       =    zero
                     go to rou-exp-ana-420.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      w-cnt-wrk-ctr-002    to   v-lin                  .
           add       03                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      rf-cli-prt-iva       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       rou-exp-ana-420.
      *                  *---------------------------------------------*
      *                  * Visualizzazione codice mnemonico            *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      w-cnt-wrk-ctr-002    to   v-lin                  .
           add       03                   to   v-lin                  .
           move      44                   to   v-pos                  .
           move      rf-cli-cod-mne       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       rou-exp-ana-900.
      *              *-------------------------------------------------*
      *              * Video in 'ON'                                   *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione di un carattere per presa visione  *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      w-cnt-wrk-ctr-002    to   v-lin                  .
           add       03                   to   v-lin                  .
           move      56                   to   v-pos                  .
           move      "EXIT"               to   v-pfk (1)              .
           move      "DO  "               to   v-pfk (2)              .
           move      "DOWN"               to   v-pfk (3)              .
           move      "UP  "               to   v-pfk (4)              .
           move      "EXPD"               to   v-pfk (5)              .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      spaces               to   v-key                  .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       rou-exp-ana-999.
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
      *    * Box per superamento massimo righe caricabili              *
      *    *-----------------------------------------------------------*
       box-err-rig-000.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Costruzione box di dialogo                      *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      08                   to   v-lin                  .
           move      15                   to   v-pos                  .
           move      17                   to   v-lto                  .
           move      66                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Literal nel box di dialogo - 1                  *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      47                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      17                   to   v-pos                  .
           move      "Il numero di clienti selezionati e' maggiore di"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Literal nel box di dialogo - 2                  *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      47                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      17                   to   v-pos                  .
           move      "quello gestibile dal programma (99999).        "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Literal nel box di dialogo - 3                  *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      47                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      17                   to   v-pos                  .
           move      "Si consiglia di operare una serie di selezioni "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Literal nel box di dialogo - 4                  *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      47                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      17                   to   v-pos                  .
           move      "sul minimo e massimo per poter agire su tutti  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Literal nel box di dialogo - 5                  *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      47                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      17                   to   v-pos                  .
           move      "gli elementi desiderati.                       "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Literal nel box di dialogo - 6                  *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      47                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      17                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Literal di presa visione                        *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "Premere 'OK' per presa visione :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       box-err-rig-800.
      *              *-------------------------------------------------*
      *              * Accettazione campo di conferma                  *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      63                   to   v-pos                  .
           move      spaces               to   v-edm                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           if        v-alf                not  = "OK"
                     go to box-err-rig-800.
       box-err-rig-900.
      *              *-------------------------------------------------*
      *              * Restore video                                   *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       box-err-rig-999.
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
      *    * Subroutines per l'accettazione codice cliente commerciale *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmndcc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice dipendenza cliente  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoddcc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per modulo modulo generazione set             *
      *    *-----------------------------------------------------------*
           copy      "pgm/svf/prg/cpy/msetsvf0.mds"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

      *    *===========================================================*
      *    * Richiamo sottoprogramma per gestione catena righe         *
      *    *-----------------------------------------------------------*
       cll-rlt-sup-000.
           call      "pgm/dcc/prg/obj/pdcc4502"
                                         using w-rlt-sup              .
       cll-rlt-sup-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione sottoprogramma per gestione catena righe    *
      *    *-----------------------------------------------------------*
       cnc-rlt-sup-000.
           cancel    "pgm/dcc/prg/obj/pdcc4502"                       .
       cnc-rlt-sup-999.
           exit.

