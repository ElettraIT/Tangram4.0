       Identification Division.
       Program-Id.                                 pdcp1200           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dcp                 *
      *                                Settore:    tab                 *
      *                                   Fase:    dcp120              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 29/05/91    *
      *                       Ultima revisione:    NdK del 13/05/13    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione tabelle :                          *
      *                     - Classi                                   *
      *                     - Gruppi                                   *
      *                     - Sottogruppi                              *
      *                    per classificazioni gerarchiche prodotti.   *
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
                     "dcp"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "tab"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "dcp120"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pdcp1200"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     " GESTIONE CLASSI - GRUPPI - SOTTOGRUPPI "       .

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
      *        * [zp1]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzp1"                          .
      *        *-------------------------------------------------------*
      *        * [zp2]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzp2"                          .
      *        *-------------------------------------------------------*
      *        * [zp3]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzp3"                          .
      *        *-------------------------------------------------------*
      *        * [zum]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzum"                          .

      *    *===========================================================*
      *    * Work-area per defaults di impostazione                    *
      *    *-----------------------------------------------------------*
       01  w-def.
           05  w-def-tip-ele              pic  9(02) value zero       .
           05  w-def-cod-cla              pic  9(05) value zero       .
           05  w-def-cod-gru              pic  9(05) value zero       .
           05  w-def-cod-sgr              pic  9(05) value zero       .

      *    *===========================================================*
      *    * Work-area per bufferizzazione testata                     *
      *    *-----------------------------------------------------------*
       01  w-tes.
      *        *-------------------------------------------------------*
      *        * Valori chiave                                         *
      *        *-------------------------------------------------------*
           05  w-tes-val-key.
               10  w-tes-tip-ele          pic  9(02)                  .
               10  w-tes-cod-cla          pic  9(05)                  .
               10  w-tes-cod-cla-mne      pic  x(05)                  .
               10  w-tes-cod-cla-des      pic  x(40)                  .
               10  w-tes-cod-cla-sud      pic  9(02)                  .
               10  w-tes-cod-cla-umi      pic  x(03)                  .
               10  w-tes-cod-cla-aut      pic  x(01)                  .
               10  w-tes-cod-gru          pic  9(05)                  .
               10  w-tes-cod-gru-mne      pic  x(05)                  .
               10  w-tes-cod-gru-des      pic  x(40)                  .
               10  w-tes-cod-gru-sud      pic  9(02)                  .
               10  w-tes-cod-gru-umi      pic  x(03)                  .
               10  w-tes-cod-gru-aut      pic  x(01)                  .
               10  w-tes-cod-sgr          pic  9(05)                  .
               10  w-tes-cod-sgr-mne      pic  x(05)                  .
               10  w-tes-cod-sgr-des      pic  x(40)                  .
               10  w-tes-cod-sgr-sud      pic  9(02)                  .
               10  w-tes-cod-sgr-umi      pic  x(03)                  .
               10  w-tes-cod-sgr-aut      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
               10  w-tes-mne-cgs          pic  x(05)                  .
               10  w-tes-des-key          pic  x(40)                  .
               10  w-tes-des-cgs          pic  x(40)                  .
               10  w-tes-sqz-num          pic  9(07)                  .
               10  w-tes-ult-sud          pic  9(02)                  .
               10  w-tes-umi-def          pic  x(03)                  .
               10  w-tes-umi-def-des      pic  x(20)                  .
               10  w-tes-ult-cod          pic  9(05)                  .
               10  w-tes-alx-exp.
                   15  filler  occurs 20  pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zp1]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zp1.
               10  w-let-arc-zp1-flg      pic  x(01)                  .
               10  w-let-arc-zp1-cla      pic  9(05)                  .
               10  w-let-arc-zp1-des      pic  x(40)                  .
               10  w-let-arc-zp1-mne      pic  x(05)                  .
               10  w-let-arc-zp1-sud      pic  9(02)                  .
               10  w-let-arc-zp1-umi      pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zp2]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zp2.
               10  w-let-arc-zp2-flg      pic  x(01)                  .
               10  w-let-arc-zp2-cla      pic  9(05)                  .
               10  w-let-arc-zp2-gru      pic  9(05)                  .
               10  w-let-arc-zp2-des      pic  x(40)                  .
               10  w-let-arc-zp2-mne      pic  x(05)                  .
               10  w-let-arc-zp2-sud      pic  9(02)                  .
               10  w-let-arc-zp2-umi      pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zp3]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zp3.
               10  w-let-arc-zp3-flg      pic  x(01)                  .
               10  w-let-arc-zp3-cla      pic  9(05)                  .
               10  w-let-arc-zp3-gru      pic  9(05)                  .
               10  w-let-arc-zp3-sgr      pic  9(05)                  .
               10  w-let-arc-zp3-des      pic  x(40)                  .
               10  w-let-arc-zp3-mne      pic  x(05)                  .
               10  w-let-arc-zp3-sud      pic  9(02)                  .
               10  w-let-arc-zp3-umi      pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zum]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zum.
               10  w-let-arc-zum-flg      pic  x(01)                  .
               10  w-let-arc-zum-cod      pic  x(03)                  .
               10  w-let-arc-zum-des      pic  x(20)                  .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Tipo esposizione classificazione merceologica         *
      *        *-------------------------------------------------------*
      *            *---------------------------------------------------*
      *            * Stringa contenente le personalizzazioni           *
      *            *---------------------------------------------------*
           05  w-prs-esp-cgs              pic  x(03)                  .
           05  w-prs-esp-cgs-r            redefines
               w-prs-esp-cgs.
      *            *---------------------------------------------------*
      *            * Esposizione per classi                            *
      *            *                                                   *
      *            * Default : 'C' = Codice numerico                   *
      *            *---------------------------------------------------*
               10  w-prs-esp-cgs-cla      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Esposizione per gruppi                            *
      *            *                                                   *
      *            * Default : 'C' = Codice numerico                   *
      *            *---------------------------------------------------*
               10  w-prs-esp-cgs-gru      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Esposizione per sotto gruppi                      *
      *            *                                                   *
      *            * Default : 'C' = Codice numerico                   *
      *            *---------------------------------------------------*
               10  w-prs-esp-cgs-sgr      pic  x(01)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo elemento                              *
      *        *-------------------------------------------------------*
           05  w-exp-tip-ele.
               10  w-exp-tip-ele-num      pic  9(02)       value 03   .
               10  w-exp-tip-ele-lun      pic  9(02)       value 20   .
               10  w-exp-tip-ele-tbl.
                   15  filler             pic  x(20) value
                            "Classe              "                    .
                   15  filler             pic  x(20) value
                            "Gruppo              "                    .
                   15  filler             pic  x(20) value
                            "Sottogruppo         "                    .
      *        *-------------------------------------------------------*
      *        * Work per : Ulteriore suddivisione                     *
      *        *-------------------------------------------------------*
           05  w-exp-ult-sud.
               10  w-exp-ult-sud-num      pic  9(02)       value 02   .
               10  w-exp-ult-sud-lun      pic  9(02)       value 02   .
               10  w-exp-ult-sud-tbl.
                   15  filler             pic  x(02) value "No"       .
                   15  filler             pic  x(02) value "Si"       .
      *        *-------------------------------------------------------*
      *        * Work per : pre cancellazione                          *
      *        *-------------------------------------------------------*
           05  w-exp-pre-del.
               10  w-exp-pre-del-num      pic  9(02)       value 2    .
               10  w-exp-pre-del-lun      pic  9(02)       value 40   .
               10  w-exp-pre-del-tbl.
                   15  filler             pic  x(40) value
                            "Si rinuncia alla cancellazione          ".
                   15  filler             pic  x(40) value
                            "Si cancellano classe, gruppi e sottogr. ".
               10  w-exp-pre-del-sce      pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per determinazione se elemento cancellabile      *
      *        *-------------------------------------------------------*
           05  w-det-snx-del.
      *            *---------------------------------------------------*
      *            * Esito                                             *
      *            * - S : Si, elemento cancellabile                   *
      *            * - N : No, elemento non cancellabile               *
      *            *---------------------------------------------------*
               10  w-det-snx-del-snx      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per determinazione gestione elemento '0'         *
      *        *-------------------------------------------------------*
           05  w-det-cla-pf1.
      *            *---------------------------------------------------*
      *            * Esito                                             *
      *            *---------------------------------------------------*
               10  w-det-cla-pf1-snx      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per determinazione se gruppi nella classe        *
      *        *-------------------------------------------------------*
           05  w-det-gru-cla.
      *            *---------------------------------------------------*
      *            * Codice classe                                     *
      *            *---------------------------------------------------*
               10  w-det-gru-cla-cla      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Esito                                             *
      *            * - S : Si, ci sono gruppi nella classe             *
      *            * - N : No, non ce ne sono                          *
      *            *---------------------------------------------------*
               10  w-det-gru-cla-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Massimo elementi bufferizzabili                   *
      *            *---------------------------------------------------*
               10  w-det-gru-cla-max      pic  9(05)    value 999     .
      *            *---------------------------------------------------*
      *            * Numero elementi bufferizzati                      *
      *            *---------------------------------------------------*
               10  w-det-gru-cla-ele      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Numero di gruppi rilevati                         *
      *            *---------------------------------------------------*
               10  w-det-gru-cla-gru      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Numero di sottogruppi rilevati                    *
      *            *---------------------------------------------------*
               10  w-det-gru-cla-sgr      pic  9(09)                  .
      *            *---------------------------------------------------*
      *            * Contatore di comodo                               *
      *            *---------------------------------------------------*
               10  w-det-gru-cla-ctr      pic  9(09)                  .
      *            *---------------------------------------------------*
      *            * Castelletto elementi bufferizzati                 *
      *            *---------------------------------------------------*
               10  w-det-gru-cla-buf.
      *                *-----------------------------------------------*
      *                * Singolo elemento                              *
      *                *-----------------------------------------------*
                   15  w-det-gru-cla-sel  occurs 999.
      *                    *-------------------------------------------*
      *                    * Codice gruppo                             *
      *                    *-------------------------------------------*
                       20  w-det-gru-cla-bcg
                                          pic  9(05)                  .
      *                    *-------------------------------------------*
      *                    * Codice sottogruppo                        *
      *                    *-------------------------------------------*
                       20  w-det-gru-cla-bcs
                                          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Work per determinazione se sottogruppi nel gruppo     *
      *        *-------------------------------------------------------*
           05  w-det-sgr-gru.
      *            *---------------------------------------------------*
      *            * Codice classe                                     *
      *            *---------------------------------------------------*
               10  w-det-sgr-gru-cla      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Codice gruppo                                     *
      *            *---------------------------------------------------*
               10  w-det-sgr-gru-gru      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Esito                                             *
      *            * - S : Si, ci sono sottogruppi nel gruppo          *
      *            * - N : No, non ce ne sono                          *
      *            *---------------------------------------------------*
               10  w-det-sgr-gru-snx      pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Ctl                               *
      *    *-----------------------------------------------------------*
       01  w-ctl.
      *        *-------------------------------------------------------*
      *        * Work per Ctl su unicita' numero di sequenza           *
      *        *-------------------------------------------------------*
           05  w-ctl-uni-nrs.
               10  w-ctl-uni-nrs-flg      pic  x(01)                  .
               10  w-ctl-uni-nrs-ele      pic  9(02)                  .
               10  w-ctl-uni-nrs-cla      pic  9(05)                  .
               10  w-ctl-uni-nrs-gru      pic  9(05)                  .
               10  w-ctl-uni-nrs-sgr      pic  9(05)                  .
               10  w-ctl-uni-nrs-nrs      pic  9(07)                  .
               10  w-ctl-uni-nrs-ctr      pic  9(02)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice classe prodotto         *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acmnzp10.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice gruppo prodotto         *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acmnzp20.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice sottogruppo prodotto    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acmnzp30.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione unita' di misura               *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acodzum0.acl"                   .

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
      *    * Work per salvataggi valori precedenti                     *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Tipo elemento                                         *
      *        *-------------------------------------------------------*
           05  w-sav-tip-ele              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Codice classe                                         *
      *        *-------------------------------------------------------*
           05  w-sav-cod-cla              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Codice gruppo                                         *
      *        *-------------------------------------------------------*
           05  w-sav-cod-gru              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Codice sottogruppo                                    *
      *        *-------------------------------------------------------*
           05  w-sav-cod-sgr              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione                                           *
      *        *-------------------------------------------------------*
           05  w-sav-des-cgs              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Mnemonico                                             *
      *        *-------------------------------------------------------*
           05  w-sav-mne-cgs              pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero di sequenza                                    *
      *        *-------------------------------------------------------*
           05  w-sav-sqz-num              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Ulteriore suddivisione                                *
      *        *-------------------------------------------------------*
           05  w-sav-ult-sud              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Unita' di misura di default                           *
      *        *-------------------------------------------------------*
           05  w-sav-umi-def              pic  x(03)                  .
           05  w-sav-umi-def-des          pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Ultimo autocodice                                     *
      *        *-------------------------------------------------------*
           05  w-sav-ult-cod              pic  9(05)                  .

      *    *===========================================================*
      *    * Work per attribuzione e ripristino codice automatico      *
      *    *-----------------------------------------------------------*
       01  w-enc-cod.
      *        *-------------------------------------------------------*
      *        * Per codice classe prodotto                            *
      *        *-------------------------------------------------------*
           05  w-enc-cod-cla.
      *            *---------------------------------------------------*
      *            * Valore pre incremento                             *
      *            *---------------------------------------------------*    
               10  w-enc-cod-cla-pre      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Valore post incremento                            *
      *            *---------------------------------------------------*
               10  w-enc-cod-cla-pos      pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Per codice gruppo prodotto                            *
      *        *-------------------------------------------------------*
           05  w-enc-cod-gru.
      *            *---------------------------------------------------*
      *            * Codice classe                                     *
      *            *---------------------------------------------------*    
               10  w-enc-cod-gru-cla      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Valore pre incremento                             *
      *            *---------------------------------------------------*    
               10  w-enc-cod-gru-pre      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Valore post incremento                            *
      *            *---------------------------------------------------*
               10  w-enc-cod-gru-pos      pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Per codice sottogruppo prodotto                       *
      *        *-------------------------------------------------------*
           05  w-enc-cod-sgr.
      *            *---------------------------------------------------*
      *            * Codice classe                                     *
      *            *---------------------------------------------------*    
               10  w-enc-cod-sgr-cla      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Codice gruppo                                     *
      *            *---------------------------------------------------*    
               10  w-enc-cod-sgr-gru      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Valore pre incremento                             *
      *            *---------------------------------------------------*    
               10  w-enc-cod-sgr-pre      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Valore post incremento                            *
      *            *---------------------------------------------------*
               10  w-enc-cod-sgr-pos      pic  9(05)                  .

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
      *              * Test se presente record 'zero' nel file [zp1]   *
      *              *-------------------------------------------------*
           perform   ctl-rcz-zp1-000      thru ctl-rcz-zp1-999        .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione relativa all'esposi-  *
      *              * zione per la classificazione merceologica       *
      *              *-------------------------------------------------*
           perform   prs-esp-cgs-000      thru prs-esp-cgs-999        .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice classe          *
      *              *-------------------------------------------------*
           perform   cod-mne-zp1-opn-000  thru cod-mne-zp1-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice gruppo          *
      *              *-------------------------------------------------*
           perform   cod-mne-zp2-opn-000  thru cod-mne-zp2-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice sottogruppo     *
      *              *-------------------------------------------------*
           perform   cod-mne-zp3-opn-000  thru cod-mne-zp3-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione unita' di misura       *
      *              *-------------------------------------------------*
           perform   cod-cod-zum-opn-000  thru cod-cod-zum-opn-999    .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Accettazione valori per tipo  *
      *    * esposizione classificazione merceologica                  *
      *    *-----------------------------------------------------------*
       prs-esp-cgs-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dcp[esp-cgs]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       prs-esp-cgs-010.
      *              *-------------------------------------------------*
      *              * Se personalizzazione non esistente              *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     go to prs-esp-cgs-050.
      *                  *---------------------------------------------*
      *                  * Normalizzazione al valore di default        *
      *                  *---------------------------------------------*
           move      "C"                  to   w-prs-esp-cgs-cla      .
           move      "C"                  to   w-prs-esp-cgs-gru      .
           move      "C"                  to   w-prs-esp-cgs-sgr      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     prs-esp-cgs-999.
       prs-esp-cgs-050.
      *              *-------------------------------------------------*
      *              * Valore letto in work personalizzazioni          *
      *              *-------------------------------------------------*
           move      s-alf                to   w-prs-esp-cgs          .
       prs-esp-cgs-100.
      *              *-------------------------------------------------*
      *              * Controllo valori letti                          *
      *              *-------------------------------------------------*
           if        w-prs-esp-cgs-cla    not  = "M" and
                     w-prs-esp-cgs-cla    not  = "C"
                     move  "C"            to   w-prs-esp-cgs-cla      .
           if        w-prs-esp-cgs-gru    not  = "M" and
                     w-prs-esp-cgs-gru    not  = "C"
                     move  "C"            to   w-prs-esp-cgs-gru      .
           if        w-prs-esp-cgs-sgr    not  = "M" and
                     w-prs-esp-cgs-sgr    not  = "C"
                     move  "C"            to   w-prs-esp-cgs-sgr      .
       prs-esp-cgs-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice classe         *
      *              *-------------------------------------------------*
           perform   cod-mne-zp1-cls-000  thru cod-mne-zp1-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice gruppo         *
      *              *-------------------------------------------------*
           perform   cod-mne-zp2-cls-000  thru cod-mne-zp2-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice sottogruppo    *
      *              *-------------------------------------------------*
           perform   cod-mne-zp3-cls-000  thru cod-mne-zp3-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione unita' di misura      *
      *              *-------------------------------------------------*
           perform   cod-cod-zum-cls-000  thru cod-cod-zum-cls-999    .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * [zp1]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
      *              *-------------------------------------------------*
      *              * [zp2]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
      *              *-------------------------------------------------*
      *              * [zp3]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
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
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [zp1]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
      *              *-------------------------------------------------*
      *              * [zp2]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
      *              *-------------------------------------------------*
      *              * [zp3]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
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
      *                  * Tipo elemento                               *
      *                  *---------------------------------------------*
           perform   acc-tip-ele-000      thru acc-tip-ele-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
       acc-key-reg-200.
      *                  *---------------------------------------------*
      *                  * Codice classe                               *
      *                  *---------------------------------------------*
           perform   acc-cod-cla-000      thru acc-cod-cla-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
           if        v-key                =    "UP  "
                     go to acc-key-reg-100.
       acc-key-reg-300.
      *                  *---------------------------------------------*
      *                  * Codice gruppo                               *
      *                  *---------------------------------------------*
           perform   acc-cod-gru-000      thru acc-cod-gru-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
           if        v-key                =    "UP  "
                     go to acc-key-reg-200.
       acc-key-reg-400.
      *                  *---------------------------------------------*
      *                  * Codice sottogruppo                          *
      *                  *---------------------------------------------*
           perform   acc-cod-sgr-000      thru acc-cod-sgr-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
           if        v-key                =    "UP  "
                     go to acc-key-reg-300.
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
      *              * Tipo elemento                                   *
      *              *-------------------------------------------------*
           perform   vis-tip-ele-000      thru vis-tip-ele-999        .
      *              *-------------------------------------------------*
      *              * Codice classe                                   *
      *              *-------------------------------------------------*
           perform   vis-cod-cla-000      thru vis-cod-cla-999        .
      *              *-------------------------------------------------*
      *              * Descrizione classe                              *
      *              *-------------------------------------------------*
           perform   vis-des-cla-000      thru vis-des-cla-999        .
      *              *-------------------------------------------------*
      *              * Codice gruppo                                   *
      *              *-------------------------------------------------*
           perform   vis-cod-gru-000      thru vis-cod-gru-999        .
      *              *-------------------------------------------------*
      *              * Descrizione gruppo                              *
      *              *-------------------------------------------------*
           perform   vis-des-gru-000      thru vis-des-gru-999        .
      *              *-------------------------------------------------*
      *              * Codice sottogruppo                              *
      *              *-------------------------------------------------*
           perform   vis-cod-sgr-000      thru vis-cod-sgr-999        .
      *              *-------------------------------------------------*
      *              * Descrizione sottogruppo                         *
      *              *-------------------------------------------------*
           perform   vis-des-sgr-000      thru vis-des-sgr-999        .
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
           move      08                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Tipo elemento                                   *
      *              *-------------------------------------------------*
           perform   pmt-tip-ele-000      thru pmt-tip-ele-999        .
      *              *-------------------------------------------------*
      *              * Codice classe                                   *
      *              *-------------------------------------------------*
           perform   pmt-cod-cla-000      thru pmt-cod-cla-999        .
      *              *-------------------------------------------------*
      *              * Codice gruppo                                   *
      *              *-------------------------------------------------*
           perform   pmt-cod-gru-000      thru pmt-cod-gru-999        .
      *              *-------------------------------------------------*
      *              * Codice sottogruppo                              *
      *              *-------------------------------------------------*
           perform   pmt-cod-sgr-000      thru pmt-cod-sgr-999        .
      *              *-------------------------------------------------*
      *              * Linea di trattini                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Tipo elemento                 *
      *    *-----------------------------------------------------------*
       pmt-tip-ele-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo elemento              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-tip-ele-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Codice classe                 *
      *    *-----------------------------------------------------------*
       pmt-cod-cla-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice classe              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-cla-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Codice gruppo                 *
      *    *-----------------------------------------------------------*
       pmt-cod-gru-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice gruppo              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-gru-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Codice sottogruppo            *
      *    *-----------------------------------------------------------*
       pmt-cod-sgr-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice sottogruppo         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-sgr-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Tipo elemento                 *
      *    *-----------------------------------------------------------*
       acc-tip-ele-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-tip-ele        to   w-sav-tip-ele          .
      *                  *---------------------------------------------*
      *                  * Preparazione valore di default              *
      *                  *---------------------------------------------*
           move      w-def-tip-ele        to   w-tes-tip-ele          .
       acc-tip-ele-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-ele-lun    to   v-car                  .
           move      w-exp-tip-ele-num    to   v-ldt                  .
           move      "CGS#"               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-ele-tbl    to   v-txt                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-tip-ele        to   v-num                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-tip-ele-999.
       acc-tip-ele-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-tip-ele          .
      *              *-------------------------------------------------*
      *              * Preparazione default per accettazione successi- *
      *              * va                                              *
      *              *-------------------------------------------------*
           move      w-tes-tip-ele        to   w-def-tip-ele          .
       acc-tip-ele-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che il valore sia accettabile          *
      *                  *---------------------------------------------*
           if        w-tes-tip-ele        =    zero           or
                     w-tes-tip-ele        >    w-exp-tip-ele-num
                     go to acc-tip-ele-100.
       acc-tip-ele-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo elemento    *
      *                  *---------------------------------------------*
           if        w-tes-tip-ele        =    01
                     go to acc-tip-ele-630
           else if   w-tes-tip-ele        =    02
                     go to acc-tip-ele-635
           else if   w-tes-tip-ele        =    03
                     go to acc-tip-ele-640.
       acc-tip-ele-630.
      *                  *---------------------------------------------*
      *                  * Normalizzazione della classe                *
      *                  *---------------------------------------------*
           if        w-tes-cod-cla        =    zero
                     go to acc-tip-ele-632.
           move      zero                 to   w-tes-cod-cla          .
           move      spaces               to   w-tes-cod-cla-mne      .
           move      spaces               to   w-tes-cod-cla-des      .
           move      zero                 to   w-tes-cod-cla-sud      .
           move      spaces               to   w-tes-cod-cla-umi      .
           move      spaces               to   w-tes-cod-cla-aut      .
           perform   vis-cod-cla-000      thru vis-cod-cla-999        .
           perform   vis-des-cla-000      thru vis-des-cla-999        .
       acc-tip-ele-632.
           move      zero                 to   w-def-cod-cla          .
       acc-tip-ele-635.
      *                  *---------------------------------------------*
      *                  * Normalizzazione del gruppo                  *
      *                  *---------------------------------------------*
           if        w-tes-cod-gru        =    zero
                     go to acc-tip-ele-637.
           move      zero                 to   w-tes-cod-gru          .
           move      spaces               to   w-tes-cod-gru-mne      .
           move      spaces               to   w-tes-cod-gru-des      .
           move      zero                 to   w-tes-cod-gru-sud      .
           move      spaces               to   w-tes-cod-gru-umi      .
           move      spaces               to   w-tes-cod-gru-aut      .
           perform   vis-cod-gru-000      thru vis-cod-gru-999        .
           perform   vis-des-gru-000      thru vis-des-gru-999        .
       acc-tip-ele-637.
           move      zero                 to   w-def-cod-gru          .
       acc-tip-ele-640.
      *                  *---------------------------------------------*
      *                  * Normalizzazione del sottogruppo             *
      *                  *---------------------------------------------*
           if        w-tes-cod-sgr        =    zero
                     go to acc-tip-ele-642.
           move      zero                 to   w-tes-cod-sgr          .
           move      spaces               to   w-tes-cod-sgr-mne      .
           move      spaces               to   w-tes-cod-sgr-des      .
           move      zero                 to   w-tes-cod-sgr-sud      .
           move      spaces               to   w-tes-cod-sgr-umi      .
           move      spaces               to   w-tes-cod-sgr-aut      .
           perform   vis-cod-sgr-000      thru vis-cod-sgr-999        .
           perform   vis-des-sgr-000      thru vis-des-sgr-999        .
       acc-tip-ele-642.
           move      zero                 to   w-def-cod-sgr          .
       acc-tip-ele-645.
      *                  *---------------------------------------------*
      *                  * A controlli tasto Do                        *
      *                  *---------------------------------------------*
           go to     acc-tip-ele-800.
       acc-tip-ele-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-tip-ele-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-tip-ele-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-tip-ele-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-tip-ele-999.
       acc-tip-ele-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Tipo elemento              *
      *    *-----------------------------------------------------------*
       vis-tip-ele-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-ele-lun    to   v-car                  .
           move      w-exp-tip-ele-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-ele-tbl    to   v-txt                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-tip-ele        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-ele-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Codice classe                 *
      *    *-----------------------------------------------------------*
       acc-cod-cla-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-cla-025.
      *                  *---------------------------------------------*
      *                  * Segnale per PF1                             *
      *                  *---------------------------------------------*
           move      "N"                  to   w-det-cla-pf1-snx      .
       acc-cod-cla-030.
      *                  *---------------------------------------------*
      *                  * Salvataggio valori precedenti               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice classe                           *
      *                      *-----------------------------------------*
           move      w-tes-cod-cla        to   w-sav-cod-cla          .
      *                      *-----------------------------------------*
      *                      * Codice gruppo                           *
      *                      *-----------------------------------------*
           move      w-tes-cod-gru        to   w-sav-cod-gru          .
      *                      *-----------------------------------------*
      *                      * Codice sottogruppo                      *
      *                      *-----------------------------------------*
           move      w-tes-cod-sgr        to   w-sav-cod-sgr          .
       acc-cod-cla-050.
      *                  *---------------------------------------------*
      *                  * Preparazione valore di default              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se valore attuale diverso da zero : no  *
      *                      * preparazione default                    *
      *                      *-----------------------------------------*
           if        w-tes-cod-cla        not  = zero
                     go to acc-cod-cla-075.
      *                      *-----------------------------------------*
      *                      * Se valore di default pari a zero : no   *
      *                      * preparazione default                    *
      *                      *-----------------------------------------*
           if        w-def-cod-cla        =    zero
                     go to acc-cod-cla-075.
      *                      *-----------------------------------------*
      *                      * Preparazione                            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Codice classe                       *
      *                          *-------------------------------------*
           move      w-def-cod-cla        to   w-tes-cod-cla          .
      *                          *-------------------------------------*
      *                          * Lettura codice classe               *
      *                          *-------------------------------------*
           move      w-tes-cod-cla        to   w-let-arc-zp1-cla      .
           perform   let-arc-zp1-000      thru let-arc-zp1-999        .
      *                          *-------------------------------------*
      *                          * Bufferizzazione di :                *
      *                          * - Mnemonico                         *
      *                          * - Descrizione                       *
      *                          * - Ulteriore suddivisione            *
      *                          * - Unita' di misura da proporre      *
      *                          *-------------------------------------*
           move      w-let-arc-zp1-mne    to   w-tes-cod-cla-mne      .
           move      w-let-arc-zp1-des    to   w-tes-cod-cla-des      .
           move      w-let-arc-zp1-sud    to   w-tes-cod-cla-sud      .
           move      w-let-arc-zp1-umi    to   w-tes-cod-cla-umi      .
      *                          *-------------------------------------*
      *                          * Visualizzazione codice              *
      *                          *-------------------------------------*
           perform   vis-cod-cla-000      thru vis-cod-cla-999        .
      *                          *-------------------------------------*
      *                          * Visualizzazione descrizione, a meno *
      *                          * che il tipo elemento non corrispon- *
      *                          * da ad una classe                    *
      *                          *-------------------------------------*
           if        w-tes-tip-ele        =    01
                     go to acc-cod-cla-075.
           perform   vis-des-cla-000      thru vis-des-cla-999        .
       acc-cod-cla-075.
      *                  *---------------------------------------------*
      *                  * Continuazione                               *
      *                  *---------------------------------------------*
           go to     acc-cod-cla-100.
       acc-cod-cla-100.
      *              *-------------------------------------------------*
      *              * Note operative                                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "[F1] per modificare la dicitura dei NON CLASSIFICA
      -              "TI (Codice '0')"    to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zp1-ope      .
           move      w-tes-cod-cla        to   w-cod-mne-zp1-cla      .
           move      zero                 to   w-cod-mne-zp1-gru      .
           move      zero                 to   w-cod-mne-zp1-sgr      .
           move      05                   to   w-cod-mne-zp1-lin      .
           move      30                   to   w-cod-mne-zp1-pos      .
           if        w-tes-tip-ele        =    01
                     move  09             to   w-cod-mne-zp1-dln
                     move  30             to   w-cod-mne-zp1-dps
           else      move  05             to   w-cod-mne-zp1-dln
                     move  39             to   w-cod-mne-zp1-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "[1] "               to   v-pfk (14)             .
           perform   cod-mne-zp1-cll-000  thru cod-mne-zp1-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zp1-foi-000  thru cod-mne-zp1-foi-999    .
       acc-cod-cla-110.
           perform   cod-mne-zp1-cll-000  thru cod-mne-zp1-cll-999    .
           if        w-cod-mne-zp1-ope    =    "F+"
                     go to acc-cod-cla-115.
           if        w-cod-mne-zp1-ope    =    "AC"
                     go to acc-cod-cla-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-cla-115.
           perform   cod-mne-zp1-foi-000  thru cod-mne-zp1-foi-999    .
           go to     acc-cod-cla-110.
       acc-cod-cla-120.
           move      w-cod-mne-zp1-cla    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Cancellazione note operative                    *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-cla-999.
       acc-cod-cla-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-cla          .
       acc-cod-cla-300.
      *              *-------------------------------------------------*
      *              * Se Pf1                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se Pf1                                 *
      *                  *---------------------------------------------*
           if        v-key                not  = "[1] "
                     go to acc-cod-cla-400.
      *                  *---------------------------------------------*
      *                  * Segnale per PF1                             *
      *                  *---------------------------------------------*
           move      "S"                  to   w-det-cla-pf1-snx      .
       acc-cod-cla-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tasto Up : esclusione controlli          *
      *                  *---------------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-cod-cla-600.
      *                  *---------------------------------------------*
      *                  * Preparazione default per accettazione suc-  *
      *                  * cessiva                                     *
      *                  *---------------------------------------------*
           move      w-tes-cod-cla        to   w-def-cod-cla          .
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo elemento da *
      *                  * trattare                                    *
      *                  *---------------------------------------------*
           if        w-tes-tip-ele        =    01
                     go to acc-cod-cla-450
           else      go to acc-cod-cla-500.
       acc-cod-cla-450.
      *                  *---------------------------------------------*
      *                  * Se il tipo elemento da trattare corrisponde *
      *                  * ad una classe                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del valore impo- *
      *                      * stato                                   *
      *                      *-----------------------------------------*
           if        w-tes-cod-cla        =    zero
                     go to acc-cod-cla-460
           else      go to acc-cod-cla-470.
       acc-cod-cla-460.
      *                      *-----------------------------------------*
      *                      * Se il valore impostato e' a zero        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se inserimento consentito      *
      *                          *-------------------------------------*
           if        w-cnt-mfu-vis-sgr    not  = "V"
                     go to acc-cod-cla-462.
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
           go to     acc-cod-cla-100.
       acc-cod-cla-462.
      *                          *-------------------------------------*
      *                          * Se segnale per PF1 attivo, no at-   *
      *                          * tribuzione                          *
      *                          *-------------------------------------*
           if        w-det-cla-pf1-snx    = "S"
                     go to acc-cod-cla-600.
      *                          *-------------------------------------*
      *                          * Attribuzione codice automatico pro- *
      *                          * gressivo per la classe              *
      *                          *-------------------------------------*
           perform   att-cla-aut-000      thru att-cla-aut-999        .
      *                          *-------------------------------------*
      *                          * Codice automatico in campo di de-   *
      *                          * stinazione                          *
      *                          *-------------------------------------*
           move      w-enc-cod-cla-pos    to   w-tes-cod-cla          .
      *                          *-------------------------------------*
      *                          * Normalizzazione valori associati    *
      *                          * alla classe                         *
      *                          *-------------------------------------*
           move      spaces               to   w-tes-cod-cla-mne      .
           move      spaces               to   w-tes-cod-cla-des      .
           move      zero                 to   w-tes-cod-cla-sud      .
           move      spaces               to   w-tes-cod-cla-umi      .
      *                          *-------------------------------------*
      *                          * Segnale di attribuzione codice ese- *
      *                          * guita automaticamente               *
      *                          *-------------------------------------*
           move      "#"                  to   w-tes-cod-cla-aut      .
      *                          *-------------------------------------*
      *                          * Visualizzazione codice classe       *
      *                          *-------------------------------------*
           perform   vis-cod-cla-000      thru vis-cod-cla-999        .
      *                          *-------------------------------------*
      *                          * Prosecuzione                        *
      *                          *-------------------------------------*
           go to     acc-cod-cla-600.
       acc-cod-cla-470.
      *                      *-----------------------------------------*
      *                      * Se il valore impostato e' a non-zero    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura del codice classe           *
      *                          *-------------------------------------*
           move      w-tes-cod-cla        to   w-let-arc-zp1-cla      .
           perform   let-arc-zp1-000      thru let-arc-zp1-999        .
      *                          *-------------------------------------*
      *                          * Bufferizzazione di :                *
      *                          * - Mnemonico                         *
      *                          * - Descrizione                       *
      *                          * - Ulteriore suddivisione            *
      *                          * - Unita' di misura da proporre      *
      *                          *-------------------------------------*
           move      w-let-arc-zp1-mne    to   w-tes-cod-cla-mne      .
           move      w-let-arc-zp1-des    to   w-tes-cod-cla-des      .
           move      w-let-arc-zp1-sud    to   w-tes-cod-cla-sud      .
           move      w-let-arc-zp1-umi    to   w-tes-cod-cla-umi      .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     acc-cod-cla-600.
       acc-cod-cla-500.
      *                  *---------------------------------------------*
      *                  * Se il tipo elemento da trattare non corri-  *
      *                  * sponde ad una classe                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del valore impo- *
      *                      * stato                                   *
      *                      *-----------------------------------------*
           if        w-tes-cod-cla        =    zero
                     go to acc-cod-cla-510
           else      go to acc-cod-cla-520.
       acc-cod-cla-510.
      *                      *-----------------------------------------*
      *                      * Se il valore impostato e' a zero        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura del codice classe           *
      *                          *-------------------------------------*
           move      w-tes-cod-cla        to   w-let-arc-zp1-cla      .
           perform   let-arc-zp1-000      thru let-arc-zp1-999        .
      *                          *-------------------------------------*
      *                          * Bufferizzazione di :                *
      *                          * - Mnemonico                         *
      *                          * - Descrizione                       *
      *                          * - Ulteriore suddivisione            *
      *                          * - Unita' di misura da proporre      *
      *                          *-------------------------------------*
           move      w-let-arc-zp1-mne    to   w-tes-cod-cla-mne      .
           move      w-let-arc-zp1-des    to   w-tes-cod-cla-des      .
           move      w-let-arc-zp1-sud    to   w-tes-cod-cla-sud      .
           move      w-let-arc-zp1-umi    to   w-tes-cod-cla-umi      .
      *                          *-------------------------------------*
      *                          * Visualizzazione descrizione per la  *
      *                          * classe                              *
      *                          *-------------------------------------*
           perform   vis-des-cla-000      thru vis-des-cla-999        .
      *                          *-------------------------------------*
      *                          * Se tasto Up : a dipendenze da impo- *
      *                          * stazione                            *
      *                          *-------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-cod-cla-600.
      *                          *-------------------------------------*
      *                          * Altrimenti : a reimpostazione       *
      *                          *-------------------------------------*
           go to     acc-cod-cla-100.
       acc-cod-cla-520.
      *                      *-----------------------------------------*
      *                      * Se il valore impostato e' a non-zero    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura del codice classe           *
      *                          *-------------------------------------*
           move      w-tes-cod-cla        to   w-let-arc-zp1-cla      .
           perform   let-arc-zp1-000      thru let-arc-zp1-999        .
      *                          *-------------------------------------*
      *                          * Bufferizzazione di :                *
      *                          * - Mnemonico                         *
      *                          * - Descrizione                       *
      *                          * - Ulteriore suddivisione            *
      *                          * - Unita' di misura da proporre      *
      *                          *-------------------------------------*
           move      w-let-arc-zp1-mne    to   w-tes-cod-cla-mne      .
           move      w-let-arc-zp1-des    to   w-tes-cod-cla-des      .
           move      w-let-arc-zp1-sud    to   w-tes-cod-cla-sud      .
           move      w-let-arc-zp1-umi    to   w-tes-cod-cla-umi      .
      *                          *-------------------------------------*
      *                          * Visualizzazione codice e descrizio- *
      *                          * ne classe                           *
      *                          *-------------------------------------*
           perform   vis-cod-cla-000      thru vis-cod-cla-999        .
           perform   vis-des-cla-000      thru vis-des-cla-999        .
      *                          *-------------------------------------*
      *                          * Deviazione in funzione dell'esito   *
      *                          * della lettura                       *
      *                          *-------------------------------------*
           if        w-let-arc-zp1-flg    =    spaces
                     go to acc-cod-cla-540
           else      go to acc-cod-cla-530.
       acc-cod-cla-530.
      *                          *-------------------------------------*
      *                          * Se codice classe non esistente      *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * A reimpostazione                *
      *                              *---------------------------------*
           go to     acc-cod-cla-100.
       acc-cod-cla-540.
      *                          *-------------------------------------*
      *                          * Se codice classe esistente          *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Deviazione a seconda se il co-  *
      *                              * dice classe e' ulteriormente    *
      *                              * suddiviso in gruppi             *
      *                              *---------------------------------*
           if        w-tes-cod-cla-sud    =    02
                     go to acc-cod-cla-560
           else      go to acc-cod-cla-550.
       acc-cod-cla-550.
      *                              *---------------------------------*
      *                              * Se la classe non e' ulterior-   *
      *                              * mente suddivisa in gruppi       *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Messaggio di errore         *
      *                                  *-----------------------------*
           move      "La classe non e' ulteriormente suddivisa in gruppi
      -              "      "             to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                                  *-----------------------------*
      *                                  * A reimpostazione            *
      *                                  *-----------------------------*
           go to     acc-cod-cla-100.
       acc-cod-cla-560.
      *                              *---------------------------------*
      *                              * Se la classe e' ulteriormente   *
      *                              * suddivisa in gruppi             *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Continuazione               *
      *                                  *-----------------------------*
           go to     acc-cod-cla-600.
       acc-cod-cla-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo elemento da *
      *                  * trattare                                    *
      *                  *---------------------------------------------*
           if        w-tes-tip-ele        =    01
                     go to acc-cod-cla-650
           else      go to acc-cod-cla-700.
       acc-cod-cla-650.
      *                  *---------------------------------------------*
      *                  * Se il tipo elemento da trattare corrisponde *
      *                  * ad una classe                               *
      *                  *---------------------------------------------*
       acc-cod-cla-655.
      *                      *-----------------------------------------*
      *                      * Normalizzazione gruppo                  *
      *                      *-----------------------------------------*
           if        w-tes-cod-gru        =    zero
                     go to acc-cod-cla-657.
           move      zero                 to   w-tes-cod-gru          .
           move      spaces               to   w-tes-cod-gru-mne      .
           move      spaces               to   w-tes-cod-gru-des      .
           move      zero                 to   w-tes-cod-gru-sud      .
           move      spaces               to   w-tes-cod-gru-umi      .
           move      spaces               to   w-tes-cod-gru-aut      .
           perform   vis-cod-gru-000      thru vis-cod-gru-999        .
           perform   vis-des-gru-000      thru vis-des-gru-999        .
       acc-cod-cla-657.
           move      zero                 to   w-def-cod-gru          .
       acc-cod-cla-660.
      *                      *-----------------------------------------*
      *                      * Normalizzazione del sottogruppo         *
      *                      *-----------------------------------------*
           if        w-tes-cod-sgr        =    zero
                     go to acc-cod-cla-662.
           move      zero                 to   w-tes-cod-sgr          .
           move      spaces               to   w-tes-cod-sgr-mne      .
           move      spaces               to   w-tes-cod-sgr-des      .
           move      zero                 to   w-tes-cod-sgr-sud      .
           move      spaces               to   w-tes-cod-sgr-umi      .
           move      spaces               to   w-tes-cod-sgr-aut      .
           perform   vis-cod-sgr-000      thru vis-cod-sgr-999        .
           perform   vis-des-sgr-000      thru vis-des-sgr-999        .
       acc-cod-cla-662.
           move      zero                 to   w-def-cod-sgr          .
       acc-cod-cla-675.
      *                      *-----------------------------------------*
      *                      * A controlli tasto Do                    *
      *                      *-----------------------------------------*
           go to     acc-cod-cla-800.
       acc-cod-cla-700.
      *                  *---------------------------------------------*
      *                  * Se il tipo elemento da trattare non corri-  *
      *                  * sponde ad una classe                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del valore pre-  *
      *                      * cedente del codice classe classe        *
      *                      *-----------------------------------------*
           if        w-tes-cod-cla        =    w-sav-cod-cla
                     go to acc-cod-cla-710
           else      go to acc-cod-cla-720.
       acc-cod-cla-710.
      *                      *-----------------------------------------*
      *                      * Se il valore non e' variato rispetto al *
      *                      * valore precedente                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A controlli tasto Do                *
      *                          *-------------------------------------*
           go to     acc-cod-cla-800.
       acc-cod-cla-720.
      *                      *-----------------------------------------*
      *                      * Se il valore e' variato rispetto al va- *
      *                      * lore precedente                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A normalizzazione gruppo e sotto-   *
      *                          * gruppo                              *
      *                          *-------------------------------------*
           go to     acc-cod-cla-650.
       acc-cod-cla-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-cla-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-cla-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-cla-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-cla-999.
       acc-cod-cla-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Codice classe              *
      *    *-----------------------------------------------------------*
       vis-cod-cla-000.
      *              *-------------------------------------------------*
      *              * Visualizzazione codice                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "<B"                 to   v-edm                  .
           move      w-tes-cod-cla        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-cla-200.
      *              *-------------------------------------------------*
      *              * Visualizzazione mnemonico                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su personalizzazione                   *
      *                  *---------------------------------------------*
           if        w-prs-esp-cgs-cla    not  = "M"
                     go to vis-cod-cla-999.
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-tes-cod-cla-mne    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-cla-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Descrizione classe         *
      *    *-----------------------------------------------------------*
       vis-des-cla-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      39                   to   v-pos                  .
           move      w-tes-cod-cla-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-cla-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Codice gruppo                 *
      *    *-----------------------------------------------------------*
       acc-cod-gru-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se accettazione da eseguire            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se tipo elemento classe : no accetta-   *
      *                      * zione                                   *
      *                      *-----------------------------------------*
           if        w-tes-tip-ele        =    01
                     go to acc-cod-gru-999.
      *                      *-----------------------------------------*
      *                      * Se la classe non e' ulteriormente sud-  *
      *                      * divisa in gruppi : no accettazione      *
      *                      *-----------------------------------------*
           if        w-tes-cod-cla-sud    not  = 02
                     go to acc-cod-gru-999.
       acc-cod-gru-025.
      *                  *---------------------------------------------*
      *                  * Salvataggio valori precedenti               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice classe                           *
      *                      *-----------------------------------------*
           move      w-tes-cod-cla        to   w-sav-cod-cla          .
      *                      *-----------------------------------------*
      *                      * Codice gruppo                           *
      *                      *-----------------------------------------*
           move      w-tes-cod-gru        to   w-sav-cod-gru          .
      *                      *-----------------------------------------*
      *                      * Codice sottogruppo                      *
      *                      *-----------------------------------------*
           move      w-tes-cod-sgr        to   w-sav-cod-sgr          .
       acc-cod-gru-050.
      *                  *---------------------------------------------*
      *                  * Preparazione valore di default              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se valore attuale diverso da zero : no  *
      *                      * preparazione default                    *
      *                      *-----------------------------------------*
           if        w-tes-cod-gru        not  = zero
                     go to acc-cod-gru-075.
      *                      *-----------------------------------------*
      *                      * Se valore di default pari a zero : no   *
      *                      * preparazione default                    *
      *                      *-----------------------------------------*
           if        w-def-cod-gru        =    zero
                     go to acc-cod-gru-075.
      *                      *-----------------------------------------*
      *                      * Preparazione                            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Codice gruppo                       *
      *                          *-------------------------------------*
           move      w-def-cod-gru        to   w-tes-cod-gru          .
      *                          *-------------------------------------*
      *                          * Lettura codice gruppo               *
      *                          *-------------------------------------*
           move      w-tes-cod-cla        to   w-let-arc-zp2-cla      .
           move      w-tes-cod-gru        to   w-let-arc-zp2-gru      .
           perform   let-arc-zp2-000      thru let-arc-zp2-999        .
      *                          *-------------------------------------*
      *                          * Bufferizzazione di :                *
      *                          * - Mnemonico                         *
      *                          * - Descrizione                       *
      *                          * - Ulteriore suddivisione            *
      *                          * - Unita' di misura da proporre      *
      *                          *-------------------------------------*
           move      w-let-arc-zp2-mne    to   w-tes-cod-gru-mne      .
           move      w-let-arc-zp2-des    to   w-tes-cod-gru-des      .
           move      w-let-arc-zp2-sud    to   w-tes-cod-gru-sud      .
           move      w-let-arc-zp2-umi    to   w-tes-cod-gru-umi      .
      *                          *-------------------------------------*
      *                          * Visualizzazione codice              *
      *                          *-------------------------------------*
           perform   vis-cod-gru-000      thru vis-cod-gru-999        .
      *                          *-------------------------------------*
      *                          * Visualizzazione descrizione, a meno *
      *                          * che il tipo elemento non corrispon- *
      *                          * da ad un gruppo                     *
      *                          *-------------------------------------*
           if        w-tes-tip-ele        =    02
                     go to acc-cod-gru-075.
           perform   vis-des-gru-000      thru vis-des-gru-999        .
       acc-cod-gru-075.
      *                  *---------------------------------------------*
      *                  * Continuazione                               *
      *                  *---------------------------------------------*
           go to     acc-cod-gru-100.
       acc-cod-gru-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zp2-ope      .
           move      w-tes-cod-cla        to   w-cod-mne-zp2-cla      .
           move      w-tes-cod-cla-des    to   w-cod-mne-zp2-dcl      .
           move      w-tes-cod-gru        to   w-cod-mne-zp2-gru      .
           move      zero                 to   w-cod-mne-zp2-sgr      .
           move      06                   to   w-cod-mne-zp2-lin      .
           move      30                   to   w-cod-mne-zp2-pos      .
           if        w-tes-tip-ele        =    02
                     move  09             to   w-cod-mne-zp2-dln
                     move  30             to   w-cod-mne-zp2-dps
           else      move  06             to   w-cod-mne-zp2-dln
                     move  39             to   w-cod-mne-zp2-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-zp2-cll-000  thru cod-mne-zp2-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zp2-foi-000  thru cod-mne-zp2-foi-999    .
       acc-cod-gru-110.
           perform   cod-mne-zp2-cll-000  thru cod-mne-zp2-cll-999    .
           if        w-cod-mne-zp2-ope    =    "F+"
                     go to acc-cod-gru-115.
           if        w-cod-mne-zp2-ope    =    "AC"
                     go to acc-cod-gru-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-gru-115.
           perform   cod-mne-zp2-foi-000  thru cod-mne-zp2-foi-999    .
           go to     acc-cod-gru-110.
       acc-cod-gru-120.
           move      w-cod-mne-zp2-gru    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-gru-999.
       acc-cod-gru-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-gru          .
       acc-cod-gru-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tasto Up : esclusione controlli          *
      *                  *---------------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-cod-gru-600.
      *                  *---------------------------------------------*
      *                  * Preparazione default per accettazione suc-  *
      *                  * cessiva                                     *
      *                  *---------------------------------------------*
           move      w-tes-cod-gru        to   w-def-cod-gru          .
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo elemento da *
      *                  * trattare                                    *
      *                  *---------------------------------------------*
           if        w-tes-tip-ele        =    02
                     go to acc-cod-gru-450
           else      go to acc-cod-gru-500.
       acc-cod-gru-450.
      *                  *---------------------------------------------*
      *                  * Se il tipo elemento da trattare corrisponde *
      *                  * ad un gruppo                                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del valore impo- *
      *                      * stato                                   *
      *                      *-----------------------------------------*
           if        w-tes-cod-gru        =    zero
                     go to acc-cod-gru-460
           else      go to acc-cod-gru-470.
       acc-cod-gru-460.
      *                      *-----------------------------------------*
      *                      * Se il valore impostato e' a zero        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se inserimento consentito      *
      *                          *-------------------------------------*
           if        w-cnt-mfu-vis-sgr    not  = "V"
                     go to acc-cod-gru-462.
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
           go to     acc-cod-gru-100.
       acc-cod-gru-462.
      *                          *-------------------------------------*
      *                          * Attribuzione codice automatico pro- *
      *                          * gressivo per il gruppo              *
      *                          *-------------------------------------*
           move      w-tes-cod-cla        to   w-enc-cod-gru-cla      .
           perform   att-gru-aut-000      thru att-gru-aut-999        .
      *                          *-------------------------------------*
      *                          * Codice automatico in campo di de-   *
      *                          * stinazione                          *
      *                          *-------------------------------------*
           move      w-enc-cod-gru-pos    to   w-tes-cod-gru          .
      *                          *-------------------------------------*
      *                          * Normalizzazione valori associati    *
      *                          * al gruppo                           *
      *                          *-------------------------------------*
           move      spaces               to   w-tes-cod-gru-mne      .
           move      spaces               to   w-tes-cod-gru-des      .
           move      zero                 to   w-tes-cod-gru-sud      .
           move      spaces               to   w-tes-cod-gru-umi      .
      *                          *-------------------------------------*
      *                          * Segnale di attribuzione codice ese- *
      *                          * guita automaticamente               *
      *                          *-------------------------------------*
           move      "#"                  to   w-tes-cod-gru-aut      .
      *                          *-------------------------------------*
      *                          * Visualizzazione codice gruppo       *
      *                          *-------------------------------------*
           perform   vis-cod-gru-000      thru vis-cod-gru-999        .
      *                          *-------------------------------------*
      *                          * Prosecuzione                        *
      *                          *-------------------------------------*
           go to     acc-cod-gru-600.
       acc-cod-gru-470.
      *                      *-----------------------------------------*
      *                      * Se il valore impostato e' a non-zero    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura del codice gruppo           *
      *                          *-------------------------------------*
           move      w-tes-cod-cla        to   w-let-arc-zp2-cla      .
           move      w-tes-cod-gru        to   w-let-arc-zp2-gru      .
           perform   let-arc-zp2-000      thru let-arc-zp2-999        .
      *                          *-------------------------------------*
      *                          * Bufferizzazione di :                *
      *                          * - Mnemonico                         *
      *                          * - Descrizione                       *
      *                          * - Ulteriore suddivisione            *
      *                          * - Unita' di misura da proporre      *
      *                          *-------------------------------------*
           move      w-let-arc-zp2-mne    to   w-tes-cod-gru-mne      .
           move      w-let-arc-zp2-des    to   w-tes-cod-gru-des      .
           move      w-let-arc-zp2-sud    to   w-tes-cod-gru-sud      .
           move      w-let-arc-zp2-umi    to   w-tes-cod-gru-umi      .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     acc-cod-gru-600.
       acc-cod-gru-500.
      *                  *---------------------------------------------*
      *                  * Se il tipo elemento da trattare non corri-  *
      *                  * sponde ad un gruppo                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del valore impo- *
      *                      * stato                                   *
      *                      *-----------------------------------------*
           if        w-tes-cod-gru        =    zero
                     go to acc-cod-gru-510
           else      go to acc-cod-gru-520.
       acc-cod-gru-510.
      *                      *-----------------------------------------*
      *                      * Se il valore impostato e' a zero        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura del codice gruppo           *
      *                          *-------------------------------------*
           move      w-tes-cod-cla        to   w-let-arc-zp2-cla      .
           move      w-tes-cod-gru        to   w-let-arc-zp2-gru      .
           perform   let-arc-zp2-000      thru let-arc-zp2-999        .
      *                          *-------------------------------------*
      *                          * Bufferizzazione di :                *
      *                          * - Mnemonico                         *
      *                          * - Descrizione                       *
      *                          * - Ulteriore suddivisione            *
      *                          * - Unita' di misura da proporre      *
      *                          *-------------------------------------*
           move      w-let-arc-zp2-mne    to   w-tes-cod-gru-mne      .
           move      w-let-arc-zp2-des    to   w-tes-cod-gru-des      .
           move      w-let-arc-zp2-sud    to   w-tes-cod-gru-sud      .
           move      w-let-arc-zp2-umi    to   w-tes-cod-gru-umi      .
      *                          *-------------------------------------*
      *                          * Visualizzazione descrizione per il  *
      *                          * gruppo                              *
      *                          *-------------------------------------*
           perform   vis-des-gru-000      thru vis-des-gru-999        .
      *                          *-------------------------------------*
      *                          * Se tasto Up : a dipendenze da impo- *
      *                          * stazione                            *
      *                          *-------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-cod-gru-600.
      *                          *-------------------------------------*
      *                          * Altrimenti : a reimpostazione       *
      *                          *-------------------------------------*
           go to     acc-cod-gru-100.
       acc-cod-gru-520.
      *                      *-----------------------------------------*
      *                      * Se il valore impostato e' a non-zero    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura del codice gruppo           *
      *                          *-------------------------------------*
           move      w-tes-cod-cla        to   w-let-arc-zp2-cla      .
           move      w-tes-cod-gru        to   w-let-arc-zp2-gru      .
           perform   let-arc-zp2-000      thru let-arc-zp2-999        .
      *                          *-------------------------------------*
      *                          * Bufferizzazione di :                *
      *                          * - Mnemonico                         *
      *                          * - Descrizione                       *
      *                          * - Ulteriore suddivisione            *
      *                          * - Unita' di misura da proporre      *
      *                          *-------------------------------------*
           move      w-let-arc-zp2-mne    to   w-tes-cod-gru-mne      .
           move      w-let-arc-zp2-des    to   w-tes-cod-gru-des      .
           move      w-let-arc-zp2-sud    to   w-tes-cod-gru-sud      .
           move      w-let-arc-zp2-umi    to   w-tes-cod-gru-umi      .
      *                          *-------------------------------------*
      *                          * Visualizzazione codice e descrizio- *
      *                          * ne gruppo                           *
      *                          *-------------------------------------*
           perform   vis-cod-gru-000      thru vis-cod-gru-999        .
           perform   vis-des-gru-000      thru vis-des-gru-999        .
      *                          *-------------------------------------*
      *                          * Deviazione in funzione dell'esito   *
      *                          * della lettura                       *
      *                          *-------------------------------------*
           if        w-let-arc-zp2-flg    =    spaces
                     go to acc-cod-gru-540
           else      go to acc-cod-gru-530.
       acc-cod-gru-530.
      *                          *-------------------------------------*
      *                          * Se codice gruppo non esistente      *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * A reimpostazione                *
      *                              *---------------------------------*
           go to     acc-cod-gru-100.
       acc-cod-gru-540.
      *                          *-------------------------------------*
      *                          * Se codice gruppo esistente          *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Deviazione a seconda se il co-  *
      *                              * dice gruppo e' ulteriormente    *
      *                              * suddiviso in sottogruppi        *
      *                              *---------------------------------*
           if        w-tes-cod-gru-sud    =    02
                     go to acc-cod-gru-560
           else      go to acc-cod-gru-550.
       acc-cod-gru-550.
      *                              *---------------------------------*
      *                              * Se il gruppo non e' ulterior-   *
      *                              * mente suddiviso in sottogruppi  *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Messaggio di errore         *
      *                                  *-----------------------------*
           move      "Il gruppo non e' ulteriormente suddiviso in sottog
      -              "ruppi "             to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                                  *-----------------------------*
      *                                  * A reimpostazione            *
      *                                  *-----------------------------*
           go to     acc-cod-gru-100.
       acc-cod-gru-560.
      *                              *---------------------------------*
      *                              * Se il gruppo e' ulteriormente   *
      *                              * suddivisa in sottogruppi        *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Continuazione               *
      *                                  *-----------------------------*
           go to     acc-cod-gru-600.
       acc-cod-gru-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo elemento da *
      *                  * trattare                                    *
      *                  *---------------------------------------------*
           if        w-tes-tip-ele        =    02
                     go to acc-cod-gru-650
           else      go to acc-cod-gru-700.
       acc-cod-gru-650.
      *                  *---------------------------------------------*
      *                  * Se il tipo elemento da trattare corrisponde *
      *                  * ad un gruppo                                *
      *                  *---------------------------------------------*
       acc-cod-gru-655.
      *                      *-----------------------------------------*
      *                      * Normalizzazione sottogruppo             *
      *                      *-----------------------------------------*
           if        w-tes-cod-sgr        =    zero
                     go to acc-cod-gru-657.
           move      zero                 to   w-tes-cod-sgr          .
           move      spaces               to   w-tes-cod-sgr-mne      .
           move      spaces               to   w-tes-cod-sgr-des      .
           move      zero                 to   w-tes-cod-sgr-sud      .
           move      spaces               to   w-tes-cod-sgr-umi      .
           move      spaces               to   w-tes-cod-sgr-aut      .
           perform   vis-cod-sgr-000      thru vis-cod-sgr-999        .
           perform   vis-des-sgr-000      thru vis-des-sgr-999        .
       acc-cod-gru-657.
           move      zero                 to   w-def-cod-sgr          .
       acc-cod-gru-660.
      *                      *-----------------------------------------*
      *                      * A controlli tasto Do                    *
      *                      *-----------------------------------------*
           go to     acc-cod-gru-800.
       acc-cod-gru-700.
      *                  *---------------------------------------------*
      *                  * Se il tipo elemento da trattare non corri-  *
      *                  * sponde ad un gruppo                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del valore pre-  *
      *                      * cedente del codice gruppo               *
      *                      *-----------------------------------------*
           if        w-tes-cod-gru        =    w-sav-cod-gru
                     go to acc-cod-gru-710
           else      go to acc-cod-gru-720.
       acc-cod-gru-710.
      *                      *-----------------------------------------*
      *                      * Se il valore non e' variato rispetto al *
      *                      * valore precedente                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A controlli tasto Do                *
      *                          *-------------------------------------*
           go to     acc-cod-gru-800.
       acc-cod-gru-720.
      *                      *-----------------------------------------*
      *                      * Se il valore e' variato rispetto al va- *
      *                      * lore precedente                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A normalizzazione sottogruppo       *
      *                          *-------------------------------------*
           go to     acc-cod-gru-650.
       acc-cod-gru-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-gru-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-gru-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-gru-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-gru-999.
       acc-cod-gru-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Codice gruppo              *
      *    *-----------------------------------------------------------*
       vis-cod-gru-000.
      *              *-------------------------------------------------*
      *              * Visualizzazione codice                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "<B"                 to   v-edm                  .
           move      w-tes-cod-gru        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-gru-200.
      *              *-------------------------------------------------*
      *              * Visualizzazione mnemonico                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su personalizzazione                   *
      *                  *---------------------------------------------*
           if        w-prs-esp-cgs-gru    not  = "M"
                     go to vis-cod-gru-999.
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-tes-cod-gru-mne    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-gru-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Descrizione gruppo         *
      *    *-----------------------------------------------------------*
       vis-des-gru-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      39                   to   v-pos                  .
           move      w-tes-cod-gru-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-gru-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Codice sottogruppo            *
      *    *-----------------------------------------------------------*
       acc-cod-sgr-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se accettazione da eseguire            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se tipo elemento classe : no accetta-   *
      *                      * zione                                   *
      *                      *-----------------------------------------*
           if        w-tes-tip-ele        =    01
                     go to acc-cod-sgr-999.
      *                      *-----------------------------------------*
      *                      * Se tipo elemento gruppo : no accetta-   *
      *                      * zione                                   *
      *                      *-----------------------------------------*
           if        w-tes-tip-ele        =    02
                     go to acc-cod-sgr-999.
      *                      *-----------------------------------------*
      *                      * Se la classe non e' ulteriormente sud-  *
      *                      * divisa in gruppi : no accettazione      *
      *                      *-----------------------------------------*
           if        w-tes-cod-cla-sud    not  = 02
                     go to acc-cod-sgr-999.
      *                      *-----------------------------------------*
      *                      * Se il gruppo non e' ulteriormente sud-  *
      *                      * divisa in sottogruppi : no accettazione *
      *                      *-----------------------------------------*
           if        w-tes-cod-gru-sud    not  = 02
                     go to acc-cod-sgr-999.
       acc-cod-sgr-025.
      *                  *---------------------------------------------*
      *                  * Salvataggio valori precedenti               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice classe                           *
      *                      *-----------------------------------------*
           move      w-tes-cod-cla        to   w-sav-cod-cla          .
      *                      *-----------------------------------------*
      *                      * Codice gruppo                           *
      *                      *-----------------------------------------*
           move      w-tes-cod-gru        to   w-sav-cod-gru          .
      *                      *-----------------------------------------*
      *                      * Codice sottogruppo                      *
      *                      *-----------------------------------------*
           move      w-tes-cod-sgr        to   w-sav-cod-sgr          .
       acc-cod-sgr-050.
      *                  *---------------------------------------------*
      *                  * Preparazione valore di default              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se valore attuale diverso da zero : no  *
      *                      * preparazione default                    *
      *                      *-----------------------------------------*
           if        w-tes-cod-sgr        not  = zero
                     go to acc-cod-sgr-075.
      *                      *-----------------------------------------*
      *                      * Se valore di default pari a zero : no   *
      *                      * preparazione default                    *
      *                      *-----------------------------------------*
           if        w-def-cod-sgr        =    zero
                     go to acc-cod-sgr-075.
      *                      *-----------------------------------------*
      *                      * Preparazione                            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Codice sottogruppo                  *
      *                          *-------------------------------------*
           move      w-def-cod-sgr        to   w-tes-cod-sgr          .
      *                          *-------------------------------------*
      *                          * Lettura codice sottogruppo          *
      *                          *-------------------------------------*
           move      w-tes-cod-cla        to   w-let-arc-zp3-cla      .
           move      w-tes-cod-gru        to   w-let-arc-zp3-gru      .
           move      w-tes-cod-sgr        to   w-let-arc-zp3-sgr      .
           perform   let-arc-zp3-000      thru let-arc-zp3-999        .
      *                          *-------------------------------------*
      *                          * Bufferizzazione di :                *
      *                          * - Mnemonico                         *
      *                          * - Descrizione                       *
      *                          * - Ulteriore suddivisione            *
      *                          * - Unita' di misura da proporre      *
      *                          *-------------------------------------*
           move      w-let-arc-zp3-mne    to   w-tes-cod-sgr-mne      .
           move      w-let-arc-zp3-des    to   w-tes-cod-sgr-des      .
           move      w-let-arc-zp3-sud    to   w-tes-cod-sgr-sud      .
           move      w-let-arc-zp3-umi    to   w-tes-cod-sgr-umi      .
      *                          *-------------------------------------*
      *                          * Visualizzazione codice              *
      *                          *-------------------------------------*
           perform   vis-cod-sgr-000      thru vis-cod-sgr-999        .
      *                          *-------------------------------------*
      *                          * Visualizzazione descrizione, a meno *
      *                          * che il tipo elemento non corrispon- *
      *                          * da ad un sottogruppo                *
      *                          *-------------------------------------*
           if        w-tes-tip-ele        =    03
                     go to acc-cod-sgr-075.
           perform   vis-des-sgr-000      thru vis-des-sgr-999        .
       acc-cod-sgr-075.
      *                  *---------------------------------------------*
      *                  * Continuazione                               *
      *                  *---------------------------------------------*
           go to     acc-cod-sgr-100.
       acc-cod-sgr-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zp3-ope      .
           move      w-tes-cod-cla        to   w-cod-mne-zp3-cla      .
           move      w-tes-cod-cla-des    to   w-cod-mne-zp3-dcl      .
           move      w-tes-cod-gru        to   w-cod-mne-zp3-gru      .
           move      w-tes-cod-gru-des    to   w-cod-mne-zp3-dgr      .
           move      w-tes-cod-sgr        to   w-cod-mne-zp3-sgr      .
           move      07                   to   w-cod-mne-zp3-lin      .
           move      30                   to   w-cod-mne-zp3-pos      .
           move      09                   to   w-cod-mne-zp3-dln      .
           move      30                   to   w-cod-mne-zp3-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-zp3-cll-000  thru cod-mne-zp3-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zp3-foi-000  thru cod-mne-zp3-foi-999    .
       acc-cod-sgr-110.
           perform   cod-mne-zp3-cll-000  thru cod-mne-zp3-cll-999    .
           if        w-cod-mne-zp3-ope    =    "F+"
                     go to acc-cod-sgr-115.
           if        w-cod-mne-zp3-ope    =    "AC"
                     go to acc-cod-sgr-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-sgr-115.
           perform   cod-mne-zp3-foi-000  thru cod-mne-zp3-foi-999    .
           go to     acc-cod-sgr-110.
       acc-cod-sgr-120.
           move      w-cod-mne-zp3-sgr    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-sgr-999.
       acc-cod-sgr-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-sgr          .
       acc-cod-sgr-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tasto Up : esclusione controlli          *
      *                  *---------------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-cod-sgr-600.
      *                  *---------------------------------------------*
      *                  * Preparazione default per accettazione suc-  *
      *                  * cessiva                                     *
      *                  *---------------------------------------------*
           move      w-tes-cod-sgr        to   w-def-cod-sgr          .
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo elemento da *
      *                  * trattare                                    *
      *                  *---------------------------------------------*
           if        w-tes-tip-ele        =    03
                     go to acc-cod-sgr-450
           else      go to acc-cod-sgr-500.
       acc-cod-sgr-450.
      *                  *---------------------------------------------*
      *                  * Se il tipo elemento da trattare corrisponde *
      *                  * ad un sottogruppo                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del valore impo- *
      *                      * stato                                   *
      *                      *-----------------------------------------*
           if        w-tes-cod-sgr        =    zero
                     go to acc-cod-sgr-460
           else      go to acc-cod-sgr-470.
       acc-cod-sgr-460.
      *                      *-----------------------------------------*
      *                      * Se il valore impostato e' a zero        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se inserimento consentito      *
      *                          *-------------------------------------*
           if        w-cnt-mfu-vis-sgr    not  = "V"
                     go to acc-cod-sgr-462.
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
           go to     acc-cod-sgr-100.
       acc-cod-sgr-462.
      *                          *-------------------------------------*
      *                          * Attribuzione codice automatico pro- *
      *                          * gressivo per il sottogruppo         *
      *                          *-------------------------------------*
           move      w-tes-cod-cla        to   w-enc-cod-sgr-cla      .
           move      w-tes-cod-gru        to   w-enc-cod-sgr-gru      .
           perform   att-sgr-aut-000      thru att-sgr-aut-999        .
      *                          *-------------------------------------*
      *                          * Codice automatico in campo di de-   *
      *                          * stinazione                          *
      *                          *-------------------------------------*
           move      w-enc-cod-sgr-pos    to   w-tes-cod-sgr          .
      *                          *-------------------------------------*
      *                          * Normalizzazione valori associati    *
      *                          * al sottogruppo                      *
      *                          *-------------------------------------*
           move      spaces               to   w-tes-cod-sgr-mne      .
           move      spaces               to   w-tes-cod-sgr-des      .
           move      zero                 to   w-tes-cod-sgr-sud      .
           move      spaces               to   w-tes-cod-sgr-umi      .
      *                          *-------------------------------------*
      *                          * Segnale di attribuzione codice ese- *
      *                          * guita automaticamente               *
      *                          *-------------------------------------*
           move      "#"                  to   w-tes-cod-sgr-aut      .
      *                          *-------------------------------------*
      *                          * Visualizzazione codice sottogruppo  *
      *                          *-------------------------------------*
           perform   vis-cod-sgr-000      thru vis-cod-sgr-999        .
      *                          *-------------------------------------*
      *                          * Prosecuzione                        *
      *                          *-------------------------------------*
           go to     acc-cod-sgr-600.
       acc-cod-sgr-470.
      *                      *-----------------------------------------*
      *                      * Se il valore impostato e' a non-zero    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura codice sottogruppo          *
      *                          *-------------------------------------*
           move      w-tes-cod-cla        to   w-let-arc-zp3-cla      .
           move      w-tes-cod-gru        to   w-let-arc-zp3-gru      .
           move      w-tes-cod-sgr        to   w-let-arc-zp3-sgr      .
           perform   let-arc-zp3-000      thru let-arc-zp3-999        .
      *                          *-------------------------------------*
      *                          * Bufferizzazione di :                *
      *                          * - Mnemonico                         *
      *                          * - Descrizione                       *
      *                          * - Ulteriore suddivisione            *
      *                          * - Unita' di misura da proporre      *
      *                          *-------------------------------------*
           move      w-let-arc-zp3-mne    to   w-tes-cod-sgr-mne      .
           move      w-let-arc-zp3-des    to   w-tes-cod-sgr-des      .
           move      w-let-arc-zp3-sud    to   w-tes-cod-sgr-sud      .
           move      w-let-arc-zp3-umi    to   w-tes-cod-sgr-umi      .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     acc-cod-sgr-600.
       acc-cod-sgr-500.
      *                  *---------------------------------------------*
      *                  * Se il tipo elemento da trattare non corri-  *
      *                  * sponde ad un sottogruppo                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del valore impo- *
      *                      * stato                                   *
      *                      *-----------------------------------------*
           if        w-tes-cod-sgr        =    zero
                     go to acc-cod-sgr-510
           else      go to acc-cod-sgr-520.
       acc-cod-sgr-510.
      *                      *-----------------------------------------*
      *                      * Se il valore impostato e' a zero        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura codice sottogruppo          *
      *                          *-------------------------------------*
           move      w-tes-cod-cla        to   w-let-arc-zp3-cla      .
           move      w-tes-cod-gru        to   w-let-arc-zp3-gru      .
           move      w-tes-cod-sgr        to   w-let-arc-zp3-sgr      .
           perform   let-arc-zp3-000      thru let-arc-zp3-999        .
      *                          *-------------------------------------*
      *                          * Bufferizzazione di :                *
      *                          * - Mnemonico                         *
      *                          * - Descrizione                       *
      *                          * - Ulteriore suddivisione            *
      *                          * - Unita' di misura da proporre      *
      *                          *-------------------------------------*
           move      w-let-arc-zp3-mne    to   w-tes-cod-sgr-mne      .
           move      w-let-arc-zp3-des    to   w-tes-cod-sgr-des      .
           move      w-let-arc-zp3-sud    to   w-tes-cod-sgr-sud      .
           move      w-let-arc-zp3-umi    to   w-tes-cod-sgr-umi      .
      *                          *-------------------------------------*
      *                          * Visualizzazione codice e descrizio- *
      *                          * ne sottogruppo                      *
      *                          *-------------------------------------*
           perform   vis-cod-sgr-000      thru vis-cod-sgr-999        .
           perform   vis-des-sgr-000      thru vis-des-sgr-999        .
      *                          *-------------------------------------*
      *                          * Se tasto Up : a dipendenze da impo- *
      *                          * stazione                            *
      *                          *-------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-cod-sgr-600.
      *                          *-------------------------------------*
      *                          * Altrimenti : a reimpostazione       *
      *                          *-------------------------------------*
           go to     acc-cod-sgr-100.
       acc-cod-sgr-520.
      *                      *-----------------------------------------*
      *                      * Se il valore impostato e' a non-zero    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura codice sottogruppo          *
      *                          *-------------------------------------*
           move      w-tes-cod-cla        to   w-let-arc-zp3-cla      .
           move      w-tes-cod-gru        to   w-let-arc-zp3-gru      .
           move      w-tes-cod-sgr        to   w-let-arc-zp3-sgr      .
           perform   let-arc-zp3-000      thru let-arc-zp3-999        .
      *                          *-------------------------------------*
      *                          * Bufferizzazione di :                *
      *                          * - Mnemonico                         *
      *                          * - Descrizione                       *
      *                          * - Ulteriore suddivisione            *
      *                          * - Unita' di misura da proporre      *
      *                          *-------------------------------------*
           move      w-let-arc-zp3-mne    to   w-tes-cod-sgr-mne      .
           move      w-let-arc-zp3-des    to   w-tes-cod-sgr-des      .
           move      w-let-arc-zp3-sud    to   w-tes-cod-sgr-sud      .
           move      w-let-arc-zp3-umi    to   w-tes-cod-sgr-umi      .
      *                          *-------------------------------------*
      *                          * Visualizzazione descrizione per il  *
      *                          * sottogruppo                         *
      *                          *-------------------------------------*
           perform   vis-des-sgr-000      thru vis-des-sgr-999        .
      *                          *-------------------------------------*
      *                          * Deviazione in funzione dell'esito   *
      *                          * della lettura                       *
      *                          *-------------------------------------*
           if        w-let-arc-zp3-flg    =    spaces
                     go to acc-cod-sgr-540
           else      go to acc-cod-sgr-530.
       acc-cod-sgr-530.
      *                          *-------------------------------------*
      *                          * Se codice sottogruppo non esistente *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * A reimpostazione                *
      *                              *---------------------------------*
           go to     acc-cod-sgr-100.
       acc-cod-sgr-540.
      *                          *-------------------------------------*
      *                          * Se codice sottogruppo esistente     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Deviazione a seconda se il co-  *
      *                              * dice sottogruppo e' ulterior-   *
      *                              * mente suddiviso                 *
      *                              *---------------------------------*
           if        w-tes-cod-sgr-sud    =    02
                     go to acc-cod-sgr-560
           else      go to acc-cod-sgr-550.
       acc-cod-sgr-550.
      *                              *---------------------------------*
      *                              * Se il sottogruppo non e' ulte-  *
      *                              * mente suddiviso                 *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Messaggio di errore         *
      *                                  *-----------------------------*
           move      "Il sottogruppo non e' ulteriormente suddiviso     
      -              "      "             to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                                  *-----------------------------*
      *                                  * A reimpostazione            *
      *                                  *-----------------------------*
           go to     acc-cod-sgr-100.
       acc-cod-sgr-560.
      *                              *---------------------------------*
      *                              * Se il sottogruppo e' ulterior-  *
      *                              * mente suddiviso                 *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Continuazione               *
      *                                  *-----------------------------*
           go to     acc-cod-sgr-600.
       acc-cod-sgr-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo elemento da *
      *                  * trattare                                    *
      *                  *---------------------------------------------*
           if        w-tes-tip-ele        =    03
                     go to acc-cod-sgr-650
           else      go to acc-cod-sgr-700.
       acc-cod-sgr-650.
      *                  *---------------------------------------------*
      *                  * Se il tipo elemento da trattare corrisponde *
      *                  * ad un sottogruppo                           *
      *                  *---------------------------------------------*
       acc-cod-sgr-655.
      *                      *-----------------------------------------*
      *                      * A controlli tasto Do                    *
      *                      *-----------------------------------------*
           go to     acc-cod-sgr-800.
       acc-cod-sgr-700.
      *                  *---------------------------------------------*
      *                  * Se il tipo elemento da trattare non corri-  *
      *                  * sponde ad un sottogruppo                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del valore pre-  *
      *                      * cedente del codice sottogruppo          *
      *                      *-----------------------------------------*
           if        w-tes-cod-sgr        =    w-sav-cod-sgr
                     go to acc-cod-sgr-710
           else      go to acc-cod-sgr-720.
       acc-cod-sgr-710.
      *                      *-----------------------------------------*
      *                      * Se il valore non e' variato rispetto al *
      *                      * valore precedente                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A controlli tasto Do                *
      *                          *-------------------------------------*
           go to     acc-cod-sgr-800.
       acc-cod-sgr-720.
      *                      *-----------------------------------------*
      *                      * Se il valore e' variato rispetto al va- *
      *                      * lore precedente                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A controlli tasto Do                *
      *                          *-------------------------------------*
           go to     acc-cod-sgr-800.
       acc-cod-sgr-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-sgr-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-sgr-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-sgr-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-sgr-999.
       acc-cod-sgr-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Codice sottogruppo         *
      *    *-----------------------------------------------------------*
       vis-cod-sgr-000.
      *              *-------------------------------------------------*
      *              * Visualizzazione codice                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "<B"                 to   v-edm                  .
           move      w-tes-cod-sgr        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-sgr-200.
      *              *-------------------------------------------------*
      *              * Visualizzazione mnemonico                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su personalizzazione                   *
      *                  *---------------------------------------------*
           if        w-prs-esp-cgs-sgr    not  = "M"
                     go to vis-cod-sgr-999.
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-tes-cod-sgr-mne    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-sgr-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Descrizione sottogruppo    *
      *    *-----------------------------------------------------------*
       vis-des-sgr-000.
       vis-des-sgr-999.
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
      *                          *-------------------------------------*
      *                          * Decremento numero pagina attuale    *
      *                          *-------------------------------------*
           subtract  1                    from w-cnt-sts-imp-npt      .
      *                          *-------------------------------------*
      *                          * Riciclo ad impostazione testata     *
      *                          *-------------------------------------*
           go to     acc-nok-reg-200.
       acc-nok-reg-400.
      *                      *-----------------------------------------*
      *                      * Se spostamento a pagina successiva      *
      *                      *-----------------------------------------*
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
                     go to acc-nok-reg-800.
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
      *                          *-------------------------------------*
      *                          * Incremento numero pagina attuale    *
      *                          *-------------------------------------*
           add       1                    to   w-cnt-sts-imp-npt      .
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
      *              * La testata e' composta di nr. 1 pagina          *
      *              *-------------------------------------------------*
           move      1                    to   w-cnt-sts-imp-mpt      .
       dmp-tes-reg-999.
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
      *                  * Descrizione                                 *
      *                  *---------------------------------------------*
           perform   acc-des-cgs-000      thru acc-des-cgs-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-110.
      *                  *---------------------------------------------*
      *                  * Mnemonico                                   *
      *                  *---------------------------------------------*
           perform   acc-mne-cgs-000      thru acc-mne-cgs-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-100.
       acc-tes-reg-120.
      *                  *---------------------------------------------*
      *                  * Numero di sequenza                          *
      *                  *---------------------------------------------*
           perform   acc-sqz-num-000      thru acc-sqz-num-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-110.
       acc-tes-reg-130.
      *                  *---------------------------------------------*
      *                  * Ulteriore suddivisione                      *
      *                  *---------------------------------------------*
           perform   acc-ult-sud-000      thru acc-ult-sud-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-120.
       acc-tes-reg-140.
      *                  *---------------------------------------------*
      *                  * Unita' di misura di default                 *
      *                  *---------------------------------------------*
           perform   acc-umi-def-000      thru acc-umi-def-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-130.
       acc-tes-reg-150.
      *                  *---------------------------------------------*
      *                  * Ultimo autocodice                           *
      *                  *---------------------------------------------*
           perform   acc-ult-cod-000      thru acc-ult-cod-999        .
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
       acc-tes-reg-200.
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
      *              * Descrizione                                     *
      *              *-------------------------------------------------*
           perform   vis-des-cgs-000      thru vis-des-cgs-999        .
      *              *-------------------------------------------------*
      *              * Mnemonico                                       *
      *              *-------------------------------------------------*
           perform   vis-mne-cgs-000      thru vis-mne-cgs-999        .
      *              *-------------------------------------------------*
      *              * Numero di sequenza                              *
      *              *-------------------------------------------------*
           perform   vis-sqz-num-000      thru vis-sqz-num-999        .
      *              *-------------------------------------------------*
      *              * Ulteriore suddivisione                          *
      *              *-------------------------------------------------*
           perform   vis-ult-sud-000      thru vis-ult-sud-999        .
      *              *-------------------------------------------------*
      *              * Unita' di misura di default                     *
      *              *-------------------------------------------------*
           perform   vis-umi-def-000      thru vis-umi-def-999        .
      *              *-------------------------------------------------*
      *              * Descrizione unita' di misura di default         *
      *              *-------------------------------------------------*
           perform   vis-umi-des-000      thru vis-umi-des-999        .
      *              *-------------------------------------------------*
      *              * Ultimo autocodice                               *
      *              *-------------------------------------------------*
           perform   vis-ult-cod-000      thru vis-ult-cod-999        .
           go to     vis-tes-reg-999.
       vis-tes-reg-200.
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
           move      09                   to   v-lin                  .
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
      *              * Prompts per pagina uno                          *
      *              *-------------------------------------------------*
           perform   pmt-tes-uno-000      thru pmt-tes-uno-999        .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-200.
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts testata per pagina uno            *
      *    *-----------------------------------------------------------*
       pmt-tes-uno-000.
      *              *-------------------------------------------------*
      *              * Descrizione                                     *
      *              *-------------------------------------------------*
           perform   pmt-des-cgs-000      thru pmt-des-cgs-999        .
      *              *-------------------------------------------------*
      *              * Mnemonico                                       *
      *              *-------------------------------------------------*
           perform   pmt-mne-cgs-000      thru pmt-mne-cgs-999        .
      *              *-------------------------------------------------*
      *              * Numero di sequenza                              *
      *              *-------------------------------------------------*
           perform   pmt-sqz-num-000      thru pmt-sqz-num-999        .
      *              *-------------------------------------------------*
      *              * Ulteriore suddivisione                          *
      *              *-------------------------------------------------*
           perform   pmt-ult-sud-000      thru pmt-ult-sud-999        .
      *              *-------------------------------------------------*
      *              * Unita' di misura di default                     *
      *              *-------------------------------------------------*
           perform   pmt-umi-def-000      thru pmt-umi-def-999        .
      *              *-------------------------------------------------*
      *              * Ultimo autocodice                               *
      *              *-------------------------------------------------*
           perform   pmt-ult-cod-000      thru pmt-ult-cod-999        .
       pmt-tes-uno-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Descrizione                      *
      *    *-----------------------------------------------------------*
       pmt-des-cgs-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo funzionamento   *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    spaces
                     go to pmt-des-cgs-200
           else      go to pmt-des-cgs-400.
       pmt-des-cgs-200.
      *              *-------------------------------------------------*
      *              * Se in impostazione campi chiave                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt generico                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Descrizione                :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-des-cgs-999.
       pmt-des-cgs-400.
      *              *-------------------------------------------------*
      *              * Se non in impostazione campi chiave             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo elemento    *
      *                  *---------------------------------------------*
           if        w-tes-tip-ele        =    01
                     go to pmt-des-cgs-500
           else if   w-tes-tip-ele        =    02
                     go to pmt-des-cgs-600
           else if   w-tes-tip-ele        =    03
                     go to pmt-des-cgs-700
           else      go to pmt-des-cgs-999.
       pmt-des-cgs-500.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento : classe                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt per classe                       *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Descrizione classe         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pmt-des-cgs-999.
       pmt-des-cgs-600.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento : gruppo                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt per gruppo                       *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Descrizione gruppo         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pmt-des-cgs-999.
       pmt-des-cgs-700.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento : sottogruppo              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt per sottogruppo                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Descrizione sottogruppo    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pmt-des-cgs-999.
       pmt-des-cgs-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Mnemonico                        *
      *    *-----------------------------------------------------------*
       pmt-mne-cgs-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo funzionamento   *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    spaces
                     go to pmt-mne-cgs-200
           else      go to pmt-mne-cgs-400.
       pmt-mne-cgs-200.
      *              *-------------------------------------------------*
      *              * Se in impostazione campi chiave                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt generico                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Mnemonico                  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-mne-cgs-999.
       pmt-mne-cgs-400.
      *              *-------------------------------------------------*
      *              * Se non in impostazione campi chiave             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo elemento    *
      *                  *---------------------------------------------*
           if        w-tes-tip-ele        =    01
                     go to pmt-mne-cgs-500
           else if   w-tes-tip-ele        =    02
                     go to pmt-mne-cgs-600
           else if   w-tes-tip-ele        =    03
                     go to pmt-mne-cgs-700
           else      go to pmt-mne-cgs-999.
       pmt-mne-cgs-500.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento : classe                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt per classe                       *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Mnemonico classe           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pmt-mne-cgs-999.
       pmt-mne-cgs-600.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento : gruppo                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt per gruppo                       *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Mnemonico gruppo           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pmt-mne-cgs-999.
       pmt-mne-cgs-700.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento : sottogruppo              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt per sottogruppo                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Mnemonico sottogruppo      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pmt-mne-cgs-999.
       pmt-mne-cgs-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Numero di sequenza               *
      *    *-----------------------------------------------------------*
       pmt-sqz-num-000.
      *              *-------------------------------------------------*
      *              * Visualizzazione prompt                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero di sequenza         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-sqz-num-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Ulteriore suddivisione           *
      *    *-----------------------------------------------------------*
       pmt-ult-sud-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo funzionamento   *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    spaces
                     go to pmt-ult-sud-200
           else      go to pmt-ult-sud-400.
       pmt-ult-sud-200.
      *              *-------------------------------------------------*
      *              * Se in impostazione campi chiave                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita senza alcun prompt                   *
      *                  *---------------------------------------------*
           go to     pmt-ult-sud-999.
       pmt-ult-sud-400.
      *              *-------------------------------------------------*
      *              * Se non in impostazione campi chiave             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo elemento    *
      *                  *---------------------------------------------*
           if        w-tes-tip-ele        =    01
                     go to pmt-ult-sud-500
           else if   w-tes-tip-ele        =    02
                     go to pmt-ult-sud-600
           else if   w-tes-tip-ele        =    03
                     go to pmt-ult-sud-700
           else      go to pmt-ult-sud-999.
       pmt-ult-sud-500.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento : classe                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt per classe                       *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      48                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Si/No ulteriore suddivisione in gruppi         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pmt-ult-sud-999.
       pmt-ult-sud-600.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento : gruppo                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt per gruppo                       *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      48                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Si/No ulteriore suddivisione in sottogruppi    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pmt-ult-sud-999.
       pmt-ult-sud-700.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento : sottogruppo              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Uscita senza alcun prompt               *
      *                      *-----------------------------------------*
           go to     pmt-ult-sud-999.
       pmt-ult-sud-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Unita' di misura du default      *
      *    *-----------------------------------------------------------*
       pmt-umi-def-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo funzionamento   *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    spaces
                     go to pmt-umi-def-200
           else      go to pmt-umi-def-400.
       pmt-umi-def-200.
      *              *-------------------------------------------------*
      *              * Se in impostazione campi chiave                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita senza alcun prompt                   *
      *                  *---------------------------------------------*
           go to     pmt-umi-def-999.
       pmt-umi-def-400.
      *              *-------------------------------------------------*
      *              * Se non in impostazione campi chiave             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo elemento    *
      *                  *---------------------------------------------*
           if        w-tes-tip-ele        =    01
                     go to pmt-umi-def-500
           else if   w-tes-tip-ele        =    02
                     go to pmt-umi-def-600
           else if   w-tes-tip-ele        =    03
                     go to pmt-umi-def-700
           else      go to pmt-umi-def-999.
       pmt-umi-def-500.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento : classe                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompts per classe                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      48                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Unita' di misura da proporre per i prodotti    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      48                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "               appartenenti a questa classe     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pmt-umi-def-999.
       pmt-umi-def-600.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento : gruppo                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompts per gruppo                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      48                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Unita' di misura da proporre per i prodotti    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      48                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "               appartenenti a questo gruppo     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pmt-umi-def-999.
       pmt-umi-def-700.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento : sottogruppo              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompts per sottogruppo                 *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      48                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Unita' di misura da proporre per i prodotti    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      48                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "          appartenenti a questo sottogruppo     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pmt-umi-def-999.
       pmt-umi-def-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Ultimo autocodice                *
      *    *-----------------------------------------------------------*
       pmt-ult-cod-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo funzionamento   *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    spaces
                     go to pmt-ult-cod-200
           else      go to pmt-ult-cod-400.
       pmt-ult-cod-200.
      *              *-------------------------------------------------*
      *              * Se in impostazione campi chiave                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita senza alcun prompt                   *
      *                  *---------------------------------------------*
           go to     pmt-ult-cod-999.
       pmt-ult-cod-400.
      *              *-------------------------------------------------*
      *              * Se non in impostazione campi chiave             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo elemento    *
      *                  *---------------------------------------------*
           if        w-tes-tip-ele        =    01
                     go to pmt-ult-cod-500
           else if   w-tes-tip-ele        =    02
                     go to pmt-ult-cod-600
           else if   w-tes-tip-ele        =    03
                     go to pmt-ult-cod-700
           else      go to pmt-ult-cod-999.
       pmt-ult-cod-500.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento : classe                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompts per classe                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      48                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Ultimo codice automatico per i gruppi          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      48                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "         appartenenti a questa classe           "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pmt-ult-cod-999.
       pmt-ult-cod-600.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento : gruppo                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompts per gruppo                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      48                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Ultimo codice automatico per i sottogruppi     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      48                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "              appartenenti a questo gruppo      "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pmt-ult-cod-999.
       pmt-ult-cod-700.
      *                  *---------------------------------------------*
      *                  * Se tipo elemento : sottogruppo              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Uscita senza alcun prompt               *
      *                      *-----------------------------------------*
           go to     pmt-ult-cod-999.
       pmt-ult-cod-999.
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
      *    * Accettazione campo testata : Descrizione                  *
      *    *-----------------------------------------------------------*
       acc-des-cgs-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-des-cgs (1)    to   w-sav-des-cgs          .
       acc-des-cgs-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-des-cgs (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-des-cgs-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-des-cgs-999.
       acc-des-cgs-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-des-cgs (1)      .
       acc-des-cgs-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione, a meno *
      *                  * che non sia su tasto Up                     *
      *                  *---------------------------------------------*
           if        w-tes-des-cgs (1)    not  = spaces
                     go to acc-des-cgs-450.
           if        v-key                =    "UP  "
                     go to acc-des-cgs-600
           else      go to acc-des-cgs-100.
       acc-des-cgs-450.
      *                  *---------------------------------------------*
      *                  * Se valore a non spaces il primo carattere   *
      *                  * non deve essere a spaces                    *
      *                  *---------------------------------------------*
           if        w-tes-des-cgs (1)
                    (01 : 01)             =    spaces
                     go to acc-des-cgs-100.
       acc-des-cgs-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione descrizione in uppercase       *
      *                  *---------------------------------------------*
           move      w-tes-des-cgs (1)    to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-tes-des-key (1)      .
       acc-des-cgs-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-des-cgs-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-des-cgs-100.
       acc-des-cgs-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione               *
      *    *-----------------------------------------------------------*
       vis-des-cgs-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-des-cgs (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-cgs-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Mnemonico                    *
      *    *-----------------------------------------------------------*
       acc-mne-cgs-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-mne-cgs (1)    to   w-sav-mne-cgs          .
       acc-mne-cgs-100.
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
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-mne-cgs (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-mne-cgs-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-mne-cgs-999.
       acc-mne-cgs-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-mne-cgs (1)      .
       acc-mne-cgs-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      w-tes-mne-cgs (1)    to   w-all-str-alf          .
           move      05                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-mne-cgs-100.
       acc-mne-cgs-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-mne-cgs-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-mne-cgs-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-mne-cgs-100.
       acc-mne-cgs-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Mnemonico                 *
      *    *-----------------------------------------------------------*
       vis-mne-cgs-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-mne-cgs (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-mne-cgs-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Numero di sequenza           *
      *    *-----------------------------------------------------------*
       acc-sqz-num-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente e dati asso-  *
      *                  * ciati                                       *
      *                  *---------------------------------------------*
           move      w-tes-sqz-num (1)    to   w-sav-sqz-num          .
      *                  *---------------------------------------------*
      *                  * Preparazione valore default                 *
      *                  *---------------------------------------------*
           if        w-tes-sqz-num (1)    not  = zero
                     go to acc-sqz-num-100.
           if        w-tes-tip-ele        =    01
                     move  w-tes-cod-cla  to   w-tes-sqz-num (1)
           else if   w-tes-tip-ele        =    02
                     move  w-tes-cod-gru  to   w-tes-sqz-num (1)
           else if   w-tes-tip-ele        =    03
                     move  w-tes-cod-sgr  to   w-tes-sqz-num (1)      .
           multiply  100                  by   w-tes-sqz-num (1)      .
       acc-sqz-num-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-sqz-num (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-sqz-num-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-sqz-num-999.
       acc-sqz-num-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-sqz-num (1)      .
       acc-sqz-num-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore zero non ammesso, a meno che non si  *
      *                  * sia su tasto Up                             *
      *                  *---------------------------------------------*
           if        w-tes-sqz-num (1)    not  = zero
                     go to acc-sqz-num-450.
           if        v-key                =    "UP  "
                     go to acc-sqz-num-600
           else      go to acc-sqz-num-100.
       acc-sqz-num-450.
      *                  *---------------------------------------------*
      *                  * Controllo che il valore impostato sia uni-  *
      *                  * co in archivio                              *
      *                  *---------------------------------------------*
           move      w-tes-tip-ele        to   w-ctl-uni-nrs-ele      .
           move      w-tes-cod-cla        to   w-ctl-uni-nrs-cla      .
           move      w-tes-cod-gru        to   w-ctl-uni-nrs-gru      .
           move      w-tes-cod-sgr        to   w-ctl-uni-nrs-sgr      .
           move      w-tes-sqz-num (1)    to   w-ctl-uni-nrs-nrs      .
           perform   ctl-uni-nrs-000      thru ctl-uni-nrs-999        .
           if        w-ctl-uni-nrs-flg    =    spaces
                     go to acc-sqz-num-600.
      *                  *---------------------------------------------*
      *                  * Se controllo non superato                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Numero di sequenza gia' utilizzato !"
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-sqz-num-100.
       acc-sqz-num-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sqz-num-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-sqz-num-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-sqz-num-100.
       acc-sqz-num-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Numero di sequenza                *
      *    *-----------------------------------------------------------*
       vis-sqz-num-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-sqz-num (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sqz-num-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Ulteriore suddivisione       *
      *    *-----------------------------------------------------------*
       acc-ult-sud-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-ult-sud-025.
      *                  *---------------------------------------------*
      *                  * Test se accettazione da eseguire            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il tipo elemento non e' una classe o *
      *                      * un gruppo : no accettazione             *
      *                      *-----------------------------------------*
           if        w-tes-tip-ele        not  = 01 and
                     w-tes-tip-ele        not  = 02
                     go to acc-ult-sud-999.
       acc-ult-sud-050.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-ult-sud (1)    to   w-sav-ult-sud          .
       acc-ult-sud-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ult-sud-lun    to   v-car                  .
           move      w-exp-ult-sud-num    to   v-ldt                  .
           move      "NS#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      w-exp-ult-sud-tbl    to   v-txt                  .
           move      15                   to   v-lin                  .
           move      50                   to   v-pos                  .
           move      w-tes-ult-sud (1)    to   v-num                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-ult-sud-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-ult-sud-999.
       acc-ult-sud-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-ult-sud (1)      .
       acc-ult-sud-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che il valore non manchi, a meno che   *
      *                  * non si sia in tasto Up                      *
      *                  *---------------------------------------------*
           if        w-tes-ult-sud (1)    not  = zero
                     go to acc-ult-sud-425.
           if        v-key                not  = "UP  "
                     go to acc-ult-sud-100.
       acc-ult-sud-425.
      *                  *---------------------------------------------*
      *                  * Test che il valore sia accettabile          *
      *                  *---------------------------------------------*
           if        w-tes-ult-sud (1)    >    w-exp-ult-sud-num
                     go to acc-ult-sud-100.
       acc-ult-sud-450.
      *                  *---------------------------------------------*
      *                  * Test che, se il valore indica No suddivi-   *
      *                  * sione, non esistano attualmente elementi di *
      *                  * suddivisione per l'elemento                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se valore a zero : nessun test          *
      *                      *-----------------------------------------*
           if        w-tes-ult-sud (1)    =    zero
                     go to acc-ult-sud-500.
      *                      *-----------------------------------------*
      *                      * Se valore a 'Si suddivisione' : nessun  *
      *                      * test                                    *
      *                      *-----------------------------------------*
           if        w-tes-ult-sud (1)    =    02
                     go to acc-ult-sud-500.
      *                      *-----------------------------------------*
      *                      * Se in Inserimento : nessun test         *
      *                      *-----------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "I"
                     go to acc-ult-sud-500.
      *                      *-----------------------------------------*
      *                      * Se il valore non e' cambiato rispetto   *
      *                      * al valore precedente : nessun test      *
      *                      *-----------------------------------------*
           if        w-tes-ult-sud (1)    =    w-sav-ult-sud
                     go to acc-ult-sud-500.
      *                      *-----------------------------------------*
      *                      * Esecuzione test                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del tipo e-  *
      *                          * lemento                             *
      *                          *-------------------------------------*
           if        w-tes-tip-ele        =    01
                     go to acc-ult-sud-460
           else if   w-tes-tip-ele        =    02
                     go to acc-ult-sud-470
           else      go to acc-ult-sud-480.
       acc-ult-sud-460.
      *                          *-------------------------------------*
      *                          * Se tipo elemento : classe           *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Controllo se ci sono gruppi ap- *
      *                              * partenenti alla classe          *
      *                              *---------------------------------*
           move      w-tes-cod-cla        to   w-det-gru-cla-cla      .
           perform   det-gru-cla-000      thru det-gru-cla-999        .
      *                              *---------------------------------*
      *                              * Deviazione secondo l'esito      *
      *                              *---------------------------------*
           if        w-det-gru-cla-snx    =    "S"
                     go to acc-ult-sud-462
           else      go to acc-ult-sud-464.
       acc-ult-sud-462.
      *                              *---------------------------------*
      *                              * Se ci sono gruppi appartenenti  *
      *                              * alla classe                     *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Messaggio di errore         *
      *                                  *-----------------------------*
           move      "Esistono ancora dei gruppi appartenenti alla class
      -              "e     "             to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                                  *-----------------------------*
      *                                  * A reimpostazione            *
      *                                  *-----------------------------*
           go to     acc-ult-sud-100.
       acc-ult-sud-464.
      *                              *---------------------------------*
      *                              * Se non ci sono gruppi apparte-  *
      *                              * nenti alla classe               *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Continuazione               *
      *                                  *-----------------------------*
           go to     acc-ult-sud-500.
       acc-ult-sud-470.
      *                          *-------------------------------------*
      *                          * Se tipo elemento : gruppo           *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Controllo se ci sono sottogrup- *
      *                              * pi appartenenti al gruppo       *
      *                              *---------------------------------*
           move      w-tes-cod-cla        to   w-det-sgr-gru-cla      .
           move      w-tes-cod-gru        to   w-det-sgr-gru-gru      .
           move      zero                 to   w-det-gru-cla-sgr      .
           perform   det-sgr-gru-000      thru det-sgr-gru-999        .
      *                              *---------------------------------*
      *                              * Deviazione secondo l'esito      *
      *                              *---------------------------------*
           if        w-det-sgr-gru-snx    =    "S"
                     go to acc-ult-sud-472
           else      go to acc-ult-sud-474.
       acc-ult-sud-472.
      *                              *---------------------------------*
      *                              * Se ci sono sottogruppi appar-   *
      *                              * tenenti al gruppo               *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Messaggio di errore         *
      *                                  *-----------------------------*
           move      "Esistono ancora dei sottogruppi appartenenti al gr
      -              "uppo  "             to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                                  *-----------------------------*
      *                                  * A reimpostazione            *
      *                                  *-----------------------------*
           go to     acc-ult-sud-100.
       acc-ult-sud-474.
      *                              *---------------------------------*
      *                              * Se non ci sono sottogruppi ap-  *
      *                              * partenenti al gruppo            *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Continuazione               *
      *                                  *-----------------------------*
           go to     acc-ult-sud-500.
       acc-ult-sud-480.
      *                          *-------------------------------------*
      *                          * Se tipo elemento : ne' classe ne'   *
      *                          * gruppo                              *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Continuazione                   *
      *                              *---------------------------------*
           go to     acc-ult-sud-500.
       acc-ult-sud-500.
      *                  *---------------------------------------------*
      *                  * Continuazione                               *
      *                  *---------------------------------------------*
           go to     acc-ult-sud-600.
       acc-ult-sud-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-ult-sud-625.
      *                  *---------------------------------------------*
      *                  * Se ulteriore suddivisione pari a No, norma- *
      *                  * lizzazione del campo Ultimo autocodice      *
      *                  *---------------------------------------------*
           if        w-tes-ult-sud (1)    =    02
                     go to acc-ult-sud-650.
           if        w-tes-ult-cod (1)    =    zero
                     go to acc-ult-sud-650.
           move      zero                 to   w-tes-ult-cod (1)      .
           perform   vis-ult-cod-000      thru vis-ult-cod-999        .
       acc-ult-sud-650.
      *                  *---------------------------------------------*
      *                  * Continuazione                               *
      *                  *---------------------------------------------*
           go to     acc-ult-sud-800.
       acc-ult-sud-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-ult-sud-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-ult-sud-100.
       acc-ult-sud-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Ulteriore suddivisione    *
      *    *-----------------------------------------------------------*
       vis-ult-sud-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo elemento        *
      *              *-------------------------------------------------*
           if        w-tes-tip-ele        =    01 or
                     w-tes-tip-ele        =    02
                     go to vis-ult-sud-200
           else      go to vis-ult-sud-400.
       vis-ult-sud-200.
      *              *-------------------------------------------------*
      *              * Se tipo elemento classe o gruppo                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ult-sud-lun    to   v-car                  .
           move      w-exp-ult-sud-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-ult-sud-tbl    to   v-txt                  .
           move      15                   to   v-lin                  .
           move      50                   to   v-pos                  .
           move      w-tes-ult-sud (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-ult-sud-999.
       vis-ult-sud-400.
      *              *-------------------------------------------------*
      *              * Se tipo elemento ne' classe ne' gruppo          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita senza alcuna visualizzazione         *
      *                  *---------------------------------------------*
           go to     vis-ult-sud-999.
       vis-ult-sud-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Unita' di misura di default  *
      *    *-----------------------------------------------------------*
       acc-umi-def-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente e dati asso-  *
      *                  * ciati                                       *
      *                  *---------------------------------------------*
           move      w-tes-umi-def (1)    to   w-sav-umi-def          .
           move      w-tes-umi-def-des (1)
                                          to   w-sav-umi-def-des      .
       acc-umi-def-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-umi-ope      .
           move      w-tes-umi-def (1)    to   w-cod-cod-umi-cod      .
           move      17                   to   w-cod-cod-umi-lin      .
           move      50                   to   w-cod-cod-umi-pos      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-cod-zum-cll-000  thru cod-cod-zum-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-zum-foi-000  thru cod-cod-zum-foi-999    .
       acc-umi-def-110.
           perform   cod-cod-zum-cll-000  thru cod-cod-zum-cll-999    .
           if        w-cod-cod-umi-ope    =    "F+"
                     go to acc-umi-def-115.
           if        w-cod-cod-umi-ope    =    "AC"
                     go to acc-umi-def-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-umi-def-115.
           perform   cod-cod-zum-foi-000  thru cod-cod-zum-foi-999    .
           go to     acc-umi-def-110.
       acc-umi-def-120.
           move      w-cod-cod-umi-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-umi-def-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-umi-def-999.
       acc-umi-def-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-umi-def (1)      .
       acc-umi-def-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella unita' di misura            *
      *                  *---------------------------------------------*
           move      w-tes-umi-def (1)    to   w-let-arc-zum-cod      .
           perform   let-arc-zum-000      thru let-arc-zum-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione dati associati al codice     *
      *                  * unita' di misura                            *
      *                  *---------------------------------------------*
           move      w-let-arc-zum-des    to   w-tes-umi-def-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione unita' di mi-   *
      *                  * sura                                        *
      *                  *---------------------------------------------*
           perform   vis-umi-des-000      thru vis-umi-des-999        .
      *                  *---------------------------------------------*
      *                  * Se unita' di misura non esistente : reimpo- *
      *                  * stazione                                    *
      *                  *---------------------------------------------*
           if        w-let-arc-zum-flg    not  = spaces
                     go to acc-umi-def-100.
       acc-umi-def-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-umi-def-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-umi-def-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-umi-def-100.
       acc-umi-def-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Unita' di misura di default       *
      *    *-----------------------------------------------------------*
       vis-umi-def-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      50                   to   v-pos                  .
           move      w-tes-umi-def (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-umi-def-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Descrizione unita' di misura di   *
      *    *                         default                           *
      *    *-----------------------------------------------------------*
       vis-umi-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      55                   to   v-pos                  .
           move      w-tes-umi-def-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-umi-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Ultimo autocodice            *
      *    *-----------------------------------------------------------*
       acc-ult-cod-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-tip-ele        =    03
                     go to acc-ult-cod-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente e dati asso-  *
      *                  * ciati                                       *
      *                  *---------------------------------------------*
           move      w-tes-ult-cod (1)    to   w-sav-ult-cod          .
       acc-ult-cod-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      20                   to   v-lin                  .
           move      50                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-ult-cod (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-ult-cod-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-ult-cod-999.
       acc-ult-cod-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-ult-cod (1)      .
       acc-ult-cod-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-ult-cod-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-ult-cod-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-ult-cod-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-ult-cod-100.
       acc-ult-cod-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Ultimo autocodice                 *
      *    *-----------------------------------------------------------*
       vis-ult-cod-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      20                   to   v-lin                  .
           move      50                   to   v-pos                  .
           move      w-tes-ult-cod (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ult-cod-999.
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
      *              * Test sul tipo elemento                          *
      *              *-------------------------------------------------*
           if        w-tes-tip-ele        not  = zero
                     go to cnt-tdo-key-125.
           move      "Manca il tipo elemento                            
      -              "      "             to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           move      "#"                  to   w-cnt-tdo-key-flg      .
           go to     cnt-tdo-key-999.
       cnt-tdo-key-125.
           if        w-tes-tip-ele        =    01
                     go to cnt-tdo-key-200
           else if   w-tes-tip-ele        =    02
                     go to cnt-tdo-key-400
           else if   w-tes-tip-ele        =    03
                     go to cnt-tdo-key-600.
           move      "Tipo elemento errato                              
      -              "      "             to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           move      "#"                  to   w-cnt-tdo-key-flg      .
           go to     cnt-tdo-key-999.
       cnt-tdo-key-200.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : classe                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su segnale per PF1 attivo              *
      *                  *---------------------------------------------*
           if        w-det-cla-pf1-snx    = "S"
                     go to cnt-tdo-key-999.
      *                  *---------------------------------------------*
      *                  * Test su codice classe                       *
      *                  *---------------------------------------------*
           if        w-tes-cod-cla        not  = zero
                     go to cnt-tdo-key-999.
           move      "Manca il codice classe                            
      -              "      "             to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           move      "#"                  to   w-cnt-tdo-key-flg      .
           go to     cnt-tdo-key-999.
       cnt-tdo-key-400.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : gruppo                       *
      *              *-------------------------------------------------*
           if        w-tes-cod-cla        not  = zero
                     go to cnt-tdo-key-200.
           move      "Manca il codice gruppo                            
      -              "      "             to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           move      "#"                  to   w-cnt-tdo-key-flg      .
           go to     cnt-tdo-key-999.
       cnt-tdo-key-600.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : sottogruppo                  *
      *              *-------------------------------------------------*
           if        w-tes-cod-cla        not  = zero
                     go to cnt-tdo-key-400.
           move      "Manca il codice sottogruppo                       
      -              "      "             to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           move      "#"                  to   w-cnt-tdo-key-flg      .
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
      *              *-------------------------------------------------*
      *              * Test                                            *
      *              *-------------------------------------------------*
           if        w-tes-tip-ele        =    zero and
                     w-tes-cod-cla        =    zero and
                     w-tes-cod-gru        =    zero and
                     w-tes-cod-sgr        =    zero
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
       cnt-tdo-nok-025.
      *              *-------------------------------------------------*
      *              * Controlli                                       *
      *              *-------------------------------------------------*
       cnt-tdo-nok-050.
      *                  *---------------------------------------------*
      *                  * Controllo su Descrizione                    *
      *                  *---------------------------------------------*
           if        w-tes-des-cgs (1)    not  = spaces
                     go to cnt-tdo-nok-100.
           move      "Manca la descrizione                              
      -              "      "             to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-100.
      *                  *---------------------------------------------*
      *                  * Controllo su Ulteriore suddivisione         *
      *                  *---------------------------------------------*
           if        w-tes-tip-ele        not  = 01 and
                     w-tes-tip-ele        not  = 02
                     go to cnt-tdo-nok-150.
           if        w-tes-ult-sud (1)    not  = zero
                     go to cnt-tdo-nok-105.
           move      "Manca la specifica di suddivisione                
      -              "      "             to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-105.
           if        w-tes-ult-sud (1)    =    01 or
                     w-tes-ult-sud (1)    =    02
                     go to cnt-tdo-nok-150.
           move      "Specifica di suddivisione errata                  
      -              "      "             to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-150.
      *                  *---------------------------------------------*
      *                  * Controllo che esista il numero sequenza     *
      *                  *---------------------------------------------*
           if        w-tes-sqz-num (1)    not  = zero
                     go to cnt-tdo-nok-175.
           move      "Manca il numero di sequenza !"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-175.
      *                  *---------------------------------------------*
      *                  * Controllo che il numero sequenza sia uni-   *
      *                  * co in archivio                              *
      *                  *---------------------------------------------*
           move      w-tes-tip-ele        to   w-ctl-uni-nrs-ele      .
           move      w-tes-cod-cla        to   w-ctl-uni-nrs-cla      .
           move      w-tes-cod-gru        to   w-ctl-uni-nrs-gru      .
           move      w-tes-cod-sgr        to   w-ctl-uni-nrs-sgr      .
           move      w-tes-sqz-num (1)    to   w-ctl-uni-nrs-nrs      .
           perform   ctl-uni-nrs-000      thru ctl-uni-nrs-999        .
           if        w-ctl-uni-nrs-flg    =    spaces
                     go to cnt-tdo-nok-200.
           move      "Numero di sequenza gia' utilizzato !"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-200.
      *                  *---------------------------------------------*
      *                  * Fine controlli                              *
      *                  *---------------------------------------------*
           go to     cnt-tdo-nok-600.
       cnt-tdo-nok-600.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
       cnt-tdo-nok-610.
      *                  *---------------------------------------------*
      *                  * Ulteriore suddivisione                      *
      *                  *---------------------------------------------*
           if        w-tes-tip-ele        not  = 01 and
                     w-tes-tip-ele        not  = 02
                     move  zero           to   w-tes-ult-sud (1)      .
       cnt-tdo-nok-620.
      *                  *---------------------------------------------*
      *                  * Ultimo autocodice                           *
      *                  *---------------------------------------------*
           if        w-tes-tip-ele        not  = 01 and
                     w-tes-tip-ele        not  = 02
                     go to cnt-tdo-nok-622.
           if        w-tes-ult-sud (1)    not  = 02
                     go to cnt-tdo-nok-622.
           go to     cnt-tdo-nok-630.
       cnt-tdo-nok-622.
           move      zero                 to   w-tes-ult-cod (1)      .
       cnt-tdo-nok-630.
      *                  *---------------------------------------------*
      *                  * Fine normalizzazioni                        *
      *                  *---------------------------------------------*
           go to     cnt-tdo-nok-800.
       cnt-tdo-nok-800.
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
           move      zero                 to   w-tes-tip-ele          .
           move      zero                 to   w-tes-cod-cla          .
           move      spaces               to   w-tes-cod-cla-mne      .
           move      spaces               to   w-tes-cod-cla-des      .
           move      zero                 to   w-tes-cod-cla-sud      .
           move      spaces               to   w-tes-cod-cla-umi      .
           move      spaces               to   w-tes-cod-cla-aut      .
           move      zero                 to   w-tes-cod-gru          .
           move      spaces               to   w-tes-cod-gru-mne      .
           move      spaces               to   w-tes-cod-gru-des      .
           move      zero                 to   w-tes-cod-gru-sud      .
           move      spaces               to   w-tes-cod-gru-umi      .
           move      spaces               to   w-tes-cod-gru-aut      .
           move      zero                 to   w-tes-cod-sgr          .
           move      spaces               to   w-tes-cod-sgr-mne      .
           move      spaces               to   w-tes-cod-sgr-des      .
           move      zero                 to   w-tes-cod-sgr-sud      .
           move      spaces               to   w-tes-cod-sgr-umi      .
           move      spaces               to   w-tes-cod-sgr-aut      .
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *-----------------------------------------------------------*
       nor-nok-tes-000.
           move      spaces               to   w-tes-mne-cgs (1)      .
           move      spaces               to   w-tes-des-key (1)      .
           move      spaces               to   w-tes-des-cgs (1)      .
           move      zero                 to   w-tes-sqz-num (1)      .
           move      zero                 to   w-tes-ult-sud (1)      .
           move      spaces               to   w-tes-umi-def (1)      .
           move      spaces               to   w-tes-umi-def-des (1)  .
           move      zero                 to   w-tes-ult-cod (1)      .
           move      spaces               to   w-tes-alx-exp (1)      .
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
      *              * Deviazione in funzione del tipo elemento        *
      *              *-------------------------------------------------*
           if        w-tes-tip-ele        =    01
                     go to rou-let-reg-200
           else if   w-tes-tip-ele        =    02
                     go to rou-let-reg-400
           else if   w-tes-tip-ele        =    03
                     go to rou-let-reg-600.
           move      "#"                  to   w-cnt-rou-let-reg      .
           go to     rou-let-reg-999.
       rou-let-reg-200.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : classe                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura classe                              *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLA    "         to   f-key                  .
           move      w-tes-cod-cla        to   rf-zp1-cod-cla         .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
           if        f-sts                =    e-not-err
                     go to rou-let-reg-225.
      *                  *---------------------------------------------*
      *                  * Se classe non trovata                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se inserimento consentito          *
      *                      *-----------------------------------------*
           if        w-cnt-mfu-vis-sgr    not  = "V"
                     go to rou-let-reg-210.
      *                      *-----------------------------------------*
      *                      * Flag di uscita                          *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-rou-let-reg      .
      *                      *-----------------------------------------*
      *                      * Messaggio                               *
      *                      *-----------------------------------------*
           move      "Inserimento non consentito !                      
      -              "               "    to   w-err-box-err-msg      .
      *                      *-----------------------------------------*
      *                      * Box di errore                           *
      *                      *-----------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-let-reg-900.
       rou-let-reg-210.
      *                      *-----------------------------------------*
      *                      * Tipo funzionamento : Inserimento        *
      *                      *-----------------------------------------*
           move      "I"                  to   w-cnt-mfu-tip-fun      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     rou-let-reg-999.
       rou-let-reg-225.
      *                  *---------------------------------------------*
      *                  * Se classe trovata                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo funzionamento : Modifica           *
      *                      *-----------------------------------------*
           move      "M"                  to   w-cnt-mfu-tip-fun      .
      *                      *-----------------------------------------*
      *                      * Determinazione valori attuali           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Valori diretti                      *
      *                          *-------------------------------------*
           move      rf-zp1-des-cla       to   w-tes-des-cgs (1)      .
           move      rf-zp1-des-key       to   w-tes-des-key (1)      .
           move      rf-zp1-mne-cla       to   w-tes-mne-cgs (1)      .
           move      rf-zp1-sqz-num       to   w-tes-sqz-num (1)      .
           move      rf-zp1-ult-sud       to   w-tes-ult-sud (1)      .
           move      rf-zp1-umi-def       to   w-tes-umi-def (1)      .
           move      rf-zp1-ult-cod       to   w-tes-ult-cod (1)      .
           move      rf-zp1-alx-exp       to   w-tes-alx-exp (1)      .
      *                          *-------------------------------------*
      *                          * Valori indiretti                    *
      *                          *-------------------------------------*
           move      w-tes-umi-def (1)    to   w-let-arc-zum-cod      .
           perform   let-arc-zum-000      thru let-arc-zum-999        .
           move      w-let-arc-zum-des    to   w-tes-umi-def-des (1)  .
      *                      *-----------------------------------------*
      *                      * Valori precedenti anagrafica            *
      *                      *-----------------------------------------*
           move      w-tes-val-aep (1)    to   w-tes-val-aep (2)      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-let-reg-800.
       rou-let-reg-400.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : gruppo                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura gruppo                              *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODGRU    "         to   f-key                  .
           move      w-tes-cod-cla        to   rf-zp2-cod-cla         .
           move      w-tes-cod-gru        to   rf-zp2-cod-gru         .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
           if        f-sts                =    e-not-err
                     go to rou-let-reg-425.
      *                  *---------------------------------------------*
      *                  * Se gruppo non trovato                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se inserimento consentito          *
      *                      *-----------------------------------------*
           if        w-cnt-mfu-vis-sgr    not  = "V"
                     go to rou-let-reg-410.
      *                      *-----------------------------------------*
      *                      * Flag di uscita                          *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-rou-let-reg      .
      *                      *-----------------------------------------*
      *                      * Messaggio                               *
      *                      *-----------------------------------------*
           move      "Inserimento non consentito !                      
      -              "               "    to   w-err-box-err-msg      .
      *                      *-----------------------------------------*
      *                      * Box di errore                           *
      *                      *-----------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-let-reg-900.
       rou-let-reg-410.
      *                      *-----------------------------------------*
      *                      * Tipo funzionamento : Inserimento        *
      *                      *-----------------------------------------*
           move      "I"                  to   w-cnt-mfu-tip-fun      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     rou-let-reg-999.
       rou-let-reg-425.
      *                  *---------------------------------------------*
      *                  * Se gruppo trovato                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo funzionamento : Modifica           *
      *                      *-----------------------------------------*
           move      "M"                  to   w-cnt-mfu-tip-fun      .
      *                      *-----------------------------------------*
      *                      * Determinazione valori attuali           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Valori diretti                      *
      *                          *-------------------------------------*
           move      rf-zp2-des-gru       to   w-tes-des-cgs (1)      .
           move      rf-zp2-des-key       to   w-tes-des-key (1)      .
           move      rf-zp2-mne-gru       to   w-tes-mne-cgs (1)      .
           move      rf-zp2-sqz-num       to   w-tes-sqz-num (1)      .
           move      rf-zp2-ult-sud       to   w-tes-ult-sud (1)      .
           move      rf-zp2-umi-def       to   w-tes-umi-def (1)      .
           move      rf-zp2-ult-cod       to   w-tes-ult-cod (1)      .
           move      rf-zp2-alx-exp       to   w-tes-alx-exp (1)      .
      *                          *-------------------------------------*
      *                          * Valori indiretti                    *
      *                          *-------------------------------------*
           move      w-tes-umi-def (1)    to   w-let-arc-zum-cod      .
           perform   let-arc-zum-000      thru let-arc-zum-999        .
           move      w-let-arc-zum-des    to   w-tes-umi-def-des (1)  .
      *                      *-----------------------------------------*
      *                      * Valori precedenti anagrafica            *
      *                      *-----------------------------------------*
           move      w-tes-val-aep (1)    to   w-tes-val-aep (2)      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-let-reg-800.
       rou-let-reg-600.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : sottogruppo                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura sottogruppo                         *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSGR    "         to   f-key                  .
           move      w-tes-cod-cla        to   rf-zp3-cod-cla         .
           move      w-tes-cod-gru        to   rf-zp3-cod-gru         .
           move      w-tes-cod-sgr        to   rf-zp3-cod-sgr         .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
           if        f-sts                =    e-not-err
                     go to rou-let-reg-625.
      *                  *---------------------------------------------*
      *                  * Se sottogruppo non trovato                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se inserimento consentito          *
      *                      *-----------------------------------------*
           if        w-cnt-mfu-vis-sgr    not  = "V"
                     go to rou-let-reg-610.
      *                      *-----------------------------------------*
      *                      * Flag di uscita                          *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-rou-let-reg      .
      *                      *-----------------------------------------*
      *                      * Messaggio                               *
      *                      *-----------------------------------------*
           move      "Inserimento non consentito !                      
      -              "               "    to   w-err-box-err-msg      .
      *                      *-----------------------------------------*
      *                      * Box di errore                           *
      *                      *-----------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-let-reg-900.
       rou-let-reg-610.
      *                      *-----------------------------------------*
      *                      * Tipo funzionamento : Inserimento        *
      *                      *-----------------------------------------*
           move      "I"                  to   w-cnt-mfu-tip-fun      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     rou-let-reg-999.
       rou-let-reg-625.
      *                  *---------------------------------------------*
      *                  * Se sottogruppo trovato                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo funzionamento : Modifica           *
      *                      *-----------------------------------------*
           move      "M"                  to   w-cnt-mfu-tip-fun      .
      *                      *-----------------------------------------*
      *                      * Determinazione valori attuali           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Valori diretti                      *
      *                          *-------------------------------------*
           move      rf-zp3-des-sgr       to   w-tes-des-cgs (1)      .
           move      rf-zp3-des-key       to   w-tes-des-key (1)      .
           move      rf-zp3-mne-sgr       to   w-tes-mne-cgs (1)      .
           move      rf-zp3-sqz-num       to   w-tes-sqz-num (1)      .
           move      rf-zp3-ult-sud       to   w-tes-ult-sud (1)      .
           move      rf-zp3-umi-def       to   w-tes-umi-def (1)      .
           move      rf-zp3-ult-cod       to   w-tes-ult-cod (1)      .
           move      rf-zp3-alx-exp       to   w-tes-alx-exp (1)      .
      *                          *-------------------------------------*
      *                          * Valori indiretti                    *
      *                          *-------------------------------------*
           move      w-tes-umi-def (1)    to   w-let-arc-zum-cod      .
           perform   let-arc-zum-000      thru let-arc-zum-999        .
           move      w-let-arc-zum-des    to   w-tes-umi-def-des (1)  .
      *                      *-----------------------------------------*
      *                      * Valori precedenti anagrafica            *
      *                      *-----------------------------------------*
           move      w-tes-val-aep (1)    to   w-tes-val-aep (2)      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-let-reg-800.
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
      *              *-------------------------------------------------*
      *              * Rivisualizzazione prompts per pagina uno        *
      *              *-------------------------------------------------*
           perform   pmt-tes-uno-000      thru pmt-tes-uno-999        .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo elemento        *
      *              *-------------------------------------------------*
           if        w-tes-tip-ele        =    01
                     go to pre-acc-ins-200
           else if   w-tes-tip-ele        =    02
                     go to pre-acc-ins-400
           else if   w-tes-tip-ele        =    03
                     go to pre-acc-ins-600.
       pre-acc-ins-200.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : classe                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Nessuna azione                              *
      *                  *---------------------------------------------*
           go to     pre-acc-ins-999.
       pre-acc-ins-400.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : gruppo                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione e visualizzazione dell'unita'  *
      *                  * di misura da proporre come default prenden- *
      *                  * dola dalla classe di appartenenza           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se a spaces : nessuna azione            *
      *                      *-----------------------------------------*
           if        w-tes-cod-cla-umi    =    spaces
                     go to pre-acc-ins-999.
      *                      *-----------------------------------------*
      *                      * Unita' di misura dalla classe           *
      *                      *-----------------------------------------*
           move      w-tes-cod-cla-umi    to   w-tes-umi-def (1)      .
      *                      *-----------------------------------------*
      *                      * Descrizione unita' di misura da lettu-  *
      *                      * ra tabella unita' di misura             *
      *                      *-----------------------------------------*
           move      w-tes-umi-def (1)    to   w-let-arc-zum-cod      .
           perform   let-arc-zum-000      thru let-arc-zum-999        .
           move      w-let-arc-zum-des    to   w-tes-umi-def-des (1)  .
      *                      *-----------------------------------------*
      *                      * Visualizzazione unita' di misura : co-  *
      *                      * dice e descrizione                      *
      *                      *-----------------------------------------*
           perform   vis-umi-def-000      thru vis-umi-def-999        .
           perform   vis-umi-des-000      thru vis-umi-des-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pre-acc-ins-999.
       pre-acc-ins-600.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : sottogruppo                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione e visualizzazione dell'unita'  *
      *                  * di misura da proporre come default prenden- *
      *                  * dola dal gruppo di appartenenza             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se a spaces : nessuna azione            *
      *                      *-----------------------------------------*
           if        w-tes-cod-gru-umi    =    spaces
                     go to pre-acc-ins-999.
      *                      *-----------------------------------------*
      *                      * Unita' di misura dal gruppo             *
      *                      *-----------------------------------------*
           move      w-tes-cod-gru-umi    to   w-tes-umi-def (1)      .
      *                      *-----------------------------------------*
      *                      * Descrizione unita' di misura da lettu-  *
      *                      * ra tabella unita' di misura             *
      *                      *-----------------------------------------*
           move      w-tes-umi-def (1)    to   w-let-arc-zum-cod      .
           perform   let-arc-zum-000      thru let-arc-zum-999        .
           move      w-let-arc-zum-des    to   w-tes-umi-def-des (1)  .
      *                      *-----------------------------------------*
      *                      * Visualizzazione unita' di misura : co-  *
      *                      * dice e descrizione                      *
      *                      *-----------------------------------------*
           perform   vis-umi-def-000      thru vis-umi-def-999        .
           perform   vis-umi-des-000      thru vis-umi-des-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pre-acc-ins-999.
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
      *              * Rivisualizzazione prompts per pagina uno        *
      *              *-------------------------------------------------*
           perform   pmt-tes-uno-000      thru pmt-tes-uno-999        .
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
      *              * Rivisualizzazione prompts per pagina uno        *
      *              *-------------------------------------------------*
           perform   pmt-tes-uno-000      thru pmt-tes-uno-999        .
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
       pre-snx-del-100.
      *              *-------------------------------------------------*
      *              * Determinazione se elemento cancellabile         *
      *              *-------------------------------------------------*
           perform   det-snx-del-000      thru det-snx-del-999        .
      *                  *---------------------------------------------*
      *                  * Test su esito determinazione                *
      *                  *---------------------------------------------*
           if        w-det-snx-del-snx    =    "S"
                     go to pre-snx-del-900.
      *                  *---------------------------------------------*
      *                  * Test su tipo di elemento                    *
      *                  *---------------------------------------------*
           if        w-tes-tip-ele        not  = 01
                     move  "#"            to   w-cnt-pre-snx-del
                     go to pre-snx-del-900.
      *                  *---------------------------------------------*
      *                  * Subroutine di richiesta                     *
      *                  *---------------------------------------------*
           perform   pre-snx-del-box-000  thru pre-snx-del-box-999    .
      *                      *-----------------------------------------*
      *                      * Se rinuncia                             *
      *                      *-----------------------------------------*
           if        w-exp-pre-del-sce    =    01
                     move  "#"            to   w-cnt-pre-snx-del
                     go to pre-snx-del-900.
      *                      *-----------------------------------------*
      *                      * Se cancellazione                        *
      *                      *-----------------------------------------*
           perform   pre-snx-del-dgs-000  thru pre-snx-del-dgs-999    .
       pre-snx-del-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pre-snx-del-999.
       pre-snx-del-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-richiesta di ratifica tasto Delete            *
      *    *                                                           *
      *    * Sub-routine di richiesta conferma cancellazione classe,   *
      *    * gruppi sottogruppi in soluzione unica                     *
      *    *-----------------------------------------------------------*
       pre-snx-del-box-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valore di uscita                *
      *              *-------------------------------------------------*
           move      zero                 to   w-exp-pre-del-sce      .
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
           move      08                   to   v-lin                  .
           move      07                   to   v-pos                  .
           move      17                   to   v-lto                  .
           move      74                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pre-snx-del-box-200.
      *              *-------------------------------------------------*
      *              * Literals nel box                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * 'Attenzione'                                *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      64                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "Attenzione :  La classe da cancellare risulta sudd
      -              "ivisa in :    "     to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Contatore gruppi                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      23                   to   v-pos                  .
           if        w-det-gru-cla-gru    =    1
                     move  "-       gruppo     "
                                          to   v-alf
           else      move  "-       gruppi     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      11                   to   v-lin                  .
           move      25                   to   v-pos                  .
           move      w-det-gru-cla-gru    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Contatore sottogruppi                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se da visualizzare                 *
      *                      *-----------------------------------------*
           if        w-det-gru-cla-sgr    =    zero
                     go to pre-snx-del-box-400.
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      23                   to   v-pos                  .
           if        w-det-gru-cla-sgr    =    1
                     move  "-       sottogruppo"
                                          to   v-alf
           else      move  "-       sottogruppi"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      12                   to   v-lin                  .
           move      25                   to   v-pos                  .
           move      w-det-gru-cla-sgr    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pre-snx-del-box-400.
      *                  *---------------------------------------------*
      *                  * 'Scelta'                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "Scelta     :"       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pre-snx-del-box-600.
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pre-snx-del-box-620.
      *              *-------------------------------------------------*
      *              * Accettazione risposta                           *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-pre-del-lun    to   v-car                  .
           move      w-exp-pre-del-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      14                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-exp-pre-del-tbl    to   v-txt                  .
           move      zero                 to   v-num                  .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-exp-pre-del-sce      .
      *              *-------------------------------------------------*
      *              * Controllo risposta                              *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  01             to   w-exp-pre-del-sce
                     go to pre-snx-del-box-800.
           if        w-exp-pre-del-sce    not  = 01 and
                     w-exp-pre-del-sce    not  = 02
                     go to pre-snx-del-box-620.
       pre-snx-del-box-800.
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pre-snx-del-box-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pre-snx-del-box-999.
       pre-snx-del-box-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-richiesta di ratifica tasto Delete            *
      *    *                                                           *
      *    * Sub-routine di cancellazione gruppi e sottogruppi appar-  *
      *    * tenenti alla stessa classe                                *
      *    *-----------------------------------------------------------*
       pre-snx-del-dgs-000.
      *              *-------------------------------------------------*
      *              * Inizio ciclo di scansione                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-gru-cla-ctr      .
       pre-snx-del-dgs-200.
           add       1                    to   w-det-gru-cla-ctr      .
           if        w-det-gru-cla-ctr    >    w-det-gru-cla-ele
                     go to pre-snx-del-dgs-900.
           if        w-det-gru-cla-ctr    >    w-det-gru-cla-max
                     go to pre-snx-del-dgs-900.
       pre-snx-del-dgs-400.
      *              *-------------------------------------------------*
      *              * Cancellazione elemento                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo elemento da cancellare         *
      *                  *---------------------------------------------*
           if        w-det-gru-cla-bcg
                    (w-det-gru-cla-ctr)   =    zero
                     go to pre-snx-del-dgs-200.
           if        w-det-gru-cla-bcs
                    (w-det-gru-cla-ctr)   =    zero
                     go to pre-snx-del-dgs-500
           else      go to pre-snx-del-dgs-600.
       pre-snx-del-dgs-500.
      *                  *---------------------------------------------*
      *                  * Cancellazione gruppo                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Composizione chiave                     *
      *                      *-----------------------------------------*
           move      w-tes-cod-cla        to   rf-zp2-cod-cla         .
           move      w-det-gru-cla-bcg
                    (w-det-gru-cla-ctr)   to   rf-zp2-cod-gru         .
      *                      *-----------------------------------------*
      *                      * Delete record                           *
      *                      *-----------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
      *                      *-----------------------------------------*
      *                      * A riciclo                               *
      *                      *-----------------------------------------*
           go to     pre-snx-del-dgs-800.
       pre-snx-del-dgs-600.
      *                  *---------------------------------------------*
      *                  * Cancellazione sottogruppo                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Composizione chiave                     *
      *                      *-----------------------------------------*
           move      w-tes-cod-cla        to   rf-zp3-cod-cla         .
           move      w-det-gru-cla-bcg
                    (w-det-gru-cla-ctr)   to   rf-zp3-cod-gru         .
           move      w-det-gru-cla-bcs
                    (w-det-gru-cla-ctr)   to   rf-zp3-cod-sgr         .
      *                      *-----------------------------------------*
      *                      * Delete record                           *
      *                      *-----------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
      *                      *-----------------------------------------*
      *                      * A riciclo                               *
      *                      *-----------------------------------------*
           go to     pre-snx-del-dgs-800.
       pre-snx-del-dgs-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     pre-snx-del-dgs-200.
       pre-snx-del-dgs-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pre-snx-del-dgs-999.
       pre-snx-del-dgs-999.
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
      *              * Deviazione in funzione del tipo elemento        *
      *              *-------------------------------------------------*
           if        w-tes-tip-ele        =    01
                     go to pos-exi-ins-200
           else if   w-tes-tip-ele        =    02
                     go to pos-exi-ins-400
           else if   w-tes-tip-ele        =    03
                     go to pos-exi-ins-600
           else      go to pos-exi-ins-999.
       pos-exi-ins-200.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : classe                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se e' stata eseguita l'attribuzione del co- *
      *                  * dice in automatico, si ripristina, se pos-  *
      *                  * sibile, il codice al valore precedente l'-  *
      *                  * incremento                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se non attribuzione automatica : uscita *
      *                      *-----------------------------------------*
           if        w-tes-cod-cla-aut    =    spaces
                     go to pos-exi-ins-999.
      *                      *-----------------------------------------*
      *                      * Ripristino codice automatico            *
      *                      *-----------------------------------------*
           perform   rip-cla-aut-000      thru rip-cla-aut-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pos-exi-ins-999.
       pos-exi-ins-400.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : gruppo                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se e' stata eseguita l'attribuzione del co- *
      *                  * dice in automatico, si ripristina, se pos-  *
      *                  * sibile, il codice al valore precedente l'-  *
      *                  * incremento                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se non attribuzione automatica : uscita *
      *                      *-----------------------------------------*
           if        w-tes-cod-gru-aut    =    spaces
                     go to pos-exi-ins-999.
      *                      *-----------------------------------------*
      *                      * Ripristino codice automatico            *
      *                      *-----------------------------------------*
           perform   rip-gru-aut-000      thru rip-gru-aut-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pos-exi-ins-999.
       pos-exi-ins-600.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : sottogruppo                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se e' stata eseguita l'attribuzione del co- *
      *                  * dice in automatico, si ripristina, se pos-  *
      *                  * sibile, il codice al valore precedente l'-  *
      *                  * incremento                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se non attribuzione automatica : uscita *
      *                      *-----------------------------------------*
           if        w-tes-cod-gru-aut    =    spaces
                     go to pos-exi-ins-999.
      *                      *-----------------------------------------*
      *                      * Ripristino codice automatico            *
      *                      *-----------------------------------------*
           perform   rip-sgr-aut-000      thru rip-sgr-aut-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     pos-exi-ins-999.
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
      *              * Deviazione in funzione del tipo elemento        *
      *              *-------------------------------------------------*
           if        w-tes-tip-ele        =    01
                     go to scr-mov-fil-200
           else if   w-tes-tip-ele        =    02
                     go to scr-mov-fil-400
           else if   w-tes-tip-ele        =    03
                     go to scr-mov-fil-600
           else      go to scr-mov-fil-999.
       scr-mov-fil-200.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : classe                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-225.
      *                      *-----------------------------------------*
      *                      * Write record [zp1]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-zp1-000      thru wrt-rec-zp1-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     scr-mov-fil-999.
       scr-mov-fil-225.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rewrite record [zp1]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-zp1-000      thru rew-rec-zp1-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     scr-mov-fil-999.
       scr-mov-fil-400.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : gruppo                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-425.
      *                      *-----------------------------------------*
      *                      * Write record [zp2]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-zp2-000      thru wrt-rec-zp2-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     scr-mov-fil-999.
       scr-mov-fil-425.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rewrite record [zp2]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-zp2-000      thru rew-rec-zp2-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     scr-mov-fil-999.
       scr-mov-fil-600.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : sottogruppo                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-625.
      *                      *-----------------------------------------*
      *                      * Write record [zp3]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-zp3-000      thru wrt-rec-zp3-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     scr-mov-fil-999.
       scr-mov-fil-625.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rewrite record [zp3]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-zp3-000      thru rew-rec-zp3-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     scr-mov-fil-999.
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *-----------------------------------------------------------*
       del-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo elemento        *
      *              *-------------------------------------------------*
           if        w-tes-tip-ele        =    01
                     go to del-mov-fil-200
           else if   w-tes-tip-ele        =    02
                     go to del-mov-fil-400
           else if   w-tes-tip-ele        =    03
                     go to del-mov-fil-600
           else      go to del-mov-fil-999.
       del-mov-fil-200.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : classe                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Delete record [zp1]                         *
      *                  *---------------------------------------------*
           perform   del-rec-zp1-000      thru del-rec-zp1-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     del-mov-fil-999.
       del-mov-fil-400.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : gruppo                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Delete record [zp2]                         *
      *                  *---------------------------------------------*
           perform   del-rec-zp2-000      thru del-rec-zp2-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     del-mov-fil-999.
       del-mov-fil-600.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : sottogruppo                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Delete record [zp3]                         *
      *                  *---------------------------------------------*
           perform   del-rec-zp3-000      thru del-rec-zp3-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     del-mov-fil-999.
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [zp1]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-zp1-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-tes-cod-cla        to   rf-zp1-cod-cla         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      w-tes-des-cgs (1)    to   rf-zp1-des-cla         .
           move      w-tes-mne-cgs (1)    to   rf-zp1-mne-cla         .
           move      w-tes-des-key (1)    to   rf-zp1-des-key         .
           move      w-tes-sqz-num (1)    to   rf-zp1-sqz-num         .
           move      w-tes-ult-sud (1)    to   rf-zp1-ult-sud         .
           move      w-tes-umi-def (1)    to   rf-zp1-umi-def         .
           move      w-tes-ult-cod (1)    to   rf-zp1-ult-cod         .
           move      w-tes-alx-exp (1)    to   rf-zp1-alx-exp         .
       cmp-rec-zp1-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [zp1]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-zp1-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-zp1-000      thru cmp-rec-zp1-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
       wrt-rec-zp1-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [zp1]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-zp1-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-zp1-000      thru cmp-rec-zp1-999        .
      *              *-------------------------------------------------*
      *              * Forced put record                               *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
       rew-rec-zp1-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [zp1]                                *
      *    *-----------------------------------------------------------*
       del-rec-zp1-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-zp1-000      thru cmp-rec-zp1-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
       del-rec-zp1-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [zp2]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-zp2-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-tes-cod-cla        to   rf-zp2-cod-cla         .
           move      w-tes-cod-gru        to   rf-zp2-cod-gru         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      w-tes-des-cgs (1)    to   rf-zp2-des-gru         .
           move      w-tes-mne-cgs (1)    to   rf-zp2-mne-gru         .
           move      w-tes-des-key (1)    to   rf-zp2-des-key         .
           move      w-tes-sqz-num (1)    to   rf-zp2-sqz-num         .
           move      w-tes-ult-sud (1)    to   rf-zp2-ult-sud         .
           move      w-tes-umi-def (1)    to   rf-zp2-umi-def         .
           move      w-tes-ult-cod (1)    to   rf-zp2-ult-cod         .
           move      w-tes-alx-exp (1)    to   rf-zp2-alx-exp         .
       cmp-rec-zp2-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [zp2]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-zp2-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-zp2-000      thru cmp-rec-zp2-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
       wrt-rec-zp2-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [zp2]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-zp2-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-zp2-000      thru cmp-rec-zp2-999        .
      *              *-------------------------------------------------*
      *              * Forced put record                               *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
       rew-rec-zp2-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [zp2]                                *
      *    *-----------------------------------------------------------*
       del-rec-zp2-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-zp2-000      thru cmp-rec-zp2-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
       del-rec-zp2-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [zp3]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-zp3-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-tes-cod-cla        to   rf-zp3-cod-cla         .
           move      w-tes-cod-gru        to   rf-zp3-cod-gru         .
           move      w-tes-cod-sgr        to   rf-zp3-cod-sgr         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      w-tes-des-cgs (1)    to   rf-zp3-des-sgr         .
           move      w-tes-mne-cgs (1)    to   rf-zp3-mne-sgr         .
           move      w-tes-des-key (1)    to   rf-zp3-des-key         .
           move      w-tes-sqz-num (1)    to   rf-zp3-sqz-num         .
           move      w-tes-ult-sud (1)    to   rf-zp3-ult-sud         .
           move      w-tes-umi-def (1)    to   rf-zp3-umi-def         .
           move      w-tes-ult-cod (1)    to   rf-zp3-ult-cod         .
           move      w-tes-alx-exp (1)    to   rf-zp3-alx-exp         .
       cmp-rec-zp3-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [zp3]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-zp3-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-zp3-000      thru cmp-rec-zp3-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
       wrt-rec-zp3-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [zp3]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-zp3-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-zp3-000      thru cmp-rec-zp3-999        .
      *              *-------------------------------------------------*
      *              * Forced put record                               *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
       rew-rec-zp3-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [zp3]                                *
      *    *-----------------------------------------------------------*
       del-rec-zp3-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-zp3-000      thru cmp-rec-zp3-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
       del-rec-zp3-999.
           exit.

      *    *===========================================================*
      *    * Routine di attribuzione codice classe automatico          *
      *    *-----------------------------------------------------------*
       att-cla-aut-000.
      *              *-------------------------------------------------*
      *              * Lettura codice automatico per [zp1]             *
      *              *-------------------------------------------------*
           move      "Eg"                 to   s-ope                  .
           move      "zp1 "               to   s-nam                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-sts                =    spaces
                     go to att-cla-aut-400.
       att-cla-aut-200.
      *              *-------------------------------------------------*
      *              * Record non esistente                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scrittura record normalizzato               *
      *                  *---------------------------------------------*
           move      "Ep"                 to   s-ope                  .
           move      "zp1 "               to   s-nam                  .
           move      zero                 to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Ripetizione dell'intera operazione          *
      *                  *---------------------------------------------*
           go to     att-cla-aut-000.
       att-cla-aut-400.
      *              *-------------------------------------------------*
      *              * Record esistente                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Memorizzazione del valore pre incremento    *
      *                  *---------------------------------------------*
           move      s-num                to   w-enc-cod-cla-pre      .
      *                  *---------------------------------------------*
      *                  * Incremento del valore                       *
      *                  *---------------------------------------------*
           move      w-enc-cod-cla-pre    to   w-enc-cod-cla-pos      .
           add       1                    to   w-enc-cod-cla-pos      .
       att-cla-aut-500.
      *                  *---------------------------------------------*
      *                  * Se l'incremento porta a zero si forza il    *
      *                  * valore a 1                                  *
      *                  *---------------------------------------------*
           if        w-enc-cod-cla-pos    =    zero
                     move  1              to   w-enc-cod-cla-pos      .
      *                  *---------------------------------------------*
      *                  * Controllo se esiste gia' un record con il   *
      *                  * codice pari al valore incrementato          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLA    "         to   f-key                  .
           move      w-enc-cod-cla-pos    to   rf-zp1-cod-cla         .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
           if        f-sts                =    e-not-err
                     go to att-cla-aut-600
           else      go to att-cla-aut-700.
       att-cla-aut-600.
      *                  *---------------------------------------------*
      *                  * Se esiste gia' un record con il codice pari *
      *                  * al valore incrementato                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ulteriore incremento del valore         *
      *                      *-----------------------------------------*
           add       1                    to   w-enc-cod-cla-pos      .
      *                      *-----------------------------------------*
      *                      * Riciclo a controllo di esistenza        *
      *                      *-----------------------------------------*
           go to     att-cla-aut-500.
       att-cla-aut-700.
      *                  *---------------------------------------------*
      *                  * Se non esiste gia' un record con il codice  *
      *                  * pari al valore incrementato                 *
      *                  *---------------------------------------------*
           move      "Eu"                 to   s-ope                  .
           move      "zp1 "               to   s-nam                  .
           move      w-enc-cod-cla-pos    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Se errori ripete l'intera operazione di *
      *                      * attribuzione                            *
      *                      *-----------------------------------------*
           if        s-sts                not  = spaces
                     go to att-cla-aut-000.
       att-cla-aut-999.
           exit.

      *    *===========================================================*
      *    * Routine di ripristino codice classe automatico            *
      *    *-----------------------------------------------------------*
       rip-cla-aut-000.
      *              *-------------------------------------------------*
      *              * Lettura codice automatico per [zp1]             *
      *              *-------------------------------------------------*
           move      "Eg"                 to   s-ope                  .
           move      "zp1 "               to   s-nam                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-sts                =    spaces
                     go to rip-cla-aut-400.
       rip-cla-aut-200.
      *              *-------------------------------------------------*
      *              * Se record non esistente                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rip-cla-aut-999.
       rip-cla-aut-400.
      *              *-------------------------------------------------*
      *              * Se record esistente                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Confronto tra il valore attuale ed il valo- *
      *                  * re post incremento                          *
      *                  *---------------------------------------------*
           if        s-num                =    w-enc-cod-cla-pos
                     go to rip-cla-aut-600.
       rip-cla-aut-500.
      *                  *---------------------------------------------*
      *                  * Se il valore attuale non e' uguale al valo- *
      *                  * re post incremento                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Unlock                                  *
      *                      *-----------------------------------------*
           move      "Er"                 to   s-ope                  .
           move      "zp1 "               to   s-nam                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     rip-cla-aut-999.
       rip-cla-aut-600.
      *                  *---------------------------------------------*
      *                  * Se il valore attuale e' uguale al valore    *
      *                  * post incremento                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Aggiornamento del codice automatico con *
      *                      * il valore pre incremento                *
      *                      *-----------------------------------------*
           move      "Eu"                 to   s-ope                  .
           move      "zp1 "               to   s-nam                  .
           move      w-enc-cod-cla-pre    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Se errori ripete l'intera operazione di *
      *                      * attribuzione                            *
      *                      *-----------------------------------------*
           if        s-sts                not  = spaces
                     go to rip-cla-aut-000.
       rip-cla-aut-999.
           exit.

      *    *===========================================================*
      *    * Routine di attribuzione codice gruppo automatico          *
      *    *-----------------------------------------------------------*
       att-gru-aut-000.
      *              *-------------------------------------------------*
      *              * Lettura record classe                           *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "CODCLA    "         to   f-key                  .
           move      w-enc-cod-gru-cla    to   rf-zp1-cod-cla         .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
           if        f-sts                =    e-not-err
                     go to att-gru-aut-400.
       att-gru-aut-200.
      *              *-------------------------------------------------*
      *              * Record classe non esistente                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ricerca del codice automatico per scansione *
      *                  * sequenziale                                 *
      *                  *---------------------------------------------*
           move      zero                 to   w-enc-cod-gru-pos      .
       att-gru-aut-250.
           add       1                    to   w-enc-cod-gru-pos      .
           if        w-enc-cod-gru-pos    =    zero
                     move  1              to   w-enc-cod-gru-pos      .
           move      "RK"                 to   f-ope                  .
           move      "CODGRU    "         to   f-key                  .
           move      w-enc-cod-gru-cla    to   rf-zp2-cod-cla         .
           move      w-enc-cod-gru-pos    to   rf-zp2-cod-gru         .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
           if        f-sts                =    e-not-err
                     go to att-gru-aut-250.
           go to     att-gru-aut-999.
       att-gru-aut-400.
      *              *-------------------------------------------------*
      *              * Record classe esistente                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Memorizzazione del valore pre incremento    *
      *                  *---------------------------------------------*
           move      rf-zp1-ult-cod       to   w-enc-cod-gru-pre      .
      *                  *---------------------------------------------*
      *                  * Incremento del valore                       *
      *                  *---------------------------------------------*
           move      w-enc-cod-gru-pre    to   w-enc-cod-gru-pos      .
           add       1                    to   w-enc-cod-gru-pos      .
       att-gru-aut-500.
      *                  *---------------------------------------------*
      *                  * Se l'incremento porta a zero si forza il    *
      *                  * valore a 1                                  *
      *                  *---------------------------------------------*
           if        w-enc-cod-gru-pos    =    zero
                     move  1              to   w-enc-cod-gru-pos      .
      *                  *---------------------------------------------*
      *                  * Controllo se esiste gia' un record con il   *
      *                  * codice pari al valore incrementato          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODGRU    "         to   f-key                  .
           move      w-enc-cod-gru-cla    to   rf-zp2-cod-cla         .
           move      w-enc-cod-gru-pos    to   rf-zp2-cod-gru         .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
           if        f-sts                =    e-not-err
                     go to att-gru-aut-600
           else      go to att-gru-aut-700.
       att-gru-aut-600.
      *                  *---------------------------------------------*
      *                  * Se esiste gia' un record con il codice pari *
      *                  * al valore incrementato                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ulteriore incremento del valore         *
      *                      *-----------------------------------------*
           add       1                    to   w-enc-cod-gru-pos      .
      *                      *-----------------------------------------*
      *                      * Riciclo a controllo di esistenza        *
      *                      *-----------------------------------------*
           go to     att-gru-aut-500.
       att-gru-aut-700.
      *                  *---------------------------------------------*
      *                  * Se non esiste gia' un record con il codice  *
      *                  * pari al valore incrementato                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Aggiornamento record classe con ultimo  *
      *                      * codice utilizzato                       *
      *                      *-----------------------------------------*
           move      w-enc-cod-gru-pos    to   rf-zp1-ult-cod         .
           move      "UP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
      *                      *-----------------------------------------*
      *                      * Rilascio record classe                  *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
       att-gru-aut-999.
           exit.

      *    *===========================================================*
      *    * Routine di ripristino codice gruppo automatico            *
      *    *-----------------------------------------------------------*
       rip-gru-aut-000.
      *              *-------------------------------------------------*
      *              * Lettura record classe                           *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "CODCLA    "         to   f-key                  .
           move      w-enc-cod-gru-cla    to   rf-zp1-cod-cla         .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
           if        f-sts                =    e-not-err
                     go to rip-gru-aut-400.
       rip-gru-aut-200.
      *              *-------------------------------------------------*
      *              * Record classe non esistente                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rip-gru-aut-999.
       rip-gru-aut-400.
      *              *-------------------------------------------------*
      *              * Record classe esistente                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Confronto tra il valore attuale ed il valo- *
      *                  * re post incremento                          *
      *                  *---------------------------------------------*
           if        rf-zp1-ult-cod       =    w-enc-cod-gru-pos
                     go to rip-gru-aut-600.
       rip-gru-aut-500.
      *                  *---------------------------------------------*
      *                  * Se il valore attuale non e' uguale al valo- *
      *                  * re post incremento                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rilascio record classe                  *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     rip-gru-aut-999.
       rip-gru-aut-600.
      *                  *---------------------------------------------*
      *                  * Se il valore attuale e' uguale al valore    *
      *                  * post incremento                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Aggiornamento record classe con ultimo  *
      *                      * codice utilizzato                       *
      *                      *-----------------------------------------*
           move      w-enc-cod-gru-pre    to   rf-zp1-ult-cod         .
           move      "UP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
      *                      *-----------------------------------------*
      *                      * Rilascio record classe                  *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
       rip-gru-aut-999.
           exit.

      *    *===========================================================*
      *    * Routine di attribuzione codice sottogruppo automatico     *
      *    *-----------------------------------------------------------*
       att-sgr-aut-000.
      *              *-------------------------------------------------*
      *              * Lettura record gruppo                           *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "CODGRU    "         to   f-key                  .
           move      w-enc-cod-sgr-cla    to   rf-zp2-cod-cla         .
           move      w-enc-cod-sgr-gru    to   rf-zp2-cod-gru         .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
           if        f-sts                =    e-not-err
                     go to att-sgr-aut-400.
       att-sgr-aut-200.
      *              *-------------------------------------------------*
      *              * Record gruppo non esistente                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ricerca del codice automatico per scansione *
      *                  * sequenziale                                 *
      *                  *---------------------------------------------*
           move      zero                 to   w-enc-cod-sgr-pos      .
       att-sgr-aut-250.
           add       1                    to   w-enc-cod-sgr-pos      .
           if        w-enc-cod-sgr-pos    =    zero
                     move  1              to   w-enc-cod-sgr-pos      .
           move      "RK"                 to   f-ope                  .
           move      "CODSGR    "         to   f-key                  .
           move      w-enc-cod-sgr-cla    to   rf-zp3-cod-cla         .
           move      w-enc-cod-sgr-gru    to   rf-zp3-cod-gru         .
           move      w-enc-cod-sgr-pos    to   rf-zp3-cod-sgr         .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
           if        f-sts                =    e-not-err
                     go to att-sgr-aut-250.
           go to     att-sgr-aut-999.
       att-sgr-aut-400.
      *              *-------------------------------------------------*
      *              * Record gruppo esistente                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Memorizzazione del valore pre incremento    *
      *                  *---------------------------------------------*
           move      rf-zp2-ult-cod       to   w-enc-cod-sgr-pre      .
      *                  *---------------------------------------------*
      *                  * Incremento del valore                       *
      *                  *---------------------------------------------*
           move      w-enc-cod-sgr-pre    to   w-enc-cod-sgr-pos      .
           add       1                    to   w-enc-cod-sgr-pos      .
       att-sgr-aut-500.
      *                  *---------------------------------------------*
      *                  * Se l'incremento porta a zero si forza il    *
      *                  * valore a 1                                  *
      *                  *---------------------------------------------*
           if        w-enc-cod-sgr-pos    =    zero
                     move  1              to   w-enc-cod-sgr-pos      .
      *                  *---------------------------------------------*
      *                  * Controllo se esiste gia' un record con il   *
      *                  * codice pari al valore incrementato          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSGR    "         to   f-key                  .
           move      w-enc-cod-sgr-cla    to   rf-zp3-cod-cla         .
           move      w-enc-cod-sgr-gru    to   rf-zp3-cod-gru         .
           move      w-enc-cod-sgr-pos    to   rf-zp3-cod-sgr         .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
           if        f-sts                =    e-not-err
                     go to att-sgr-aut-600
           else      go to att-sgr-aut-700.
       att-sgr-aut-600.
      *                  *---------------------------------------------*
      *                  * Se esiste gia' un record con il codice pari *
      *                  * al valore incrementato                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ulteriore incremento del valore         *
      *                      *-----------------------------------------*
           add       1                    to   w-enc-cod-sgr-pos      .
      *                      *-----------------------------------------*
      *                      * Riciclo a controllo di esistenza        *
      *                      *-----------------------------------------*
           go to     att-sgr-aut-500.
       att-sgr-aut-700.
      *                  *---------------------------------------------*
      *                  * Se non esiste gia' un record con il codice  *
      *                  * pari al valore incrementato                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Aggiornamento record gruppo con ultimo  *
      *                      * codice utilizzato                       *
      *                      *-----------------------------------------*
           move      w-enc-cod-sgr-pos    to   rf-zp2-ult-cod         .
           move      "UP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
      *                      *-----------------------------------------*
      *                      * Rilascio record gruppo                  *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
       att-sgr-aut-999.
           exit.

      *    *===========================================================*
      *    * Routine di ripristino codice sottogruppo automatico       *
      *    *-----------------------------------------------------------*
       rip-sgr-aut-000.
      *              *-------------------------------------------------*
      *              * Lettura record gruppo                           *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "CODGRU    "         to   f-key                  .
           move      w-enc-cod-sgr-cla    to   rf-zp2-cod-cla         .
           move      w-enc-cod-sgr-gru    to   rf-zp2-cod-gru         .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
           if        f-sts                =    e-not-err
                     go to rip-sgr-aut-400.
       rip-sgr-aut-200.
      *              *-------------------------------------------------*
      *              * Record gruppo non esistente                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rip-sgr-aut-999.
       rip-sgr-aut-400.
      *              *-------------------------------------------------*
      *              * Record gruppo esistente                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Confronto tra il valore attuale ed il valo- *
      *                  * re post incremento                          *
      *                  *---------------------------------------------*
           if        rf-zp2-ult-cod       =    w-enc-cod-sgr-pos
                     go to rip-sgr-aut-600.
       rip-sgr-aut-500.
      *                  *---------------------------------------------*
      *                  * Se il valore attuale non e' uguale al valo- *
      *                  * re post incremento                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rilascio record gruppo                  *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     rip-sgr-aut-999.
       rip-sgr-aut-600.
      *                  *---------------------------------------------*
      *                  * Se il valore attuale e' uguale al valore    *
      *                  * post incremento                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Aggiornamento record gruppo con ultimo  *
      *                      * codice utilizzato                       *
      *                      *-----------------------------------------*
           move      w-enc-cod-sgr-pre    to   rf-zp2-ult-cod         .
           move      "UP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
      *                      *-----------------------------------------*
      *                      * Rilascio record gruppo                  *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
       rip-sgr-aut-999.
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
      *    * Routine lettura tabella [zp1]                             *
      *    *-----------------------------------------------------------*
       let-arc-zp1-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zp1-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice classe a zero                    *
      *              *-------------------------------------------------*
           if        w-let-arc-zp1-cla    =    zero
                     go to let-arc-zp1-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLA    "         to   f-key                  .
           move      w-let-arc-zp1-cla    to   rf-zp1-cod-cla         .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zp1-400.
       let-arc-zp1-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zp1-des-cla       to   w-let-arc-zp1-des      .
           move      rf-zp1-mne-cla       to   w-let-arc-zp1-mne      .
           move      rf-zp1-ult-sud       to   w-let-arc-zp1-sud      .
           move      rf-zp1-umi-def       to   w-let-arc-zp1-umi      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zp1-999.
       let-arc-zp1-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zp1-flg      .
           move      all   "."            to   w-let-arc-zp1-des      .
           go to     let-arc-zp1-600.
       let-arc-zp1-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zp1-des      .
       let-arc-zp1-600.
           move      spaces               to   w-let-arc-zp1-mne      .
           move      zero                 to   w-let-arc-zp1-sud      .
           move      spaces               to   w-let-arc-zp1-umi      .
       let-arc-zp1-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zp2]                             *
      *    *-----------------------------------------------------------*
       let-arc-zp2-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zp2-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice gruppo a zero                    *
      *              *-------------------------------------------------*
           if        w-let-arc-zp2-gru    =    zero
                     go to let-arc-zp2-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODGRU    "         to   f-key                  .
           move      w-let-arc-zp2-cla    to   rf-zp2-cod-cla         .
           move      w-let-arc-zp2-gru    to   rf-zp2-cod-gru         .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zp2-400.
       let-arc-zp2-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zp2-des-gru       to   w-let-arc-zp2-des      .
           move      rf-zp2-mne-gru       to   w-let-arc-zp2-mne      .
           move      rf-zp2-ult-sud       to   w-let-arc-zp2-sud      .
           move      rf-zp2-umi-def       to   w-let-arc-zp2-umi      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zp2-999.
       let-arc-zp2-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zp2-flg      .
           move      all   "."            to   w-let-arc-zp2-des      .
           go to     let-arc-zp2-600.
       let-arc-zp2-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zp2-des      .
       let-arc-zp2-600.
           move      spaces               to   w-let-arc-zp2-mne      .
           move      zero                 to   w-let-arc-zp2-sud      .
           move      spaces               to   w-let-arc-zp2-umi      .
       let-arc-zp2-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zp3]                             *
      *    *-----------------------------------------------------------*
       let-arc-zp3-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zp3-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice sottogruppo a zero               *
      *              *-------------------------------------------------*
           if        w-let-arc-zp3-sgr    =    zero
                     go to let-arc-zp3-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSGR    "         to   f-key                  .
           move      w-let-arc-zp3-cla    to   rf-zp3-cod-cla         .
           move      w-let-arc-zp3-gru    to   rf-zp3-cod-gru         .
           move      w-let-arc-zp3-sgr    to   rf-zp3-cod-sgr         .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zp3-400.
       let-arc-zp3-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zp3-des-sgr       to   w-let-arc-zp3-des      .
           move      rf-zp3-mne-sgr       to   w-let-arc-zp3-mne      .
           move      rf-zp3-ult-sud       to   w-let-arc-zp3-sud      .
           move      rf-zp3-umi-def       to   w-let-arc-zp3-umi      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zp3-999.
       let-arc-zp3-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zp3-flg      .
           move      all   "."            to   w-let-arc-zp3-des      .
           go to     let-arc-zp3-600.
       let-arc-zp3-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zp3-des      .
       let-arc-zp3-600.
           move      spaces               to   w-let-arc-zp3-mne      .
           move      zero                 to   w-let-arc-zp3-sud      .
           move      spaces               to   w-let-arc-zp3-umi      .
       let-arc-zp3-999.
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
      *    * Determinazione se elemento cancellabile                   *
      *    *-----------------------------------------------------------*
       det-snx-del-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo elemento        *
      *              *-------------------------------------------------*
           if        w-tes-tip-ele        =    01
                     go to det-snx-del-200
           else if   w-tes-tip-ele        =    02
                     go to det-snx-del-400
           else if   w-tes-tip-ele        =    03
                     go to det-snx-del-600
           else      go to det-snx-del-800.
       det-snx-del-200.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : classe                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione se esistenza di gruppi nella *
      *                  * classe                                      *
      *                  *---------------------------------------------*
           move      w-tes-cod-cla        to   w-det-gru-cla-cla      .
           perform   det-gru-cla-000      thru det-gru-cla-999        .
      *                  *---------------------------------------------*
      *                  * Deviazione secondo l'esito                  *
      *                  *---------------------------------------------*
           if        w-det-gru-cla-snx    =    "S"
                     go to det-snx-del-900
           else      go to det-snx-del-800.
       det-snx-del-400.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : gruppo                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione se esistenza di sottogruppi  *
      *                  * nel gruppo                                  *
      *                  *---------------------------------------------*
           move      w-tes-cod-cla        to   w-det-sgr-gru-cla      .
           move      w-tes-cod-gru        to   w-det-sgr-gru-gru      .
           move      zero                 to   w-det-gru-cla-sgr      .
           perform   det-sgr-gru-000      thru det-sgr-gru-999        .
      *                  *---------------------------------------------*
      *                  * Deviazione secondo l'esito                  *
      *                  *---------------------------------------------*
           if        w-det-sgr-gru-snx    =    "S"
                     go to det-snx-del-900
           else      go to det-snx-del-800.
       det-snx-del-600.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : sottogruppo                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sempre elemento cancellabile                *
      *                  *---------------------------------------------*
           go to     det-snx-del-800.
       det-snx-del-800.
      *              *-------------------------------------------------*
      *              * Se elemento cancellabile                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Esito del test a : Si                       *
      *                  *---------------------------------------------*
           move      "S"                  to   w-det-snx-del-snx      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-snx-del-999.
       det-snx-del-900.
      *              *-------------------------------------------------*
      *              * Se elemento non cancellabile                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Esito del test a : No                       *
      *                  *---------------------------------------------*
           move      "N"                  to   w-det-snx-del-snx      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-snx-del-999.
       det-snx-del-999.
           exit.

      *    *===========================================================*
      *    * Determinazione se gruppi nella classe                     *
      *    *-----------------------------------------------------------*
       det-gru-cla-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare contatori           *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-gru-cla-gru      .
           move      zero                 to   w-det-gru-cla-sgr      .
           move      zero                 to   w-det-gru-cla-ele      .
       det-gru-cla-100.
      *              *-------------------------------------------------*
      *              * Start su gruppi della classe                    *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODGRU    "         to   f-key                  .
           move      w-det-gru-cla-cla    to   rf-zp2-cod-cla         .
           move      zero                 to   rf-zp2-cod-gru         .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
      *                  *---------------------------------------------*
      *                  * Se start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-gru-cla-900.
       det-gru-cla-200.
      *              *-------------------------------------------------*
      *              * Next su gruppi della classe                     *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
      *                  *---------------------------------------------*
      *                  * Se at end : a test su contatore             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-gru-cla-700.
       det-gru-cla-300.
      *              *-------------------------------------------------*
      *              * Max su gruppi della classe                      *
      *              *-------------------------------------------------*
           if        rf-zp2-cod-cla       not  = w-det-gru-cla-cla
                     go to det-gru-cla-700.
       det-gru-cla-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
       det-gru-cla-500.
      *              *-------------------------------------------------*
      *              * Incremento contatore gruppi rilevati            *
      *              *-------------------------------------------------*
           add       1                    to   w-det-gru-cla-gru      .
      *              *-------------------------------------------------*
      *              * Incremento contatore elementi buffer            *
      *              *-------------------------------------------------*
           add       1                    to   w-det-gru-cla-ele      .
           if        w-det-gru-cla-ele    >    w-det-gru-cla-max
                     go to det-gru-cla-700.
      *              *-------------------------------------------------*
      *              * Bufferizzazione elemento                        *
      *              *-------------------------------------------------*
           move      rf-zp2-cod-gru       to   w-det-gru-cla-bcg
                                              (w-det-gru-cla-ele)     .
           move      zero                 to   w-det-gru-cla-bcs
                                              (w-det-gru-cla-ele)     .
      *              *-------------------------------------------------*
      *              * Ciclo di scansione sottogruppi                  *
      *              *-------------------------------------------------*
           move      w-det-gru-cla-cla    to   w-det-sgr-gru-cla      .
           move      rf-zp2-cod-gru       to   w-det-sgr-gru-gru      .
           perform   det-sgr-gru-000      thru det-sgr-gru-999        .
       det-gru-cla-600.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     det-gru-cla-200.
       det-gru-cla-700.
      *              *-------------------------------------------------*
      *              * Test su contatore                               *
      *              *-------------------------------------------------*
           if        w-det-gru-cla-gru    =    zero
                     go to det-gru-cla-900
           else      go to det-gru-cla-800.
       det-gru-cla-800.
      *              *-------------------------------------------------*
      *              * Se ci sono gruppi nella classe                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Esito del test a : Si                       *
      *                  *---------------------------------------------*
           move      "S"                  to   w-det-gru-cla-snx      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-gru-cla-999.
       det-gru-cla-900.
      *              *-------------------------------------------------*
      *              * Se non ci sono gruppi nella classe              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Esito del test a : No                       *
      *                  *---------------------------------------------*
           move      "N"                  to   w-det-gru-cla-snx      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-gru-cla-999.
       det-gru-cla-999.
           exit.

      *    *===========================================================*
      *    * Determinazione se sottogruppi nel gruppo                  *
      *    *-----------------------------------------------------------*
       det-sgr-gru-000.
      *              *-------------------------------------------------*
      *              * Start su sottogruppi del gruppo                 *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODSGR    "         to   f-key                  .
           move      w-det-sgr-gru-cla    to   rf-zp3-cod-cla         .
           move      w-det-sgr-gru-gru    to   rf-zp3-cod-gru         .
           move      zero                 to   rf-zp3-cod-sgr         .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
           if        f-sts                not  = e-not-err
                     go to det-sgr-gru-900.
       det-sgr-gru-200.
      *              *-------------------------------------------------*
      *              * Next su sottogruppi del gruppo                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
           if        f-sts                not  = e-not-err
                     go to det-sgr-gru-700.
       det-sgr-gru-300.
      *              *-------------------------------------------------*
      *              * Max su gruppi della classe                      *
      *              *-------------------------------------------------*
           if        rf-zp3-cod-cla       not  = w-det-sgr-gru-cla or
                     rf-zp3-cod-gru       not  = w-det-sgr-gru-gru
                     go to det-sgr-gru-700.
       det-sgr-gru-500.
      *              *-------------------------------------------------*
      *              * Incremento contatore sottogruppi rilevati       *
      *              *-------------------------------------------------*
           add       1                    to   w-det-gru-cla-sgr      .
      *              *-------------------------------------------------*
      *              * Se in cancellazione di un gruppo si ignora la   *
      *              * bufferizzazione                                 *
      *              *-------------------------------------------------*
           if        w-tes-tip-ele        not  = 01
                     go to det-sgr-gru-600.
      *              *-------------------------------------------------*
      *              * Incremento contatore elementi buffer            *
      *              *-------------------------------------------------*
           add       1                    to   w-det-gru-cla-ele      .
           if        w-det-gru-cla-ele    >    w-det-gru-cla-max
                     go to det-sgr-gru-700.
      *              *-------------------------------------------------*
      *              * Bufferizzazione elemento                        *
      *              *-------------------------------------------------*
           move      rf-zp3-cod-gru       to   w-det-gru-cla-bcg
                                              (w-det-gru-cla-ele)     .
           move      rf-zp3-cod-sgr       to   w-det-gru-cla-bcs
                                              (w-det-gru-cla-ele)     .
       det-sgr-gru-600.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     det-sgr-gru-200.
       det-sgr-gru-700.
      *              *-------------------------------------------------*
      *              * Test su contatore                               *
      *              *-------------------------------------------------*
           if        w-det-gru-cla-sgr    =    zero
                     go to det-sgr-gru-900
           else      go to det-sgr-gru-800.
       det-sgr-gru-800.
      *              *-------------------------------------------------*
      *              * Se ci sono sottogruppi nel gruppo               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Esito del test a : Si                       *
      *                  *---------------------------------------------*
           move      "S"                  to   w-det-sgr-gru-snx      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-sgr-gru-999.
       det-sgr-gru-900.
      *              *-------------------------------------------------*
      *              * Se non ci sono sottogruppi nel gruppo           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Esito del test a : No                       *
      *                  *---------------------------------------------*
           move      "N"                  to   w-det-sgr-gru-snx      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-sgr-gru-999.
       det-sgr-gru-999.
           exit.

      *    *===========================================================*
      *    * Controllo unicita' numero di sequenza                     *
      *    *-----------------------------------------------------------*
       ctl-uni-nrs-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-ctl-uni-nrs-flg      .
      *              *-------------------------------------------------*
      *              * Contatore records letti con numero sequenza     *
      *              * pari a quello passato, ma con codice diverso    *
      *              * da quello passato : a zero                      *
      *              *-------------------------------------------------*
           move      zero                 to   w-ctl-uni-nrs-ctr      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo elemento        *
      *              *-------------------------------------------------*
           if        w-ctl-uni-nrs-ele    =    01
                     go to ctl-uni-nrs-100
           else if   w-ctl-uni-nrs-ele    =    02
                     go to ctl-uni-nrs-200
           else if   w-ctl-uni-nrs-ele    =    03
                     go to ctl-uni-nrs-300.
       ctl-uni-nrs-100.
      *              *-------------------------------------------------*
      *              * Tipo elemento : classe                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su file [zp1]                         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "SQZNUM    "         to   f-key                  .
           move      w-ctl-uni-nrs-nrs    to   rf-zp1-sqz-num         .
           move      zero                 to   rf-zp1-cod-cla         .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
      *                  *---------------------------------------------*
      *                  * Se errata : a trattamento finale            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ctl-uni-nrs-600.
       ctl-uni-nrs-110.
      *                  *---------------------------------------------*
      *                  * Lettura sequenziale file [zp1]              *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : a trattamento finale          *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ctl-uni-nrs-600.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
           if        rf-zp1-sqz-num       not  = w-ctl-uni-nrs-nrs
                     go to ctl-uni-nrs-600.
      *                  *---------------------------------------------*
      *                  * Se codice classe letto diverso da quello    *
      *                  * passato incremento il contatore             *
      *                  *---------------------------------------------*
           if        rf-zp1-cod-cla       not  = w-ctl-uni-nrs-cla
                     add   1              to   w-ctl-uni-nrs-ctr      .
      *                  *---------------------------------------------*
      *                  * Riciclo in lettura                          *
      *                  *---------------------------------------------*
           go to     ctl-uni-nrs-110.
       ctl-uni-nrs-200.
      *              *-------------------------------------------------*
      *              * Tipo elemento : gruppo                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su file [zp2]                         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "SQZNUM    "         to   f-key                  .
           move      w-ctl-uni-nrs-cla    to   rf-zp2-cod-cla         .
           move      w-ctl-uni-nrs-nrs    to   rf-zp2-sqz-num         .
           move      zero                 to   rf-zp2-cod-gru         .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
      *                  *---------------------------------------------*
      *                  * Se errata : a trattamento finale            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ctl-uni-nrs-600.
       ctl-uni-nrs-210.
      *                  *---------------------------------------------*
      *                  * Lettura sequenziale file [zp2]              *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : a trattamento finale          *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ctl-uni-nrs-600.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
           if        rf-zp2-cod-cla       not  = w-ctl-uni-nrs-cla or
                     rf-zp2-sqz-num       not  = w-ctl-uni-nrs-nrs
                     go to ctl-uni-nrs-600.
      *                  *---------------------------------------------*
      *                  * Se codice gruppo letto diverso da quello    *
      *                  * passato incremento il contatore             *
      *                  *---------------------------------------------*
           if        rf-zp2-cod-gru       not  = w-ctl-uni-nrs-gru
                     add   1              to   w-ctl-uni-nrs-ctr      .
      *                  *---------------------------------------------*
      *                  * Riciclo in lettura                          *
      *                  *---------------------------------------------*
           go to     ctl-uni-nrs-210.
       ctl-uni-nrs-300.
      *              *-------------------------------------------------*
      *              * Tipo elemento : sottogruppo                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su file [zp3]                         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "SQZNUM    "         to   f-key                  .
           move      w-ctl-uni-nrs-cla    to   rf-zp3-cod-cla         .
           move      w-ctl-uni-nrs-gru    to   rf-zp3-cod-gru         .
           move      w-ctl-uni-nrs-nrs    to   rf-zp3-sqz-num         .
           move      zero                 to   rf-zp3-cod-sgr         .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
      *                  *---------------------------------------------*
      *                  * Se errata : a trattamento finale            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ctl-uni-nrs-600.
       ctl-uni-nrs-310.
      *                  *---------------------------------------------*
      *                  * Lettura sequenziale file [zp3]              *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : a trattamento finale          *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ctl-uni-nrs-600.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
           if        rf-zp3-cod-cla       not  = w-ctl-uni-nrs-cla or
                     rf-zp3-cod-gru       not  = w-ctl-uni-nrs-gru or
                     rf-zp3-sqz-num       not  = w-ctl-uni-nrs-nrs
                     go to ctl-uni-nrs-600.
      *                  *---------------------------------------------*
      *                  * Se codice sottogruppo letto diverso da quel-*
      *                  * lo passato incremento il contatore          *
      *                  *---------------------------------------------*
           if        rf-zp3-cod-sgr       not  = w-ctl-uni-nrs-sgr
                     add   1              to   w-ctl-uni-nrs-ctr      .
      *                  *---------------------------------------------*
      *                  * Riciclo in lettura                          *
      *                  *---------------------------------------------*
           go to     ctl-uni-nrs-310.
       ctl-uni-nrs-600.
      *              *-------------------------------------------------*
      *              * Trattamento finale                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se il contatore e' a zero : uscita Ok       *
      *                  *---------------------------------------------*
           if        w-ctl-uni-nrs-ctr    =    zero
                     go to ctl-uni-nrs-999.
      *                  *---------------------------------------------*
      *                  * Altrimenti : uscita per errore              *
      *                  *---------------------------------------------*
           move      "#"                  to   w-ctl-uni-nrs-flg      .
       ctl-uni-nrs-999.
           exit.

      *    *===========================================================*
      *    * Test se presente record 'zero' nel file [zp1]             *
      *    *-----------------------------------------------------------*
       ctl-rcz-zp1-000.
      *              *-------------------------------------------------*
      *              * Open file [zp1]                                 *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
       ctl-rcz-zp1-100.
      *              *-------------------------------------------------*
      *              * Tentativo di lettura record 'zero'              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLA    "         to   f-key                  .
           move      zero                 to   rf-zp1-cod-cla         .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
      *                  *---------------------------------------------*
      *                  * Se lettura a buon fine : uscita             *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to ctl-rcz-zp1-900.
       ctl-rcz-zp1-200.
      *              *-------------------------------------------------*
      *              * Scrittura record zero                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
      *                  *---------------------------------------------*
      *                  * Composizione record 'zero'                  *
      *                  *---------------------------------------------*
           move      zero                 to   rf-zp1-cod-cla         .
           move      "*** (Prodotti non classificati) ***"
                                          to   rf-zp1-des-cla         .
           move      "(NC) "              to   rf-zp1-mne-cla         .
           move      "*** (PRODOTTI NON CLASSIFICATI) ***"
                                          to   rf-zp1-des-key         .
           move      9999999              to   rf-zp1-sqz-num         .
           move      01                   to   rf-zp1-ult-sud         .
           move      spaces               to   rf-zp1-umi-def         .
           move      zero                 to   rf-zp1-ult-cod         .
           move      spaces               to   rf-zp1-alx-exp         .
      *                  *---------------------------------------------*
      *                  * Put record                                  *
      *                  *---------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
       ctl-rcz-zp1-900.
      *              *-------------------------------------------------*
      *              * Chiusura file [zp1]                             *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
       ctl-rcz-zp1-999.
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
      *    * Subroutines per l'accettazione del codice classe          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acmnzp10.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice gruppo          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acmnzp20.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice sottogruppo     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acmnzp30.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione unita' di misura           *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acodzum0.acs"                   .

