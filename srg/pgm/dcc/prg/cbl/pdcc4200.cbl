       Identification Division.
       Program-Id.                                 pdcc4200           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dcc                 *
      *                                Settore:    com                 *
      *                                   Fase:    dcc420              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 12/10/93    *
      *                       Ultima revisione:    NdK del 14/11/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione descrizione prodotti di vendita    *
      *                    per un cliente                              *
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
                     "dcc"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "com"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "dcc420"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pdcc4200"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "  DESCRIZIONE E CODICE PRODOTTO CLIENTE "       .

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
      *            * Impostazione testata                              *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-tes      pic  x(01)                  .
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
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [pdx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfpdx"                          .
      *        *-------------------------------------------------------*
      *        * [pdk]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfpdk"                          .

      *    *===========================================================*
      *    * Work-area per bufferizzazione testata                     *
      *    *-----------------------------------------------------------*
       01  w-tes.
      *        *-------------------------------------------------------*
      *        * Valori chiave                                         *
      *        *-------------------------------------------------------*
           05  w-tes-val-key.
               10  w-tes-cod-cli          pic  9(07)                  .
               10  w-tes-cod-cli-rag      pic  x(40)                  .
               10  w-tes-num-pro          pic  9(07)                  .
               10  w-tes-num-pro-alf      pic  x(14)                  .
               10  w-tes-num-pro-des      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
               10  w-tes-cpr-cli          pic  x(40)                  .
               10  w-tes-dpr-cli.
                   15  w-tes-dpr-rig occurs 10
                                          pic  x(40)                  .

      *    *===========================================================*
      *    * Work-area referenze                                       *
      *    *-----------------------------------------------------------*
       01  w-ref.
      *        *-------------------------------------------------------*
      *        * Default per parametri di accettazione                 *
      *        *-------------------------------------------------------*
           05  w-ref-prm-acc.
               10  w-ref-prm-acc-str.
                   15  w-ref-prm-acc-aaa  pic  9(07)                  .
                   15  filler             pic  x(01)                  .
                   15  w-ref-prm-acc-bbb  pic  9(02)                  .
                   15  filler             pic  x(01)                  .
                   15  w-ref-prm-acc-ccc  pic  x(01)                  .
                   15  filler             pic  x(01)                  .
                   15  w-ref-prm-acc-ddd  pic  9(02)                  .
                   15  filler             pic  x(01)                  .
                   15  w-ref-prm-acc-eee  pic  9(02)                  .
                   15  filler             pic  x(02)                  .
               10  w-ref-prm-acc-tbl.
                   15   w-ref-prm-acc-ele occurs 99
                                 indexed  by   w-ref-prm-acc-inx      .
                        20  w-ref-prm-acc-cli
                                          pic  9(07)                  .
                        20  w-ref-prm-acc-lco
                                          pic  9(02)                  .
                        20  w-ref-prm-acc-cco
                                          pic  x(01)                  .
                        20  w-ref-prm-acc-rde
                                          pic  9(02)                  .
                        20  w-ref-prm-acc-lde
                                          pic  9(02)                  .
               10  w-ref-prm-acc-c01      pic  9(03)                  .

      *    *===========================================================*
      *    * Work per subroutines di Acc                               *
      *    *-----------------------------------------------------------*
       01  w-acc.
      *        *-------------------------------------------------------*
      *        * Work per accettazione codice prodotto per il cliente  *
      *        *-------------------------------------------------------*
           05  w-acc-cpr-cli.
      *            *---------------------------------------------------*
      *            * Lunghezza massima codice per il cliente           *
      *            *---------------------------------------------------*
               10  w-acc-cpr-cli-lco      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Tipo accettazione codice per il cliente           *
      *            *---------------------------------------------------*
               10  w-acc-cpr-cli-cco      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per accettazione descrizione prodotto cliente    *
      *        *-------------------------------------------------------*
           05  w-acc-dpr-cli.
      *            *---------------------------------------------------*
      *            * Numero massimo di righe di descrizione            *
      *            *---------------------------------------------------*
               10  w-acc-dpr-cli-rde      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Lunghezza massima descrizione per il cliente      *
      *            *---------------------------------------------------*
               10  w-acc-dpr-cli-lde      pic  9(02)                  .
               
      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcp.
               10  w-let-arc-dcp-flg      pic  x(01)                  .
               10  w-let-arc-dcp-cod      pic  9(07)                  .
               10  w-let-arc-dcp-des      pic  x(40)                  .
               10  w-let-arc-dcp-alf      pic  x(14)                  .
               10  w-let-arc-dcp-sgv      pic  x(03)                  .
               10  w-let-arc-dcp-dcv      pic  9(01)                  .
               10  w-let-arc-dcp-prz      pic  9(09)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [cli]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-cli.
               10  w-let-arc-cli-flg      pic  x(01)                  .
               10  w-let-arc-cli-cod      pic  9(07)                  .
               10  w-let-arc-cli-rag      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcc.
               10  w-let-arc-dcc-flg      pic  x(01)                  .
               10  w-let-arc-dcc-cli      pic  9(07)                  .
               10  w-let-arc-dcc-dpz      pic  x(04)                  .
               10  w-let-arc-dcc-rag      pic  x(40)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Salvataggio per accettazione codice cliente           *
      *        *-------------------------------------------------------*
           05  w-sav-acc-cod-cli          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Salvataggio status in lettura registrazione pre-esi-  *
      *        * stente per il codice prodotto per il cliente          *
      *        *-------------------------------------------------------*
           05  w-sav-rou-let-stc          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Salvataggio valore eventualmente letto                *
      *        *-------------------------------------------------------*
           05  w-sav-rou-let-cpr          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Salvataggio status in lettura registrazione pre-esi-  *
      *        * stente per la descrizione prodotto per il cliente     *
      *        *-------------------------------------------------------*
           05  w-sav-rou-let-std          pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di compattamento                     *
      *    *-----------------------------------------------------------*
       01  w-cmp.
      *        *-------------------------------------------------------*
      *        * Work per compattamento righe descrizione prodotto     *
      *        *-------------------------------------------------------*
           05  w-cmp-rig-dep.
               10  w-cmp-rig-dep-flg      pic  x(01)                  .
               10  w-cmp-rig-dep-nri      pic  9(02)                  .
               10  w-cmp-rig-dep-nrd      pic  9(02)                  .
               10  w-cmp-rig-dep-ctr      pic  9(02)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice cliente commerciale     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmndcc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice prodotto 'dcp'          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acoddcp0.acl"                   .

      *    *===========================================================*
      *    * Work-area per contatori e indici                          *
      *    *-----------------------------------------------------------*
       01  w-cix.
      *        *-------------------------------------------------------*
      *        * Contatori di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-cix-ctr-001              pic  9(03)                  .
           05  w-cix-ctr-002              pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area per valori di default                           *
      *    *-----------------------------------------------------------*
       01  w-def.
      *        *-------------------------------------------------------*
      *        * Codice cliente                                        *
      *        *-------------------------------------------------------*
           05  w-def-cod-cli              pic  9(07)                  .

      *    *===========================================================*
      *    * Work-area per valori di i.p.c.                            *
      *    *-----------------------------------------------------------*
       01  w-ipc.
      *        *-------------------------------------------------------*
      *        * Variabili di i.p.c. da livello precedente             *
      *        *-------------------------------------------------------*
           05  w-ipc-dlp.
      *            *---------------------------------------------------*
      *            * Per inserimento o modifica                        *
      *            *---------------------------------------------------*
               10  w-ipc-dlp-iom.
      *                *-----------------------------------------------*
      *                * Segnale di primo passaggio                    *
      *                *-----------------------------------------------*
                   15  w-ipc-dlp-iom-spp  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Codice cliente                                *
      *                *-----------------------------------------------*
                   15  w-ipc-dlp-iom-cli  pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Codice prodotto                               *
      *                *-----------------------------------------------*
                   15  w-ipc-dlp-iom-pro  pic  9(07)                  .

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
      *              * Se il programma e' stato chiamato da un livello *
      *              * precedente, si esce dopo il primo giro          *
      *              *-------------------------------------------------*
           if        w-ipc-dlp-iom-spp    not  = spaces
                     go to main-800.
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
      *              * Se function key DELT si esce                    *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     go to exe-acc-cmp-999.
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
      *              * Open modulo accettazione codice cliente commer- *
      *              * ciale                                           *
      *              *-------------------------------------------------*
           perform   cod-mne-dcc-opn-000  thru cod-mne-dcc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice prodotto 'dcp'  *
      *              *-------------------------------------------------*
           perform   cod-cod-dcp-opn-000  thru cod-cod-dcp-opn-999    .
      *              *-------------------------------------------------*
      *              * Inizializzazione valori di default generale     *
      *              *-------------------------------------------------*
           move      zero                 to   w-def-cod-cli          .
      *              *-------------------------------------------------*
      *              * Estrazione variabili di i.p.c. per inserimento  *
      *              * o modifica                                      *
      *              *-------------------------------------------------*
           perform   ipc-dlp-iom-000      thru ipc-dlp-iom-999        .
      *              *-------------------------------------------------*
      *              * Lettura delle referenze relative ai parametri   *
      *              * di accettazione                                 *
      *              *-------------------------------------------------*
           perform   ref-prm-acc-000      thru ref-prm-acc-999        .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura i.p.c. da livello precedente per tipo di interro- *
      *    * gazione e parametro relativo                              *
      *    *-----------------------------------------------------------*
       ipc-dlp-iom-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione iniziale                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-ipc-dlp-iom-spp      .
           move      zero                 to   w-ipc-dlp-iom-cli      .
           move      zero                 to   w-ipc-dlp-iom-pro      .
       ipc-dlp-iom-300.
      *              *-------------------------------------------------*
      *              * Estrazione della eventuale variabile di i.p.c.  *
      *              * "cod-cli" per il cliente                        *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cod-cli"            to   s-var                  .
           move      "-"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     go to ipc-dlp-iom-999.
           if        s-num                =    zero
                     go to ipc-dlp-iom-999.
           move      s-num                to   w-ipc-dlp-iom-cli      .
       ipc-dlp-iom-600.
      *              *-------------------------------------------------*
      *              * Estrazione della eventuale variabile di i.p.c.  *
      *              * "num-pro" per il codice prodotto                *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "num-pro"            to   s-var                  .
           move      "-"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     go to ipc-dlp-iom-999.
           if        s-num                =    zero
                     move  zero           to   w-ipc-dlp-iom-cli
                     go to ipc-dlp-iom-999.
           move      s-num                to   w-ipc-dlp-iom-pro      .
       ipc-dlp-iom-999.
           exit.

      *    *===========================================================*
      *    * Lettura delle referenze relative ai parametri di accet-   *
      *    * tazione                                                   *
      *    *                                                           *
      *    * N.B.: Attualmente limitata a 99 elementi possibili        *
      *    *-----------------------------------------------------------*
       ref-prm-acc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione tabella iniziale                *
      *              *-------------------------------------------------*
           move      zero                 to   w-ref-prm-acc-c01      .
       ref-prm-acc-100.
           add       1                    to   w-ref-prm-acc-c01      .
           if        w-ref-prm-acc-c01    >    1
                     go to ref-prm-acc-110.
      *                  *---------------------------------------------*
      *                  * Normalizzazione primo elemento per default  *
      *                  *---------------------------------------------*
           move      zero                 to   w-ref-prm-acc-cli
                                              (w-ref-prm-acc-c01)     .
           move      14                   to   w-ref-prm-acc-lco
                                              (w-ref-prm-acc-c01)     .
           move      "U"                  to   w-ref-prm-acc-cco
                                              (w-ref-prm-acc-c01)     .
           move      10                   to   w-ref-prm-acc-rde
                                              (w-ref-prm-acc-c01)     .
           move      40                   to   w-ref-prm-acc-lde
                                              (w-ref-prm-acc-c01)     .
           go to     ref-prm-acc-100.
       ref-prm-acc-110.
      *                  *---------------------------------------------*
      *                  * Normalizzazione altri elementi              *
      *                  *---------------------------------------------*
           if        w-ref-prm-acc-c01    >    99
                     go to ref-prm-acc-120.
           move      zero                 to   w-ref-prm-acc-cli
                                              (w-ref-prm-acc-c01)     .
           move      zero                 to   w-ref-prm-acc-lco
                                              (w-ref-prm-acc-c01)     .
           move      spaces               to   w-ref-prm-acc-cco
                                              (w-ref-prm-acc-c01)     .
           move      zero                 to   w-ref-prm-acc-rde
                                              (w-ref-prm-acc-c01)     .
           move      zero                 to   w-ref-prm-acc-lde
                                              (w-ref-prm-acc-c01)     .
           go to     ref-prm-acc-100.
       ref-prm-acc-120.
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore                       *
      *              *-------------------------------------------------*
           move      1                    to   w-ref-prm-acc-c01      .
      *              *-------------------------------------------------*
      *              * Start per lettura referenza multipla            *
      *              *-------------------------------------------------*
           move      "Rs"                 to   s-ope                  .
           move      "pgm/dcc/lst/dcc420[prm-acc]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        s-ves                not  = spaces
                     go to ref-prm-acc-999.
       ref-prm-acc-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale referenza multipla          *
      *              *-------------------------------------------------*
           move      "Rn"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        s-ves                not  = spaces
                     go to ref-prm-acc-999.
      *              *-------------------------------------------------*
      *              * Valore referenza in comodo ridefinito           *
      *              *-------------------------------------------------*
           move      s-alf                to   w-ref-prm-acc-str      .
      *              *-------------------------------------------------*
      *              * Test su valore 'aaa'                            *
      *              *-------------------------------------------------*
           if        w-ref-prm-acc-aaa    not  numeric
                     go to ref-prm-acc-200.
      *              *-------------------------------------------------*
      *              * Test su contatore                               *
      *              *-------------------------------------------------*
           add       1                    to   w-ref-prm-acc-c01      .
           if        w-ref-prm-acc-c01    >    99
                     go to ref-prm-acc-999.
       ref-prm-acc-220.
      *              *-------------------------------------------------*
      *              * Normalizzazione valore contenuto in 'bbb'       *
      *              *-------------------------------------------------*
           if        w-ref-prm-acc-bbb    not  numeric
                     move  zero           to   w-ref-prm-acc-bbb      .
           if        w-ref-prm-acc-bbb    =    zero or
                     w-ref-prm-acc-bbb    >    40
                     move  14             to   w-ref-prm-acc-bbb      .
      *              *-------------------------------------------------*
      *              * Normalizzazione valore contenuto in 'ccc'       *
      *              *-------------------------------------------------*
           if        w-ref-prm-acc-ccc    not  = "U"
                     move  "L"            to   w-ref-prm-acc-ccc      .
      *              *-------------------------------------------------*
      *              * Normalizzazione valore contenuto in 'ddd'       *
      *              *-------------------------------------------------*
           if        w-ref-prm-acc-ddd    not  numeric
                     move  zero           to   w-ref-prm-acc-ddd      .
           if        w-ref-prm-acc-ddd    =    zero or
                     w-ref-prm-acc-ddd    >    10
                     move  10             to   w-ref-prm-acc-ddd      .
      *              *-------------------------------------------------*
      *              * Normalizzazione valore contenuto in 'eee'       *
      *              *-------------------------------------------------*
           if        w-ref-prm-acc-eee    not  numeric
                     move  zero           to   w-ref-prm-acc-eee      .
           if        w-ref-prm-acc-eee    =    zero or
                     w-ref-prm-acc-eee    >    40
                     move  40             to   w-ref-prm-acc-eee      .
      *              *-------------------------------------------------*
      *              * Bufferizzazione in tabella                      *
      *              *-------------------------------------------------*
           move      w-ref-prm-acc-aaa    to   w-ref-prm-acc-cli
                                              (w-ref-prm-acc-c01)     .
           move      w-ref-prm-acc-bbb    to   w-ref-prm-acc-lco
                                              (w-ref-prm-acc-c01)     .
           move      w-ref-prm-acc-ccc    to   w-ref-prm-acc-cco
                                              (w-ref-prm-acc-c01)     .
           move      w-ref-prm-acc-ddd    to   w-ref-prm-acc-rde
                                              (w-ref-prm-acc-c01)     .
           move      w-ref-prm-acc-eee    to   w-ref-prm-acc-lde
                                              (w-ref-prm-acc-c01)     .
      *              *-------------------------------------------------*
      *              * Riciclo su lettura sequenziale referenza        *
      *              *-------------------------------------------------*
           go to     ref-prm-acc-200.
       ref-prm-acc-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice cliente com-   *
      *              * merciale                                        *
      *              *-------------------------------------------------*
           perform   cod-mne-dcc-cls-000  thru cod-mne-dcc-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice prodotto 'dcp' *
      *              *-------------------------------------------------*
           perform   cod-cod-dcp-cls-000  thru cod-cod-dcp-cls-999    .
       pos-exe-pgm-999.
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
      *              * [pdx]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *              *-------------------------------------------------*
      *              * [pdk]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdk                 .
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
      *              * [pdx]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *              *-------------------------------------------------*
      *              * [pdk]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdk                 .
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
           move      spaces               to   w-cnt-sts-imp-key
                                               w-cnt-sts-imp-tes      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status visualizzazione prompts  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-pmt-key
                                               w-cnt-sts-pmt-tes      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status visualizzazione dati     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-vis-key
                                               w-cnt-sts-vis-tes      .
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
      *                  * Prompts per testata                         *
      *                  *---------------------------------------------*
           perform   pmt-tes-reg-000      thru pmt-tes-reg-999        .
           move      "#"                  to   w-cnt-sts-pmt-tes      .
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
      *                  * Codice cliente                              *
      *                  *---------------------------------------------*
           perform   acc-cod-cli-000      thru acc-cod-cli-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
       acc-key-reg-200.
      *                  *---------------------------------------------*
      *                  * Codice prodotto                             *
      *                  *---------------------------------------------*
           perform   acc-cod-pro-000      thru acc-cod-pro-999        .
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
      *              *-------------------------------------------------*
      *              * Codice cliente                                  *
      *              *-------------------------------------------------*
           perform   vis-cod-cli-000      thru vis-cod-cli-999        .
      *              *-------------------------------------------------*
      *              * Codice prodotto                                 *
      *              *-------------------------------------------------*
           perform   vis-cod-pro-000      thru vis-cod-pro-999        .
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
      *              * Codice cliente                                  *
      *              *-------------------------------------------------*
           perform   pmt-cod-cli-000      thru pmt-cod-cli-999        .
      *              *-------------------------------------------------*
      *              * Codice prodotto                                 *
      *              *-------------------------------------------------*
           perform   pmt-cod-pro-000      thru pmt-cod-pro-999        .
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
      *    * Prompt : Codice cliente                                   *
      *    *-----------------------------------------------------------*
       pmt-cod-cli-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      22                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice cliente       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Codice prodotto               *
      *    *-----------------------------------------------------------*
       pmt-cod-pro-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      22                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice prodotto      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-pro-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice cliente                             *
      *    *-----------------------------------------------------------*
       acc-cod-cli-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuali valori provenienti da livello     *
      *                  * precedente                                  *
      *                  *---------------------------------------------*
           if        w-ipc-dlp-iom-cli    =    zero
                     go to acc-cod-cli-025.
      *                  *---------------------------------------------*
      *                  * Attivazione del segnale di primo passaggio  *
      *                  *---------------------------------------------*
           move      "#"                  to   w-ipc-dlp-iom-spp      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione dei valori provenienti da   *
      *                  * livello precedente                          *
      *                  *---------------------------------------------*
           move      w-ipc-dlp-iom-cli    to   w-tes-cod-cli          .
           move      w-tes-cod-cli        to   w-let-arc-cli-cod      .
      *                      *-----------------------------------------*
      *                      * Lettura record [cli]                    *
      *                      *-----------------------------------------*
           perform   let-arc-cli-000      thru let-arc-cli-999        .
      *                      *-----------------------------------------*
      *                      * Lettura record [dcc]                    *
      *                      *-----------------------------------------*
           move      w-tes-cod-cli        to   w-let-arc-dcc-cli      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                      *-----------------------------------------*
      *                      * Memorizzazione ragione sociale          *
      *                      *-----------------------------------------*
           if        w-let-arc-dcc-flg    =    spaces
                     move  w-let-arc-dcc-rag
                                          to   w-tes-cod-cli-rag
           else      move  w-let-arc-cli-rag
                                          to   w-tes-cod-cli-rag      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione codice e ragione socia- *
      *                      * le del cliente                          *
      *                      *-----------------------------------------*
           perform   vis-cod-cli-000      thru vis-cod-cli-999        .
           perform   vis-cod-cli-rag-000  thru vis-cod-cli-rag-999    .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     acc-cod-cli-999.
       acc-cod-cli-025.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-cod-cli        to   w-sav-acc-cod-cli      .
       acc-cod-cli-050.
      *                  *---------------------------------------------*
      *                  * Preparazione valore di default              *
      *                  *---------------------------------------------*
           if        w-tes-cod-cli        not  = zero
                     go to acc-cod-cli-075.
           if        w-def-cod-cli        =    zero
                     go to acc-cod-cli-075.
           move      w-def-cod-cli        to   w-tes-cod-cli          .
           move      w-tes-cod-cli        to   w-let-arc-cli-cod      .
      *                      *-----------------------------------------*
      *                      * Lettura record [cli]                    *
      *                      *-----------------------------------------*
           perform   let-arc-cli-000      thru let-arc-cli-999        .
      *                      *-----------------------------------------*
      *                      * Lettura record [dcc]                    *
      *                      *-----------------------------------------*
           move      w-tes-cod-cli        to   w-let-arc-dcc-cli      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                      *-----------------------------------------*
      *                      * Memorizzazione ragione sociale          *
      *                      *-----------------------------------------*
           if        w-let-arc-dcc-flg    =    spaces
                     move  w-let-arc-dcc-rag
                                          to   w-tes-cod-cli-rag
           else      move  w-let-arc-cli-rag
                                          to   w-tes-cod-cli-rag      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione codice e ragione socia- *
      *                      * le del cliente                          *
      *                      *-----------------------------------------*
           perform   vis-cod-cli-000      thru vis-cod-cli-999        .
           perform   vis-cod-cli-rag-000  thru vis-cod-cli-rag-999    .
       acc-cod-cli-075.
      *                  *---------------------------------------------*
      *                  * Fine pre-accettazione                       *
      *                  *---------------------------------------------*
           go to     acc-cod-cli-100.
       acc-cod-cli-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-dcc-ope      .
           move      w-tes-cod-cli        to   w-cod-mne-dcc-cod      .
           move      04                   to   w-cod-mne-dcc-lin      .
           move      24                   to   w-cod-mne-dcc-pos      .
           move      04                   to   w-cod-mne-dcc-rln      .
           move      41                   to   w-cod-mne-dcc-rps      .
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
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-cli-999.
       acc-cod-cli-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-cli          .
       acc-cod-cli-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cod-cli-420.
      *                  *---------------------------------------------*
      *                  * Lettura file [cli]                          *
      *                  *---------------------------------------------*
           move      w-tes-cod-cli        to   w-let-arc-cli-cod      .
           perform   let-arc-cli-000      thru let-arc-cli-999        .
      *                  *---------------------------------------------*
      *                  * Lettura record [dcc]                        *
      *                  *---------------------------------------------*
           move      w-tes-cod-cli        to   w-let-arc-dcc-cli      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione ragione sociale              *
      *                  *---------------------------------------------*
           if        w-let-arc-dcc-flg    =    spaces
                     move  w-let-arc-dcc-rag
                                          to   w-tes-cod-cli-rag
           else      move  w-let-arc-cli-rag
                                          to   w-tes-cod-cli-rag      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione ragione sociale cliente     *
      *                  *---------------------------------------------*
           perform   vis-cod-cli-rag-000  thru vis-cod-cli-rag-999    .
       acc-cod-cli-440.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del valore impostato   *
      *                  *---------------------------------------------*
           if        w-tes-cod-cli        =    zero
                     go to acc-cod-cli-460
           else      go to acc-cod-cli-480.
       acc-cod-cli-460.
      *                  *---------------------------------------------*
      *                  * Se codice cliente impostato a zero          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-cod-cli-100.
       acc-cod-cli-480.
      *                  *---------------------------------------------*
      *                  * Se codice cliente impostato diverso da zero *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda se cliente commer- *
      *                      * ciale esistente oppure no               *
      *                      *-----------------------------------------*
           if        w-let-arc-dcc-flg    =    spaces
                     go to acc-cod-cli-560.
       acc-cod-cli-500.
      *                      *-----------------------------------------*
      *                      * Se anagrafica cliente commerciale non   *
      *                      * esistente                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione a seconda se anagrafica  *
      *                          * cliente esistente oppure no         *
      *                          *-------------------------------------*
           if        w-let-arc-cli-flg    =    spaces
                     go to acc-cod-cli-540.
       acc-cod-cli-520.
      *                          *-------------------------------------*
      *                          * Se anagrafica cliente contabile non *
      *                          * esistente                           *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * A reimpostazione                *
      *                              *---------------------------------*
           go to     acc-cod-cli-100.
       acc-cod-cli-540.
      *                          *-------------------------------------*
      *                          * Se anagrafica cliente contabile     *
      *                          * esistente                           *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Emissione messaggio di errore   *
      *                              *---------------------------------*
           move      "Manca l'anagrafica commerciale per il cliente !   
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                              *---------------------------------*
      *                              * A reimpostazione                *
      *                              *---------------------------------*
           go to     acc-cod-cli-100.
       acc-cod-cli-560.
      *                      *-----------------------------------------*
      *                      * Se anagrafica cliente commerciale esi-  *
      *                      * stente                                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A dipendenze da impostazione        *
      *                          *-------------------------------------*
           go to     acc-cod-cli-600.
       acc-cod-cli-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Aggiornamento valore di default generale    *
      *                  * per il codice cliente                       *
      *                  *---------------------------------------------*
           move      w-tes-cod-cli        to   w-def-cod-cli          .
       acc-cod-cli-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-cli-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-cli-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-cli-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-cli-999.
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
           move      04                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      "<B"                 to   v-edm                  .
           move      w-tes-cod-cli        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Ragione sociale cliente                 *
      *    *-----------------------------------------------------------*
       vis-cod-cli-rag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-cli-rag    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-cli-rag-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo riga corpo : Codice prodotto           *
      *    *-----------------------------------------------------------*
       acc-cod-pro-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuali valori provenienti da livello     *
      *                  * precedente                                  *
      *                  *---------------------------------------------*
           if        w-ipc-dlp-iom-pro    =    zero
                     go to acc-cod-pro-100.
      *                  *---------------------------------------------*
      *                  * Visualizzazione dei valori provenienti da   *
      *                  * livello precedente                          *
      *                  *---------------------------------------------*
           move      w-ipc-dlp-iom-pro    to   w-tes-num-pro          .
      *                      *-----------------------------------------*
      *                      * Lettura archivio [dcp]                  *
      *                      *-----------------------------------------*
           move      w-tes-num-pro        to   w-let-arc-dcp-cod      .
           perform   let-arc-dcp-000      thru let-arc-dcp-999        .
      *                      *-----------------------------------------*
      *                      * Memorizzazione valori letti             *
      *                      *-----------------------------------------*
           move      w-let-arc-dcp-des    to   w-tes-num-pro-des      .
           move      w-let-arc-dcp-alf    to   w-tes-num-pro-alf      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione codice prodotto         *
      *                      *-----------------------------------------*
           perform   vis-cod-pro-000      thru vis-cod-pro-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione descrizione prodotto    *
      *                      *-----------------------------------------*
           perform   vis-des-pro-000      thru vis-des-pro-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     acc-cod-pro-999.
       acc-cod-pro-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-dcp-ope      .
           move      "A"                  to   w-cod-cod-dcp-tac      .
           move      w-tes-num-pro        to   w-cod-cod-dcp-num      .
           move      w-tes-num-pro-alf    to   w-cod-cod-dcp-alf      .
           move      05                   to   w-cod-cod-dcp-lin      .
           move      24                   to   w-cod-cod-dcp-pos      .
           move      05                   to   w-cod-cod-dcp-dln      .
           move      41                   to   w-cod-cod-dcp-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
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
           move      w-cod-cod-dcp-alf    to   v-alf                  .
           move      w-cod-cod-dcp-num    to   v-num                  .
       acc-cod-pro-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-pro-999.
      *              *-------------------------------------------------*
      *              * Valore impostato                                *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-num-pro-alf      .
           move      v-num                to   w-tes-num-pro          .
       acc-cod-pro-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cod-pro-450.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [dcp]                      *
      *                  *---------------------------------------------*
           move      w-tes-num-pro        to   w-let-arc-dcp-cod      .
           perform   let-arc-dcp-000      thru let-arc-dcp-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione, prezzo listino, *
      *                  * sigla valuta, decimali valuta               *
      *                  *---------------------------------------------*
           move      w-let-arc-dcp-des    to   w-tes-num-pro-des      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione prodotto        *
      *                  *---------------------------------------------*
           perform   vis-des-pro-000      thru vis-des-pro-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-dcp-flg    not  = spaces
                     go to acc-cod-pro-100.
      *                  *---------------------------------------------*
      *                  * Se codice a zero : reimpostazione a meno    *
      *                  * non si sia premuto il tasto 'Up'            *
      *                  *---------------------------------------------*
           if        w-tes-num-pro        not  = zero
                     go to acc-cod-pro-600.
           if        v-key                =    "UP  "
                     go to acc-cod-pro-800
           else      go to acc-cod-pro-100.
       acc-cod-pro-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
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
      *    * Visualizzazione campo : Codice prodotto                   *
      *    *-----------------------------------------------------------*
       vis-cod-pro-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      w-tes-num-pro-alf    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-pro-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Descrizione prodotto              *
      *    *-----------------------------------------------------------*
       vis-des-pro-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-num-pro-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-pro-999.
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
                     move  "#"            to   w-cnt-sts-imp-tes      .
       acc-nok-reg-200.
      *              *-------------------------------------------------*
      *              * Trattamento testata                             *
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
      *                  * Prompts per testata                         *
      *                  *---------------------------------------------*
           if        w-cnt-sts-pmt-tes    =    spaces
                     perform pmt-tes-reg-000
                                          thru pmt-tes-reg-999
                     move    "#"          to   w-cnt-sts-pmt-tes      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione dati testata                *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "M"   or
                     w-cnt-mfu-tip-fun    =    "V"   or
                     w-cnt-sts-imp-tes    not  = spaces
                     if    w-cnt-sts-vis-tes
                                          =    spaces
                           perform vis-tes-reg-000
                                          thru vis-tes-reg-999
                           move    "#"    to   w-cnt-sts-vis-tes      .
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
       acc-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Accettazioni                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Codice prodotto per il cliente              *
      *                  *---------------------------------------------*
           perform   acc-cpr-cli-000      thru acc-cpr-cli-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-200.
      *                  *---------------------------------------------*
      *                  * Descrizione per il cliente                  *
      *                  *---------------------------------------------*
           perform   acc-dpr-cli-000      thru acc-dpr-cli-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-100.
       acc-tes-reg-900.
      *              *-------------------------------------------------*
      *              * Assestamento status di uscita                   *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     move   "S"           to   w-cnt-tus-acc-tes
           else if   v-key                =    "DELT"
                     move   "X"           to   w-cnt-tus-acc-tes
           else if   v-key                =    "EXIT"
                     move   "E"           to   w-cnt-tus-acc-tes
           else      move   "+"           to   w-cnt-tus-acc-tes      .
       acc-tes-reg-990.
      *              *-------------------------------------------------*
      *              * Flag di status impostazione testata             *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-tes      .
      *              *-------------------------------------------------*
      *              * Flag di status visualizzazione dati testata     *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-sts-vis-tes      .
       acc-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione testata registrazione                     *
      *    *-----------------------------------------------------------*
       vis-tes-reg-000.
      *              *-------------------------------------------------*
      *              * Codice prodotto per il cliente                  *
      *              *-------------------------------------------------*
           perform   vis-cpr-cli-000      thru vis-cpr-cli-999        .
      *              *-------------------------------------------------*
      *              * Descrizione per il cliente                      *
      *              *-------------------------------------------------*
           perform   vis-dpr-cli-000      thru vis-dpr-cli-999        .
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
      *              * Codice prodotto per il cliente                  *
      *              *-------------------------------------------------*
           perform   pmt-cpr-cli-000      thru pmt-cpr-cli-999        .
      *              *-------------------------------------------------*
      *              * Descrizione per il cliente                      *
      *              *-------------------------------------------------*
           perform   pmt-dpr-cli-000      thru pmt-dpr-cli-999        .
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice prodotto per il cliente   *
      *    *-----------------------------------------------------------*
       pmt-cpr-cli-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      22                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice prodotto per  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      22                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "         il cliente   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cpr-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Descrizione per il cliente       *
      *    *-----------------------------------------------------------*
       pmt-dpr-cli-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      22                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Descrizione prodotto :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      22                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "      per il cliente  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dpr-cli-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice prodotto per il cliente       *
      *    *-----------------------------------------------------------*
       acc-cpr-cli-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cpr-cli-100.
      *              *-------------------------------------------------*
      *              * Note operative                                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "Tasto funzione 'PF1' per copiare il codice anagraf
      -              "ico"                to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      w-acc-cpr-cli-cco    to   v-tip                  .
           move      w-acc-cpr-cli-lco    to   v-car                  .
           move      08                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "[1] "               to   v-pfk (14)             .
           move      w-tes-cpr-cli (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Cancellazione eventuali note operative          *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cpr-cli-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cpr-cli-999.
       acc-cpr-cli-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-cpr-cli (1)      .
       acc-cpr-cli-300.
      *              *-------------------------------------------------*
      *              * Se Pf1                                          *
      *              *-------------------------------------------------*
           if        v-key                not  = "[1] "
                     go to acc-cpr-cli-400.
      *                  *---------------------------------------------*
      *                  * Codice del prodotto in anagrafica           *
      *                  *---------------------------------------------*
           move      w-tes-num-pro-alf    to   w-tes-cpr-cli (1)      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione valore                      *
      *                  *---------------------------------------------*
           perform   vis-cpr-cli-000      thru vis-cpr-cli-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-cpr-cli-100.
       acc-cpr-cli-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      w-tes-cpr-cli (1)    to   w-all-str-alf          .
           move      05                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-cpr-cli-100.
       acc-cpr-cli-450.
      *                  *---------------------------------------------*
      *                  * Test che il valore non sia totalmente ugua- *
      *                  * le a quello in anagrafica [dcp]             *
      *                  *---------------------------------------------*
           if        w-tes-cpr-cli (1)    =    spaces
                     go to acc-cpr-cli-600.
           if        w-tes-cpr-cli (1)    not  = w-tes-num-pro-alf
                     go to acc-cpr-cli-600.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Codice per il cliente uguale a quello in anagrafic
      -              "a !  "              to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-cpr-cli-100.
       acc-cpr-cli-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cpr-cli-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cpr-cli-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cpr-cli-100.
       acc-cpr-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice prodotto per il cliente    *
      *    *-----------------------------------------------------------*
       vis-cpr-cli-000.
           move      "DS"                 to   v-ope                  .
           move      w-acc-cpr-cli-cco    to   v-tip                  .
           move      w-acc-cpr-cli-lco    to   v-car                  .
           move      08                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      w-tes-cpr-cli (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cpr-cli-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Descrizione per il cliente   *
      *    *-----------------------------------------------------------*
       acc-dpr-cli-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dpr-cli-100.
      *              *-------------------------------------------------*
      *              * Note operative                                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "Tasto funzione 'PF1' per copiare la descrizione an
      -              "agrafica"           to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
      *
           if        w-acc-dpr-cli-rde    =    1
                     move  "A"            to   v-tip
                     move  w-tes-dpr-cli (1)
                                          to   v-alf
           else      move  "T"            to   v-tip
                     move  w-tes-dpr-cli (1)
                                          to   v-txt                  .
      *
           move      w-acc-dpr-cli-lde    to   v-car                  .
           move      10                   to   v-ldt                  .
           move      11                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "[1] "               to   v-pfk (14)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Cancellazione eventuali note operative          *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dpr-cli-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dpr-cli-999.
       acc-dpr-cli-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        w-acc-dpr-cli-rde    =    1
                     move  v-alf          to   w-tes-dpr-cli (1)
           else      move  v-txt          to   w-tes-dpr-cli (1)      .
       acc-dpr-cli-300.
      *              *-------------------------------------------------*
      *              * Se Pf1                                          *
      *              *-------------------------------------------------*
           if        v-key                not  = "[1] "
                     go to acc-dpr-cli-400.
      *                  *---------------------------------------------*
      *                  * Descrizione del prodotto in anagrafica      *
      *                  *---------------------------------------------*
           move      w-tes-num-pro-des    to   w-tes-dpr-cli (1)      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione valore                      *
      *                  *---------------------------------------------*
           perform   vis-dpr-cli-000      thru vis-dpr-cli-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-dpr-cli-100.
       acc-dpr-cli-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che il valore non sia totalmente ugua- *
      *                  * le alla descrizione del prodotto in anagra- *
      *                  * fica [dcp]                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se a spaces                        *
      *                      *-----------------------------------------*
           if        w-tes-dpr-cli (1)    =    spaces
                     go to acc-dpr-cli-600.
           if        w-tes-dpr-cli (1)    not  = w-tes-num-pro-des
                     go to acc-dpr-cli-470.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Descrizione per il cliente uguale a quella del pro
      -              "dotto !"            to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-dpr-cli-100.
       acc-dpr-cli-470.
      *                  *---------------------------------------------*
      *                  * Compattamento verso l'alto delle righe di   *
      *                  * descrizione                                 *
      *                  *---------------------------------------------*
           perform   cmp-rig-dep-000      thru cmp-rig-dep-999        .
      *                  *---------------------------------------------*
      *                  * Se compattamento avvenuto : reimpostazione  *
      *                  *---------------------------------------------*
           if        w-cmp-rig-dep-flg    not  = spaces
                     go to acc-dpr-cli-100.
       acc-dpr-cli-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Si rivisualizza il valore impostato in caso *
      *                  * di presenza di righe oltre alla prima       *
      *                  *---------------------------------------------*
           if        w-tes-dpr-rig (1, 02)
                                          =    spaces and
                     w-tes-dpr-rig (1, 03)
                                          =    spaces and
                     w-tes-dpr-rig (1, 04)
                                          =    spaces and
                     w-tes-dpr-rig (1, 05)
                                          =    spaces and
                     w-tes-dpr-rig (1, 06)
                                          =    spaces and
                     w-tes-dpr-rig (1, 07)
                                          =    spaces and
                     w-tes-dpr-rig (1, 08)
                                          =    spaces and
                     w-tes-dpr-rig (1, 09)
                                          =    spaces and
                     w-tes-dpr-rig (1, 10)
                                          =    spaces
                     go to acc-dpr-cli-800.
      *                  *---------------------------------------------*
      *                  * Visualizzazione della descrizione           *
      *                  *---------------------------------------------*
           perform   vis-dpr-cli-000      thru vis-dpr-cli-999        .
       acc-dpr-cli-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dpr-cli-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dpr-cli-100.
       acc-dpr-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione per cliente   *
      *    *-----------------------------------------------------------*
       vis-dpr-cli-000.
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Ciclo di visualizzazione di 10 righe            *
      *              *-------------------------------------------------*
           move      zero                 to   w-cix-ctr-002          .
       vis-dpr-cli-100.
           add       1                    to   w-cix-ctr-002          .
           if        w-cix-ctr-002        >    10
                     go to vis-dpr-cli-900.
       vis-dpr-cli-200.
      *                  *---------------------------------------------*
      *                  * Visualizzazione singola riga                *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      w-acc-dpr-cli-lde    to   v-car                  .
           move      10                   to   v-lin                  .
           add       w-cix-ctr-002        to   v-lin                  .
           move      24                   to   v-pos                  .
           move      w-tes-dpr-rig
                    (1, w-cix-ctr-002)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dpr-cli-800.
      *                  *---------------------------------------------*
      *                  * Riciclo a riga successiva                   *
      *                  *---------------------------------------------*
           go to     vis-dpr-cli-100.
       vis-dpr-cli-900.
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-dpr-cli-999.
       vis-dpr-cli-999.
           exit.

      *    *===========================================================*
      *    * Routine di compattamento righe della descrizione prodotto *
      *    *-----------------------------------------------------------*
       cmp-rig-dep-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di rivisualizzazione de-   *
      *              * scrizione prodotto                              *
      *              *-------------------------------------------------*
           move      spaces               to   w-cmp-rig-dep-flg      .
      *              *-------------------------------------------------*
      *              * Determinazione numero righe attuale             *
      *              *-------------------------------------------------*
           move      11                   to   w-cmp-rig-dep-nri      .
       cmp-rig-dep-100.
           subtract  1                    from w-cmp-rig-dep-nri      .
           if        w-tes-dpr-rig
                    (1, w-cmp-rig-dep-nri)
                                          =    spaces
                     go to cmp-rig-dep-100.
      *              *-------------------------------------------------*
      *              * Inizializzazione numero righe descrizione       *
      *              *-------------------------------------------------*
           move      zero                 to   w-cmp-rig-dep-nrd      .
      *              *-------------------------------------------------*
      *              * Ciclo per compattamento                         *
      *              *-------------------------------------------------*
           move      zero                 to   w-cmp-rig-dep-ctr      .
       cmp-rig-dep-200.
           add       1                    to   w-cmp-rig-dep-ctr      .
           if        w-cmp-rig-dep-ctr    >    w-cmp-rig-dep-nri
                     go to cmp-rig-dep-300.
      *                  *---------------------------------------------*
      *                  * Se riga di descrizione a spazi : riciclo    *
      *                  *---------------------------------------------*
           if        w-tes-dpr-rig
                    (1, w-cmp-rig-dep-ctr)
                                          =    spaces
                     go to cmp-rig-dep-200.
      *                  *---------------------------------------------*
      *                  * Incremento contatore numero righe           *
      *                  *---------------------------------------------*
           add       1                    to   w-cmp-rig-dep-nrd      .
      *                  *---------------------------------------------*
      *                  * Spostamento riga di descrizione             *
      *                  *---------------------------------------------*
           move      w-tes-dpr-rig
                    (1, w-cmp-rig-dep-ctr)
                                          to   w-tes-dpr-rig
                                              (1, w-cmp-rig-dep-nrd)  .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     cmp-rig-dep-200.
       cmp-rig-dep-300.
      *              *-------------------------------------------------*
      *              * Abblencamento righe residue                     *
      *              *-------------------------------------------------*
           move      w-cmp-rig-dep-nrd    to   w-cmp-rig-dep-ctr      .
       cmp-rig-dep-320.
           add       1                    to   w-cmp-rig-dep-ctr      .
           if        w-cmp-rig-dep-ctr    >    w-cmp-rig-dep-nri
                     go to cmp-rig-dep-900.
           move      spaces               to   w-tes-dpr-rig
                                              (1, w-cmp-rig-dep-ctr)  .
           go to     cmp-rig-dep-320.
       cmp-rig-dep-900.
      *              *-------------------------------------------------*
      *              * Determinazione del flag di rivisualizzazione    *
      *              *-------------------------------------------------*
           if        w-cmp-rig-dep-nri    not  = w-cmp-rig-dep-nrd
                     move  "#"            to   w-cmp-rig-dep-flg      .
       cmp-rig-dep-999.
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
      *              * Controllo su codice prodotto                    *
      *              *-------------------------------------------------*
           if        w-tes-num-pro        not  = zero
                     go to cnt-tdo-key-500.
           move      "Manca il codice prodotto !                       "
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-key-900.
       cnt-tdo-key-500.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per accettazioni         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valori di default                           *
      *                  *---------------------------------------------*
           move      w-ref-prm-acc-lco (1)
                                          to   w-acc-cpr-cli-lco      .
           move      w-ref-prm-acc-cco (1)
                                          to   w-acc-cpr-cli-cco      .
           move      w-ref-prm-acc-rde (1)
                                          to   w-acc-dpr-cli-rde      .
           move      w-ref-prm-acc-lde (1)
                                          to   w-acc-dpr-cli-lde      .
      *                  *---------------------------------------------*
      *                  * Ricerca in base al codice cliente           *
      *                  *---------------------------------------------*
           set       w-ref-prm-acc-inx    to   1                      .
           search    w-ref-prm-acc-ele
                     when    w-ref-prm-acc-cli
                            (w-ref-prm-acc-inx)
                                          =    w-tes-cod-cli
                     go to   cnt-tdo-key-520.
           go to     cnt-tdo-key-800.
       cnt-tdo-key-520.
      *                  *---------------------------------------------*
      *                  * Se codice cliente nelle referenze           *
      *                  *---------------------------------------------*
           move      w-ref-prm-acc-lco
                    (w-ref-prm-acc-inx)   to   w-acc-cpr-cli-lco      .
           move      w-ref-prm-acc-cco
                    (w-ref-prm-acc-inx)   to   w-acc-cpr-cli-cco      .
           move      w-ref-prm-acc-rde
                    (w-ref-prm-acc-inx)   to   w-acc-dpr-cli-rde      .
           move      w-ref-prm-acc-lde
                    (w-ref-prm-acc-inx)   to   w-acc-dpr-cli-lde      .
       cnt-tdo-key-800.
      *              *-------------------------------------------------*
      *              * Uscita per controlli tutti superati             *
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
      *              *-------------------------------------------------*
      *              * Test                                            *
      *              *-------------------------------------------------*
           if        w-tes-cod-cli        =    zero and
                     w-tes-num-pro        =    zero
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
       cnt-tdo-nok-010.
      *              *-------------------------------------------------*
      *              * Controlli su codice prodotto per il cliente     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che il valore non sia totalmente ugua- *
      *                  * le al codice del prodotto in anagrafica     *
      *                  *---------------------------------------------*
           if        w-tes-cpr-cli (1)    =    spaces
                     go to cnt-tdo-nok-100.
           if        w-tes-cpr-cli (1)    not  = w-tes-num-pro-alf
                     go to cnt-tdo-nok-100.
           move      "Codice per il cliente uguale a quello in anagrafic
      -              "a !"                to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-100.
      *              *-------------------------------------------------*
      *              * Controlli su Descrizione per il cliente         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che il valore non sia totalmente ugua- *
      *                  * le alla descrizione del prodotto in anagra- *
      *                  * fica [dcp]                                  *
      *                  *---------------------------------------------*
           if        w-tes-dpr-cli (1)    =    spaces
                     go to cnt-tdo-nok-300.
           if        w-tes-dpr-cli (1)    not  = w-tes-num-pro-des
                     go to cnt-tdo-nok-300.
           move      "Descrizione per il cliente uguale a quella del pro
      -              "dotto !"            to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-300.
      *              *-------------------------------------------------*
      *              * Controllo che non manchino entrambi             *
      *              *-------------------------------------------------*
           if        w-tes-cpr-cli (1)    not  = spaces or
                     w-tes-dpr-cli (1)    not  = spaces
                     go to cnt-tdo-nok-800.
           move      "Non possono mancare entrambi i valori !           
      -              "       "            to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
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
           move      zero                 to   w-tes-cod-cli          .
           move      spaces               to   w-tes-cod-cli-rag      .
           move      zero                 to   w-tes-num-pro          .
           move      spaces               to   w-tes-num-pro-alf      .
           move      spaces               to   w-tes-num-pro-des      .
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *-----------------------------------------------------------*
       nor-nok-tes-000.
           move      spaces               to   w-tes-cpr-cli (1)      .
           move      spaces               to   w-tes-dpr-cli (1)      .
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
      *              * Normalizzazioni status per le letture           *
      *              *-------------------------------------------------*
           move      spaces               to   w-sav-rou-let-stc      .
           move      spaces               to   w-sav-rou-let-cpr      .
           move      spaces               to   w-sav-rou-let-std      .
       rou-let-reg-010.
      *              *-------------------------------------------------*
      *              * Lettura per il codice prodotto per il cliente   *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "TRCLNG    "         to   f-key                  .
           move      05                   to   rf-pdx-tip-rec         .
           move      w-tes-cod-cli        to   rf-pdx-cod-arc         .
           move      spaces               to   rf-pdx-cod-lng         .
           move      w-tes-num-pro        to   rf-pdx-cod-num         .
           move      spaces               to   rf-pdx-for-mat         .
           move      01                   to   rf-pdx-num-prg         .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-sav-rou-let-stc
           else      move  rf-pdx-des-pro to   w-sav-rou-let-cpr      .
       rou-let-reg-020.
      *              *-------------------------------------------------*
      *              * Lettura per descrizione prodotto per il cliente *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "TRCLNG    "         to   f-key                  .
           move      03                   to   rf-pdx-tip-rec         .
           move      w-tes-cod-cli        to   rf-pdx-cod-arc         .
           move      spaces               to   rf-pdx-cod-lng         .
           move      w-tes-num-pro        to   rf-pdx-cod-num         .
           move      spaces               to   rf-pdx-for-mat         .
           move      01                   to   rf-pdx-num-prg         .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-sav-rou-let-std      .
       rou-let-reg-100.
      *              *-------------------------------------------------*
      *              * Confronto tra status di lettura                 *
      *              *-------------------------------------------------*
           if        w-sav-rou-let-stc    not  = spaces and
                     w-sav-rou-let-std    not  = spaces
                     go to rou-let-reg-210
           else      go to rou-let-reg-220.
       rou-let-reg-200.
      *              *-------------------------------------------------*
      *              * Definizione del tipo di funzionamento           *
      *              *-------------------------------------------------*
       rou-let-reg-210.
      *                  *---------------------------------------------*
      *                  * Test se inserimento consentito              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-cnt-mfu-vis-sgr    not  = "V"
                     go to rou-let-reg-215.
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
       rou-let-reg-215.
      *                  *---------------------------------------------*
      *                  * Tipo funzionamento : Inserimento            *
      *                  *---------------------------------------------*
           move      "I"                  to   w-cnt-mfu-tip-fun      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rou-let-reg-999.
       rou-let-reg-220.
      *                  *---------------------------------------------*
      *                  * Tipo funzionamento : Modifica               *
      *                  *---------------------------------------------*
           move      "M"                  to   w-cnt-mfu-tip-fun      .
       rou-let-reg-300.
      *              *-------------------------------------------------*
      *              * Determinazione valori attuali                   *
      *              *-------------------------------------------------*
       rou-let-reg-310.
      *                  *---------------------------------------------*
      *                  * Per il codice prodotto per il cliente       *
      *                  *---------------------------------------------*
           if        w-sav-rou-let-stc    not  = spaces
                     go to rou-let-reg-500.
      *                  *---------------------------------------------*
      *                  * Codice prodotto per il cliente salvato      *
      *                  *---------------------------------------------*
           move      w-sav-rou-let-cpr    to   w-tes-cpr-cli (1)      .
       rou-let-reg-500.
      *                  *---------------------------------------------*
      *                  * Per la descrizione prodotto per il cliente  *
      *                  *---------------------------------------------*
           if        w-sav-rou-let-std    not  = spaces
                     go to rou-let-reg-800.
      *                      *-----------------------------------------*
      *                      * Start su file [pdx]                     *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "TRCLNG    "         to   f-key                  .
           move      03                   to   rf-pdx-tip-rec         .
           move      w-tes-cod-cli        to   rf-pdx-cod-arc         .
           move      spaces               to   rf-pdx-cod-lng         .
           move      w-tes-num-pro        to   rf-pdx-cod-num         .
           move      spaces               to   rf-pdx-for-mat         .
           move      zero                 to   rf-pdx-num-prg         .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                          *-------------------------------------*
      *                          * Test su esito operazione            *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rou-let-reg-800.
       rou-let-reg-520.
      *                      *-----------------------------------------*
      *                      * Lettura sequenziale file [pdx]          *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                          *-------------------------------------*
      *                          * Test se 'at end'                    *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rou-let-reg-800.
       rou-let-reg-530.
      *                      *-----------------------------------------*
      *                      * Test sul massimo                        *
      *                      *-----------------------------------------*
           if        rf-pdx-tip-rec       not  = 03            or
                     rf-pdx-cod-arc       not  = w-tes-cod-cli or
                     rf-pdx-cod-lng       not  = spaces        or
                     rf-pdx-cod-num       not  = w-tes-num-pro or
                     rf-pdx-for-mat       not  = spaces
                     go to rou-let-reg-800.
       rou-let-reg-540.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione descrizione             *
      *                      *-----------------------------------------*
           move      rf-pdx-des-pro       to   w-tes-dpr-rig
                                              (1, rf-pdx-num-prg)     .
      *                      *-----------------------------------------*
      *                      * Riciclo su lettura                      *
      *                      *-----------------------------------------*
           go to     rou-let-reg-520.
       rou-let-reg-800.
      *                      *-----------------------------------------*
      *                      * Valori precedenti anagrafica            *
      *                      *-----------------------------------------*
           move      w-tes-val-aep (1)    to   w-tes-val-aep (2)      .
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
      *              * Trattamento file [pdx] per codice prodotto per  *
      *              * il cliente                                      *
      *              *-------------------------------------------------*
           perform   scr-mov-fil-cpr-000  thru scr-mov-fil-cpr-999    .
      *              *-------------------------------------------------*
      *              * Trattamento file [pdx] per descrizione prodotto *
      *              * per il cliente                                  *
      *              *-------------------------------------------------*
           perform   scr-mov-fil-dpr-000  thru scr-mov-fil-dpr-999    .
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *-----------------------------------------------------------*
       del-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Trattamento file [pdx] per codice prodotto per  *
      *              * il cliente                                      *
      *              *-------------------------------------------------*
           perform   del-rec-pdx-cpr-000  thru del-rec-pdx-cpr-999    .
      *              *-------------------------------------------------*
      *              * Trattamento file [pdx] per descrizione prodotto *
      *              * per il cliente                                  *
      *              *-------------------------------------------------*
           perform   del-rec-pdx-dpr-000  thru del-rec-pdx-dpr-999    .
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Scrittura movimento su file                               *
      *    *                                                           *
      *    * Subroutine per il trattamento del codice prodotto per il  *
      *    * cliente                                                   *
      *    *-----------------------------------------------------------*
       scr-mov-fil-cpr-000.
      *              *-------------------------------------------------*
      *              * Trattamento file [pdx]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Cancellazione del record precedente         *
      *                  *---------------------------------------------*
           perform   del-rec-pdx-cpr-000  thru del-rec-pdx-cpr-999    .
      *                  *---------------------------------------------*
      *                  * Scrittura eventuale del record nuovo        *
      *                  *---------------------------------------------*
           if        w-tes-cpr-cli (1)    =    spaces
                     go to scr-mov-fil-cpr-900.
      *                  *---------------------------------------------*
      *                  * Scrittura del record nuovo                  *
      *                  *---------------------------------------------*
           perform   wrt-rec-pdx-cpr-000  thru wrt-rec-pdx-cpr-999    .
       scr-mov-fil-cpr-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     scr-mov-fil-cpr-999.
       scr-mov-fil-cpr-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [pdx]                                 *
      *    *                                                           *
      *    * Subroutine per il trattamento del codice prodotto per il  *
      *    * cliente                                                   *
      *    *-----------------------------------------------------------*
       cmp-rec-pdx-cpr-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      05                   to   rf-pdx-tip-rec         .
           move      w-tes-cod-cli        to   rf-pdx-cod-arc         .
           move      spaces               to   rf-pdx-cod-lng         .
           move      w-tes-num-pro        to   rf-pdx-cod-num         .
           move      spaces               to   rf-pdx-for-mat         .
           move      01                   to   rf-pdx-num-prg         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      w-tes-cpr-cli (1)    to   rf-pdx-des-pro         .
       cmp-rec-pdx-cpr-200.
      *              *-------------------------------------------------*
      *              * Trattamento [pdk] per chiave                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdk                 .
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      "C"                  to   rf-pdk-tip-rec         .
           move      w-tes-cod-cli        to   rf-pdk-cod-arc         .
           move      w-tes-num-pro        to   rf-pdk-num-pro         .
           move      w-tes-cpr-cli (1)    to   rf-pdk-alf-pro         .
           move      spaces               to   rf-pdk-alx-exp         .
       cmp-rec-pdx-cpr-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [pdx]                                    *
      *    *                                                           *
      *    * Subroutine per il trattamento del codice prodotto per il  *
      *    * cliente                                                   *
      *    *-----------------------------------------------------------*
       wrt-rec-pdx-cpr-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-pdx-cpr-000  thru cmp-rec-pdx-cpr-999    .
      *              *-------------------------------------------------*
      *              * Put record [pdx]                                *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *              *-------------------------------------------------*
      *              * Put record [pdk]                                *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdk                 .
       wrt-rec-pdx-cpr-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [pdx]                                *
      *    *                                                           *
      *    * Subroutine per il trattamento del codice prodotto per il  *
      *    * cliente                                                   *
      *    *-----------------------------------------------------------*
       del-rec-pdx-cpr-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-pdx-cpr-000  thru cmp-rec-pdx-cpr-999    .
      *              *-------------------------------------------------*
      *              * Delete record [pdx]                             *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *              *-------------------------------------------------*
      *              * Delete record [pdk]                             *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdk                 .
       del-rec-pdx-cpr-999.
           exit.

      *    *===========================================================*
      *    * Scrittura movimento su file                               *
      *    *                                                           *
      *    * Subroutine per il trattamento della descrizione prodotto  *
      *    * per il cliente                                            *
      *    *-----------------------------------------------------------*
       scr-mov-fil-dpr-000.
      *              *-------------------------------------------------*
      *              * Trattamento file [pdx]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Cancellazione del record precedente         *
      *                  *---------------------------------------------*
           perform   del-rec-pdx-dpr-000  thru del-rec-pdx-dpr-999    .
      *                  *---------------------------------------------*
      *                  * Scrittura eventuale del record nuovo        *
      *                  *---------------------------------------------*
           if        w-tes-dpr-cli (1)    =    spaces
                     go to scr-mov-fil-dpr-900.
      *                  *---------------------------------------------*
      *                  * Scrittura del record nuovo                  *
      *                  *---------------------------------------------*
           perform   wrt-rec-pdx-dpr-000  thru wrt-rec-pdx-dpr-999    .
       scr-mov-fil-dpr-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     scr-mov-fil-dpr-999.
       scr-mov-fil-dpr-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [pdx]                                 *
      *    *                                                           *
      *    * Subroutine per il trattamento della descrizione prodotto  *
      *    * per il cliente                                            *
      *    *-----------------------------------------------------------*
       cmp-rec-pdx-dpr-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      03                   to   rf-pdx-tip-rec         .
           move      w-tes-cod-cli        to   rf-pdx-cod-arc         .
           move      spaces               to   rf-pdx-cod-lng         .
           move      w-tes-num-pro        to   rf-pdx-cod-num         .
           move      spaces               to   rf-pdx-for-mat         .
           move      w-cix-ctr-001        to   rf-pdx-num-prg         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      w-tes-dpr-rig
                    (1, w-cix-ctr-001)    to   rf-pdx-des-pro         .
       cmp-rec-pdx-dpr-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [pdx]                                    *
      *    *                                                           *
      *    * Subroutine per il trattamento della descrizione prodotto  *
      *    * per il cliente                                            *
      *    *-----------------------------------------------------------*
       wrt-rec-pdx-dpr-000.
      *              *-------------------------------------------------*
      *              * Ciclo per scrittura fino a 10 righe di descri-  *
      *              * zione prodotto                                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-cix-ctr-001          .
       wrt-rec-pdx-dpr-100.
           add       1                    to   w-cix-ctr-001          .
           if        w-cix-ctr-001        >    10
                     go to wrt-rec-pdx-dpr-999.
           if        w-tes-dpr-rig
                    (1, w-cix-ctr-001)    =    spaces
                     go to wrt-rec-pdx-dpr-500.
      *                  *---------------------------------------------*
      *                  * Composizione record [pdx]                   *
      *                  *---------------------------------------------*
           perform   cmp-rec-pdx-dpr-000      thru cmp-rec-pdx-dpr-999        .
      *                  *---------------------------------------------*
      *                  * Put record                                  *
      *                  *---------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     wrt-rec-pdx-dpr-100.
       wrt-rec-pdx-dpr-500.
      *              *-------------------------------------------------*
      *              * Riposizionamento contatore                      *
      *              *-------------------------------------------------*
           subtract  1                    from w-cix-ctr-001          .
      *              *-------------------------------------------------*
      *              * Ciclo per cancellazione righe di descrizione    *
      *              * prodotto rimanenti                              *
      *              *-------------------------------------------------*
       wrt-rec-pdx-dpr-520.
           add       1                    to   w-cix-ctr-001          .
           if        w-cix-ctr-001        >    10
                     go to wrt-rec-pdx-dpr-999.
      *                  *---------------------------------------------*
      *                  * Composizione campi chiave record            *
      *                  *---------------------------------------------*
           move      03                   to   rf-pdx-tip-rec         .
           move      w-tes-cod-cli        to   rf-pdx-cod-arc         .
           move      spaces               to   rf-pdx-cod-lng         .
           move      w-tes-num-pro        to   rf-pdx-cod-num         .
           move      spaces               to   rf-pdx-for-mat         .
           move      w-cix-ctr-001        to   rf-pdx-num-prg         .
      *                  *---------------------------------------------*
      *                  * Delete record                               *
      *                  *---------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     wrt-rec-pdx-dpr-520.
       wrt-rec-pdx-dpr-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [pdx]                                *
      *    *                                                           *
      *    * Subroutine per il trattamento della descrizione prodotto  *
      *    * per il cliente                                            *
      *    *-----------------------------------------------------------*
       del-rec-pdx-dpr-000.
      *              *-------------------------------------------------*
      *              * Ciclo per cancellazione fino a 10 righe di de-  *
      *              * scrizione prodotto                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-cix-ctr-001          .
       del-rec-pdx-dpr-100.
           add       1                    to   w-cix-ctr-001          .
           if        w-cix-ctr-001        >    10
                     go to del-rec-pdx-dpr-999.
      *                  *---------------------------------------------*
      *                  * Composizione record [pdx]                   *
      *                  *---------------------------------------------*
           perform   cmp-rec-pdx-dpr-000      thru cmp-rec-pdx-dpr-999        .
      *                  *---------------------------------------------*
      *                  * Delete record                               *
      *                  *---------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     del-rec-pdx-dpr-100.
       del-rec-pdx-dpr-999.
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
      *    * Routine di lettura archivio [dcc]                         *
      *    *-----------------------------------------------------------*
       let-arc-dcc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
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
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dcc-999.
       let-arc-dcc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dcc-flg      .
           move      all"."               to   w-let-arc-dcc-rag      .
           go to     let-arc-dcc-999.
       let-arc-dcc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcc-rag      .
       let-arc-dcc-999.
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
           if        w-let-arc-dcp-cod    =    zero  
                     go to let-arc-dcp-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO"             to   f-key                  .
           move      w-let-arc-dcp-cod    to   rf-dcp-num-pro         .
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
           move      rf-dcp-des-pro       to   w-let-arc-dcp-des      .
           move      rf-dcp-alf-pro       to   w-let-arc-dcp-alf      .
           move      rf-dcp-sgl-vlt       to   w-let-arc-dcp-sgv      .
           move      rf-dcp-dec-vlt       to   w-let-arc-dcp-dcv      .
           move      rf-dcp-prz-lst       to   w-let-arc-dcp-prz      .
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
           move      spaces               to   w-let-arc-dcp-sgv      .
           move      zero                 to   w-let-arc-dcp-dcv      .
           move      zero                 to   w-let-arc-dcp-prz      .
       let-arc-dcp-999.
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
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice cliente commerciale *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmndcc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per accettazione codice prodotto 'dcp'        *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acoddcp0.acs"                   .

