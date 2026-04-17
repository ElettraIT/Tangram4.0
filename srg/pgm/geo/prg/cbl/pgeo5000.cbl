       Identification Division.
       Program-Id.                                 pgeo5000           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    geo                 *
      *                                Settore:    str                 *
      *                                   Fase:    geo500              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 22/03/91    *
      *                       Ultima revisione:    NdK del 14/11/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione tabella Stradari                   *
      *                                                                *
      *================================================================*
      *                                                                *
      * Citta' divise in zone postali (agg. 26/10/06)                  *
      *                                                                *
      * Ancona              La Spezia            Pisa                  *
      * Bari                Livorno              Reggio Calabria       *
      * Bergamo             Messina              Roma                  *
      * Bologna             Milano               Salerno               *
      * Brescia             Napoli               Torino                *
      * Cagliari            Padova               Trieste               *
      * Catania             Palermo              Venezia               *
      * Firenze             Perugia              Verbania              *
      * Genova              Pescara              Verona                *
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
                     "geo"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "str"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "geo500"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pgeo5000"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "            TABELLA STRADARI            "       .

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
      *        *-------------------------------------------------------*
      *        * Area di controllo per duplicazione record             *
      *        *-------------------------------------------------------*
           05  w-cnt-dup.
               10  w-cnt-dup-rec-flg      pic  x(01)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [gxs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/geo/fls/rec/rfgxs"                          .
      *        *-------------------------------------------------------*
      *        * [gxc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/geo/fls/rec/rfgxc"                          .

      *    *===========================================================*
      *    * Work-area per bufferizzazione testata                     *
      *    *-----------------------------------------------------------*
       01  w-tes.
      *        *-------------------------------------------------------*
      *        * Valori chiave                                         *
      *        *-------------------------------------------------------*
           05  w-tes-val-key.
               10  w-tes-sgl-std          pic  x(05)                  .
               10  w-tes-spc-key          pic  x(30)                  .
               10  w-tes-des-key          pic  x(30)                  .
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
               10  w-tes-des-str          pic  x(30)                  .
               10  w-tes-cap-avp          pic  x(05)                  .
               10  w-tes-cod-cmn          pic  9(05)                  .
               10  w-tes-cod-cmn-des      pic  x(30)                  .
               10  w-tes-cod-cmn-prv      pic  x(02)                  .
               10  w-tes-cod-cmn-cap      pic  x(05)                  .
               10  w-tes-cod-lct          pic  9(03)                  .
               10  w-tes-cod-lct-des      pic  x(30)                  .
               10  w-tes-cod-fzn          pic  9(03)                  .
               10  w-tes-cod-fzn-des      pic  x(30)                  .
               10  w-tes-alx-exp.
                   15  filler occurs 20   pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per valori di defaults generali                 *
      *    *-----------------------------------------------------------*
       01  w-def.
      *        *-------------------------------------------------------*
      *        * Sigla per lo stradario                                *
      *        *-------------------------------------------------------*
           05  w-def-sgl-std              pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Codice comune                                         *
      *        *-------------------------------------------------------*
           05  w-def-cod-cmn              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Codice frazione                                       *
      *        *-------------------------------------------------------*
           05  w-def-cod-fzn              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Codice localita'                                      *
      *        *-------------------------------------------------------*
           05  w-def-cod-lct              pic  9(03)                  .

      *    *===========================================================*
      *    * Work per subroutines di Find                              *
      *    *-----------------------------------------------------------*
       01  w-fnd.
      *        *-------------------------------------------------------*
      *        * Work per Find su archivio [gxs]                       *
      *        *-------------------------------------------------------*
           05  w-fnd-arc-gxs.
               10  w-fnd-arc-gxs-sel      pic  x(01)                  .
               10  w-fnd-arc-gxs-sgl      pic  x(05)                  .
               10  w-fnd-arc-gxs-key      pic  x(30)                  .
               10  w-fnd-arc-gxs-spc      pic  x(30)                  .
               
      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [gxc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-gxc.
               10  w-let-arc-gxc-flg      pic  x(01)                  .
               10  w-let-arc-gxc-tip      pic  x(01)                  .
               10  w-let-arc-gxc-cmn      pic  9(05)                  .
               10  w-let-arc-gxc-fzn      pic  9(03)                  .
               10  w-let-arc-gxc-lct      pic  9(03)                  .
               10  w-let-arc-gxc-des      pic  x(30)                  .
               10  w-let-arc-gxc-prv      pic  x(02)                  .
               10  w-let-arc-gxc-cap      pic  x(05)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Box di espansione per errore su provincia  *
      *        *-------------------------------------------------------*
           05  w-exp-box-err.
               10  w-exp-box-err-num      pic  9(02)       value 02   .
               10  w-exp-box-err-lun      pic  9(02)       value 25   .
               10  w-exp-box-err-tbl.
                   15  filler             pic  x(25) value
                            "Forzatura del CAP        "               .
                   15  filler             pic  x(25) value
                            "Reimpostazione           "               .

      *    *===========================================================*
      *    * Work area per Controlli                                   *
      *    *-----------------------------------------------------------*
       01  w-ctl.
      *        *-------------------------------------------------------*
      *        * Work per controllo se CAP numerico                    *
      *        *-------------------------------------------------------*
           05  w-ctl-cap-avp.
               10  w-ctl-cap-avp-flg      pic  x(01)                  .
               10  w-ctl-cap-avp-ctr      pic  9(02)                  .
               10  w-ctl-cap-avp-alf.
                   15  w-ctl-cap-avp-chr  occurs 5
                                          pic  x(01)                  .
               10  w-ctl-cap-avp-aln      pic  x(05)                  .
               10  w-ctl-cap-avp-alr redefines
                   w-ctl-cap-avp-aln      pic  9(05)                  .

      *    *===========================================================*
      *    * Work per salvataggi valori precedenti                     *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Work per salvataggio Cap ufficiale + estensione       *
      *        *-------------------------------------------------------*
           05  w-sav-cod-cmn              pic  9(05)                  .
           05  w-sav-cod-fzn              pic  9(03)                  .

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
      *    * Work area di comodo                                       *
      *    *-----------------------------------------------------------*
       01  w-wrk.
      *        *-------------------------------------------------------*
      *        * Work per controllo congruenza del CAP                 *
      *        *-------------------------------------------------------*
           05  w-wrk-cap-cmn.
               10  w-wrk-cmn-002          pic  x(02)                  .
               10  w-wrk-cmn-003          pic  x(03)                  .
           05  w-wrk-cap-str.
               10  w-wrk-str-002          pic  x(02)                  .
               10  w-wrk-str-003          pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per accettazione conferma box di errore          *
      *        *-------------------------------------------------------*
           05  w-wrk-box-err              pic  9(01)                  .

      *    *===========================================================*
      *    * Link-area per accettazione comune                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/acomgeo0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione frazione                       *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/afrageo0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione localita'                      *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/alocgeo0.acl"                   .

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
      *              * Tasto di funzione Pf4 :                         *
      *              *  - se Impostazione chiave    : non abilitato    *
      *              *  - se Inserimento            : non abilitato    *
      *              *  - se Visualizzazione        : abilitato        *
      *              *  - se Almeno una modifica    : non abilitato    *
      *              *  - altrimenti                : abilitato        *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    "K"   or
                     w-cnt-mfu-tip-fun    =    "I"   or
                     w-cnt-acc-flg-aum    not  = spaces
                     go to exe-acc-cmp-080.
           move      "[4] "               to   v-pfk (18)             .
       exe-acc-cmp-080.
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
                     go to exe-acc-cmp-820.
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
       exe-acc-cmp-820.
      *              *-------------------------------------------------*
      *              * Set del flag di tasto Pf4                       *
      *              *-------------------------------------------------*
           if        v-key                not  = "[4] "
                     go to exe-acc-cmp-900.
      *                  *---------------------------------------------*
      *                  * Attivazione segnale di duplicazione         *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-dup-rec-flg      .
      *                  *---------------------------------------------*
      *                  * Forzatura tasto 'Exit'                      *
      *                  *---------------------------------------------*
           move      "EXIT"               to   v-key                  .
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
      *              * Normalizzazione segnale di duplicazione         *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-dup-rec-flg      .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione comune                 *
      *              *-------------------------------------------------*
           perform   cod-com-geo-opn-000  thru cod-com-geo-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione frazione               *
      *              *-------------------------------------------------*
           perform   cod-fra-geo-opn-000  thru cod-fra-geo-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione localita'              *
      *              *-------------------------------------------------*
           perform   cod-loc-geo-opn-000  thru cod-loc-geo-opn-999    .
      *              *-------------------------------------------------*
      *              * Inizializzazione valori di defaults generali    *
      *              *-------------------------------------------------*
           move      spaces               to   w-def-sgl-std          .
           move      zero                 to   w-def-cod-cmn          .
           move      zero                 to   w-def-cod-fzn          .
           move      zero                 to   w-def-cod-lct          .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione comune                *
      *              *-------------------------------------------------*
           perform   cod-com-geo-cls-000  thru cod-com-geo-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione frazione              *
      *              *-------------------------------------------------*
           perform   cod-fra-geo-cls-000  thru cod-fra-geo-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione localita'             *
      *              *-------------------------------------------------*
           perform   cod-loc-geo-cls-000  thru cod-loc-geo-cls-999    .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * [gxs]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxs                 .
      *              *-------------------------------------------------*
      *              * [gxc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [gxs]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxs                 .
      *              *-------------------------------------------------*
      *              * [gxc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
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
      *                  * Sigla per lo stradario                      *
      *                  *---------------------------------------------*
           perform   acc-sgl-std-000      thru acc-sgl-std-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
       acc-key-reg-200.
      *                  *---------------------------------------------*
      *                  * Descrizione in chiave                       *
      *                  *---------------------------------------------*
           perform   acc-des-key-000      thru acc-des-key-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
           if        v-key                =    "UP  "
                     go to acc-key-reg-100.
       acc-key-reg-300.
      *                  *---------------------------------------------*
      *                  * Specifica allo stradario                    *
      *                  *---------------------------------------------*
           perform   acc-spc-key-000      thru acc-spc-key-999        .
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
      *              * Sigla per lo stradario                          *
      *              *-------------------------------------------------*
           perform   vis-sgl-std-000      thru vis-sgl-std-999        .
      *              *-------------------------------------------------*
      *              * Descrizione in chiave                           *
      *              *-------------------------------------------------*
           perform   vis-des-key-000      thru vis-des-key-999        .
      *              *-------------------------------------------------*
      *              * Specifica allo stradario                        *
      *              *-------------------------------------------------*
           perform   vis-spc-key-000      thru vis-spc-key-999        .
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
           move      07                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Sigla per lo stradario                          *
      *              *-------------------------------------------------*
           perform   pmt-sgl-std-000      thru pmt-sgl-std-999        .
      *              *-------------------------------------------------*
      *              * Descrizione in chiave                           *
      *              *-------------------------------------------------*
           perform   pmt-des-key-000      thru pmt-des-key-999        .
      *              *-------------------------------------------------*
      *              * Specifica allo stradario                        *
      *              *-------------------------------------------------*
           perform   pmt-spc-key-000      thru pmt-spc-key-999        .
      *              *-------------------------------------------------*
      *              * Linea di trattini                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Sigla per lo stradario        *
      *    *-----------------------------------------------------------*
       pmt-sgl-std-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Sigla per lo stradario         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-sgl-std-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Descrizione in chiave         *
      *    *-----------------------------------------------------------*
       pmt-des-key-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Chiave per la ricerca          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-des-key-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Specifica allo stradario      *
      *    *-----------------------------------------------------------*
       pmt-spc-key-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Specifica per la strada        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-spc-key-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Sigla per lo stradario        *
      *    *-----------------------------------------------------------*
       acc-sgl-std-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se il valore attuale e' spaces ma esiste un *
      *                  * valore di default generale si forza e si    *
      *                  * visualizza il valore di default generale    *
      *                  *---------------------------------------------*
           if        w-tes-sgl-std        not  = spaces
                     go to acc-sgl-std-100.
           move      w-def-sgl-std        to   w-tes-sgl-std          .
           perform   vis-sgl-std-000      thru vis-sgl-std-999        .
       acc-sgl-std-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-sgl-std        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-sgl-std-999.
       acc-sgl-std-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-sgl-std          .
       acc-sgl-std-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che il valore non sia a spaces         *
      *                  *---------------------------------------------*
           if        w-tes-sgl-std        not  = spaces
                     go to acc-sgl-std-410.
           if        v-key                not  = "UP  "
                     go to acc-sgl-std-100.
       acc-sgl-std-410.
      *                  *---------------------------------------------*
      *                  * Test che il primo carattere non sia a spazi *
      *                  *---------------------------------------------*
           if        w-tes-sgl-std
                    (01 : 01)             not  = spaces
                     go to acc-sgl-std-420.
      *                      *-----------------------------------------*
      *                      * Box di errore e reimpostazione          *
      *                      *-----------------------------------------*
           move      "Non e' ammesso uno spazio vuoto iniziale !      "
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     acc-sgl-std-100.
       acc-sgl-std-420.
      *                  *---------------------------------------------*
      *                  * Test che il primo carattere non sia ','     *
      *                  *---------------------------------------------*
           if        w-tes-sgl-std
                    (01 : 01)             not  = ","
                     go to acc-sgl-std-430.
      *                      *-----------------------------------------*
      *                      * Box di errore e reimpostazione          *
      *                      *-----------------------------------------*
           move      "Non e' ammessa una virgola iniziale !           "
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     acc-sgl-std-100.
       acc-sgl-std-430.
      *                  *---------------------------------------------*
      *                  * Test che l'ultimo carattere non sia ','     *
      *                  *---------------------------------------------*
           move      w-tes-sgl-std        to   w-all-str-alf          .
           move      31                   to   w-all-str-i01          .
       acc-sgl-std-432.
           if        w-all-str-alf
                    (w-all-str-i01 : 01)  =    spaces
                     subtract  1          from w-all-str-i01
                     go to acc-sgl-std-432.
           if        w-all-str-alf
                    (w-all-str-i01 : 01)  not  = ","
                     go to acc-sgl-std-440.
      *                      *-----------------------------------------*
      *                      * Box di errore e reimpostazione          *
      *                      *-----------------------------------------*
           move      "Non e' ammessa una virgola finale !             "
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     acc-sgl-std-100.
       acc-sgl-std-440.
      *                  *---------------------------------------------*
      *                  * Test che non esistano caratteri ','         *
      *                  *---------------------------------------------*
           move      zero                 to   w-all-str-i01          .
           inspect   w-tes-sgl-std    tallying w-all-str-i01
                     for    all ","                                   .
           if        w-all-str-i01        =    zero
                     go to acc-sgl-std-600.
      *                      *-----------------------------------------*
      *                      * Box di errore e reimpostazione          *
      *                      *-----------------------------------------*
           move      "Non sono ammesse virgole nella sigla !          "
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     acc-sgl-std-100.
       acc-sgl-std-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Memorizzazione del valore come valore di    *
      *                  * default generale                            *
      *                  *---------------------------------------------*
           move      w-tes-sgl-std        to   w-def-sgl-std          .
       acc-sgl-std-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-sgl-std-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-sgl-std-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-sgl-std-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-sgl-std-999.
       acc-sgl-std-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Sigla per lo stradario     *
      *    *-----------------------------------------------------------*
       vis-sgl-std-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-tes-sgl-std        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sgl-std-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Descrizione in chiave         *
      *    *-----------------------------------------------------------*
       acc-des-key-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-des-key-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-des-key        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-des-key-999.
       acc-des-key-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-des-key          .
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-des-key-400.
      *                  *---------------------------------------------*
      *                  * Find su tabella [gxs]                       *
      *                  *---------------------------------------------*
           move      w-tes-sgl-std        to   w-fnd-arc-gxs-sgl      .
           move      w-tes-des-key        to   w-fnd-arc-gxs-key      .
           perform   fnd-arc-gxs-000      thru fnd-arc-gxs-999        .
           if        w-fnd-arc-gxs-sel    not  = spaces
                     go to acc-des-key-100.
           move      w-fnd-arc-gxs-key    to   w-tes-des-key          .
           move      w-fnd-arc-gxs-spc    to   w-tes-spc-key          .
           perform   vis-des-key-000      thru vis-des-key-999        .
           perform   vis-spc-key-000      thru vis-spc-key-999        .
           move      "DO  "               to   v-key                  .
           go to     acc-des-key-800.
       acc-des-key-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che se il valore e' a spaces           *
      *                  *---------------------------------------------*
           if        w-tes-des-key        not  = spaces
                     go to acc-des-key-410
           else      go to acc-des-key-600.
       acc-des-key-410.
      *                  *---------------------------------------------*
      *                  * Test che il primo carattere non sia a spazi *
      *                  *---------------------------------------------*
           if        w-tes-des-key
                    (01 : 01)             not  = spaces
                     go to acc-des-key-420.
      *                      *-----------------------------------------*
      *                      * Box di errore e reimpostazione          *
      *                      *-----------------------------------------*
           move      "Non e' ammesso uno spazio vuoto iniziale !      "
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     acc-des-key-100.
       acc-des-key-420.
      *                  *---------------------------------------------*
      *                  * Test che il primo carattere non sia ','     *
      *                  *---------------------------------------------*
           if        w-tes-des-key
                    (01 : 01)             not  = ","
                     go to acc-des-key-430.
      *                      *-----------------------------------------*
      *                      * Box di errore e reimpostazione          *
      *                      *-----------------------------------------*
           move      "Non e' ammessa una virgola iniziale !           "
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     acc-des-key-100.
       acc-des-key-430.
      *                  *---------------------------------------------*
      *                  * Test che l'ultimo carattere non sia ','     *
      *                  *---------------------------------------------*
           move      w-tes-des-key        to   w-all-str-alf          .
           move      31                   to   w-all-str-i01          .
       acc-des-key-432.
           if        w-all-str-alf
                    (w-all-str-i01 : 01)  =    spaces
                     subtract  1          from w-all-str-i01
                     go to acc-des-key-432.
           if        w-all-str-alf
                    (w-all-str-i01 : 01)  not  = ","
                     go to acc-des-key-440.
      *                      *-----------------------------------------*
      *                      * Box di errore e reimpostazione          *
      *                      *-----------------------------------------*
           move      "Non e' ammessa una virgola finale !             "
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     acc-des-key-100.
       acc-des-key-440.
      *                  *---------------------------------------------*
      *                  * Test che non esistano caratteri ','         *
      *                  *---------------------------------------------*
           move      zero                 to   w-all-str-i01          .
           inspect   w-tes-des-key    tallying w-all-str-i01
                     for    all ","                                   .
           if        w-all-str-i01        =    zero
                     go to acc-des-key-600.
      *                      *-----------------------------------------*
      *                      * Box di errore e reimpostazione          *
      *                      *-----------------------------------------*
           move      "Non sono ammesse virgole nella descrizione !    "
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     acc-des-key-100.
       acc-des-key-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se descrizione  a spaces : normalizzazione  *
      *                  * e visualizzazione specifica dello stradario *
      *                  *---------------------------------------------*
           if        w-tes-des-key        not  = spaces
                     go to acc-des-key-800.
           move      spaces               to   w-tes-spc-key          .
           perform   vis-spc-key-000      thru vis-spc-key-999        .
           move      "DO  "               to   v-key                  .
       acc-des-key-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-des-key-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-des-key-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-des-key-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-des-key-999.
       acc-des-key-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Descrizione in chiave      *
      *    *-----------------------------------------------------------*
       vis-des-key-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-tes-des-key        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-key-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Specifica allo stradario      *
      *    *-----------------------------------------------------------*
       acc-spc-key-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-des-key        =    spaces
                     go to acc-spc-key-999.
       acc-spc-key-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-spc-key        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-spc-key-999.
       acc-spc-key-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-spc-key          .
       acc-spc-key-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che il valore non sia a spaces         *
      *                  *---------------------------------------------*
           if        w-tes-spc-key        not  = spaces
                     go to acc-spc-key-410.
           if        v-key                =    "UP  "
                     go to acc-spc-key-600
           else      go to acc-spc-key-100.
       acc-spc-key-410.
      *                  *---------------------------------------------*
      *                  * Test che il primo carattere non sia a spazi *
      *                  *---------------------------------------------*
           if        w-tes-spc-key
                    (01 : 01)             not  = spaces
                     go to acc-spc-key-420.
      *                      *-----------------------------------------*
      *                      * Box di errore e reimpostazione          *
      *                      *-----------------------------------------*
           move      "Non e' ammesso uno spazio vuoto iniziale !      "
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     acc-spc-key-100.
       acc-spc-key-420.
      *                  *---------------------------------------------*
      *                  * Test che il primo carattere non sia ','     *
      *                  *---------------------------------------------*
           if        w-tes-spc-key
                    (01 : 01)             not  = ","
                     go to acc-spc-key-430.
      *                      *-----------------------------------------*
      *                      * Box di errore e reimpostazione          *
      *                      *-----------------------------------------*
           move      "Non e' ammessa una virgola iniziale !           "
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     acc-spc-key-100.
       acc-spc-key-430.
      *                  *---------------------------------------------*
      *                  * Test che l'ultimo carattere non sia ','     *
      *                  *---------------------------------------------*
           move      w-tes-spc-key        to   w-all-str-alf          .
           move      31                   to   w-all-str-i01          .
       acc-spc-key-432.
           if        w-all-str-alf
                    (w-all-str-i01 : 01)  =    spaces
                     subtract  1          from w-all-str-i01
                     go to acc-spc-key-432.
           if        w-all-str-alf
                    (w-all-str-i01 : 01)  not  = ","
                     go to acc-spc-key-440.
      *                      *-----------------------------------------*
      *                      * Box di errore e reimpostazione          *
      *                      *-----------------------------------------*
           move      "Non e' ammessa una virgola finale !             "
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     acc-spc-key-100.
       acc-spc-key-440.
      *                  *---------------------------------------------*
      *                  * Test se esiste uno e uno solo carattere ',' *
      *                  *---------------------------------------------*
           move      zero                 to   w-all-str-i01          .
           inspect   w-tes-spc-key    tallying w-all-str-i01
                     for    all ","                                   .
           if        w-all-str-i01        =    1
                     go to acc-spc-key-450.
      *                      *-----------------------------------------*
      *                      * Se nessuna virgola : nessun ulteriore   *
      *                      * controllo                               *
      *                      *-----------------------------------------*
           if        w-all-str-i01        =    zero
                     go to acc-spc-key-600.
      *                      *-----------------------------------------*
      *                      * Box di errore e reimpostazione          *
      *                      *-----------------------------------------*
           move      "Non e' ammessa piu' di una virgola !            "
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     acc-spc-key-100.
       acc-spc-key-450.
      *                  *---------------------------------------------*
      *                  * Test che il carattere immediatamente prece- *
      *                  * dente il carattere ',' non sia a spaces     *
      *                  *---------------------------------------------*
           move      w-tes-spc-key        to   w-all-str-alf          .
           move      31                   to   w-all-str-i01          .
       acc-spc-key-452.
           if        w-all-str-alf
                    (w-all-str-i01 : 01)  not  = ","
                     subtract  1          from w-all-str-i01
                     go to acc-spc-key-452.
           subtract  1                    from w-all-str-i01          .
           if        w-all-str-alf
                    (w-all-str-i01 : 01)  not  = spaces
                     go to acc-spc-key-460.
      *                      *-----------------------------------------*
      *                      * Box di errore e reimpostazione          *
      *                      *-----------------------------------------*
           move      "Non e' ammesso uno spazio prima della virgola ! "
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     acc-spc-key-100.
       acc-spc-key-460.
      *                  *---------------------------------------------*
      *                  * Test che il carattere immediatamente suc-   *
      *                  * cessivo al carattere ',' non sia a spaces   *
      *                  *---------------------------------------------*
           move      w-tes-spc-key        to   w-all-str-alf          .
           move      31                   to   w-all-str-i01          .
       acc-spc-key-462.
           if        w-all-str-alf
                    (w-all-str-i01 : 01)  not  = ","
                     subtract  1          from w-all-str-i01
                     go to acc-spc-key-462.
           add       1                    to   w-all-str-i01          .
           if        w-all-str-alf
                    (w-all-str-i01 : 01)  not  = spaces
                     go to acc-spc-key-600.
      *                      *-----------------------------------------*
      *                      * Box di errore e reimpostazione          *
      *                      *-----------------------------------------*
           move      "Non e' ammesso uno spazio dopo la virgola !     "
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     acc-spc-key-100.
       acc-spc-key-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-spc-key-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-spc-key-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-spc-key-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-spc-key-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-spc-key-999.
       acc-spc-key-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Specifica allo stradario   *
      *    *-----------------------------------------------------------*
       vis-spc-key-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-tes-spc-key        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-spc-key-999.
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
      *                  * Descrizione per esteso                      *
      *                  *---------------------------------------------*
           perform   acc-des-str-000      thru acc-des-str-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-110.
      *                  *---------------------------------------------*
      *                  * Codice comune di riferimento                *
      *                  *---------------------------------------------*
           perform   acc-cod-cmn-000      thru acc-cod-cmn-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-100.
       acc-tes-reg-120.
      *                  *---------------------------------------------*
      *                  * Codice frazione di riferimento              *
      *                  *---------------------------------------------*
           perform   acc-cod-fzn-000      thru acc-cod-fzn-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-110.
       acc-tes-reg-130.
      *                  *---------------------------------------------*
      *                  * Codice localita' di riferimento             *
      *                  *---------------------------------------------*
           perform   acc-cod-lct-000      thru acc-cod-lct-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-120.
       acc-tes-reg-140.
      *                  *---------------------------------------------*
      *                  * CAP                                         *
      *                  *---------------------------------------------*
           perform   acc-cap-avp-000      thru acc-cap-avp-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-130.
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
      *              * Descrizione per esteso                          *
      *              *-------------------------------------------------*
           perform   vis-des-str-000      thru vis-des-str-999        .
      *              *-------------------------------------------------*
      *              * Codice comune di riferimento                    *
      *              *-------------------------------------------------*
           perform   vis-cod-cmn-000      thru vis-cod-cmn-999        .
      *              *-------------------------------------------------*
      *              * Descrizione comune di riferimento               *
      *              *-------------------------------------------------*
           perform   vis-des-cmn-000      thru vis-des-cmn-999        .
      *              *-------------------------------------------------*
      *              * Codice provincia                                *
      *              *-------------------------------------------------*
           perform   vis-cod-prv-000      thru vis-cod-prv-999        .
      *              *-------------------------------------------------*
      *              * Codice frazione di riferimento                  *
      *              *-------------------------------------------------*
           perform   vis-cod-fzn-000      thru vis-cod-fzn-999        .
      *              *-------------------------------------------------*
      *              * Descrizione frazione di riferimento             *
      *              *-------------------------------------------------*
           perform   vis-des-fzn-000      thru vis-des-fzn-999        .
      *              *-------------------------------------------------*
      *              * Codice localita' di riferimento                 *
      *              *-------------------------------------------------*
           perform   vis-cod-lct-000      thru vis-cod-lct-999        .
      *              *-------------------------------------------------*
      *              * Descrizione localita' di riferimento            *
      *              *-------------------------------------------------*
           perform   vis-des-lct-000      thru vis-des-lct-999        .
      *              *-------------------------------------------------*
      *              * CAP                                             *
      *              *-------------------------------------------------*
           perform   vis-cap-avp-000      thru vis-cap-avp-999        .
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
           move      08                   to   v-lin                  .
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
      *              * Descrizione per esteso                          *
      *              *-------------------------------------------------*
           perform   pmt-des-str-000      thru pmt-des-str-999        .
      *              *-------------------------------------------------*
      *              * Codice comune di riferimento                    *
      *              *-------------------------------------------------*
           perform   pmt-cod-cmn-000      thru pmt-cod-cmn-999        .
      *              *-------------------------------------------------*
      *              * Codice frazione di riferimento                  *
      *              *-------------------------------------------------*
           perform   pmt-cod-fzn-000      thru pmt-cod-fzn-999        .
      *              *-------------------------------------------------*
      *              * Codice localita' di riferimento                 *
      *              *-------------------------------------------------*
           perform   pmt-cod-lct-000      thru pmt-cod-lct-999        .
      *              *-------------------------------------------------*
      *              * CAP                                             *
      *              *-------------------------------------------------*
           perform   pmt-cap-avp-000      thru pmt-cap-avp-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-tes-reg-999.
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Descrizione per esteso           *
      *    *-----------------------------------------------------------*
       pmt-des-str-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Nome esteso per lo stradario   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-des-str-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice comune di riferimento     *
      *    *-----------------------------------------------------------*
       pmt-cod-cmn-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice comune di riferimento   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-cmn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice frazione di riferimento   *
      *    *-----------------------------------------------------------*
       pmt-cod-fzn-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice frazione                :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-fzn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice localita' di riferimento  *
      *    *-----------------------------------------------------------*
       pmt-cod-lct-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice localita'               :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-lct-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : CAP per lo stradario             *
      *    *-----------------------------------------------------------*
       pmt-cap-avp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "CAP per lo stradario           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cap-avp-999.
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
      *    * Accettazione campo testata : Descrizione per esteso       *
      *    *-----------------------------------------------------------*
       acc-des-str-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-des-str-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-des-str (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-des-str-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-des-str-999.
       acc-des-str-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-des-str (1)      .
       acc-des-str-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se a spaces : reimpostazione                *
      *                  *---------------------------------------------*
           if        w-tes-des-str (1)    =    spaces
                     go to acc-des-str-100.
      *                  *---------------------------------------------*
      *                  * Test che il primo carattere non sia a spazi *
      *                  *---------------------------------------------*
           if        w-tes-des-str (1)
                    (01 : 01)             not  = spaces
                     go to acc-des-str-600.
      *                      *-----------------------------------------*
      *                      * Box di errore e reimpostazione          *
      *                      *-----------------------------------------*
           move      "Non e' ammesso uno spazio vuoto iniziale !      "
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     acc-des-str-100.
       acc-des-str-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-des-str-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-des-str-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-des-str-100.
       acc-des-str-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione per esteso    *
      *    *-----------------------------------------------------------*
       vis-des-str-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-tes-des-str (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-str-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice comune                *
      *    *-----------------------------------------------------------*
       acc-cod-cmn-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se il valore attuale e' zero ma esiste un   *
      *                  * valore di default generale si forza e si    *
      *                  * visualizza il valore di default generale    *
      *                  *---------------------------------------------*
           if        w-tes-cod-cmn (1)    not  = zero
                     go to acc-cod-cmn-050.
           move      w-def-cod-cmn        to   w-tes-cod-cmn (1)      .
           perform   vis-cod-cmn-000      thru vis-cod-cmn-999        .
       acc-cod-cmn-050.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-cod-cmn (1)    to   w-sav-cod-cmn          .
       acc-cod-cmn-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-com-geo-ope      .
           move      w-tes-cod-cmn (1)    to   w-cod-com-geo-cmn      .
           move      11                   to   w-cod-com-geo-lin      .
           move      34                   to   w-cod-com-geo-pos      .
           move      11                   to   w-cod-com-geo-dln      .
           move      41                   to   w-cod-com-geo-dps      .
           move      "9B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-com-geo-cll-000  thru cod-com-geo-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-com-geo-foi-000  thru cod-com-geo-foi-999    .
       acc-cod-cmn-110.
           perform   cod-com-geo-cll-000  thru cod-com-geo-cll-999    .
           if        w-cod-com-geo-ope    =    "F+"
                     go to acc-cod-cmn-115.
           if        w-cod-com-geo-ope    =    "AC"
                     go to acc-cod-cmn-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-cmn-115.
           perform   cod-com-geo-foi-000  thru cod-com-geo-foi-999    .
           go to     acc-cod-cmn-110.
       acc-cod-cmn-120.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-cmn-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-cmn-999.
       acc-cod-cmn-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      w-cod-com-geo-cmn    to   w-tes-cod-cmn (1)      .
       acc-cod-cmn-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura di controllo                        *
      *                  *---------------------------------------------*
           move      "C"                  to   w-let-arc-gxc-tip      .
           move      w-tes-cod-cmn (1)    to   w-let-arc-gxc-cmn      .
           move      zero                 to   w-let-arc-gxc-fzn      .
           move      zero                 to   w-let-arc-gxc-lct      .
           perform   let-arc-gxc-000      thru let-arc-gxc-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione dati letti                   *
      *                  *---------------------------------------------*
           move      w-let-arc-gxc-des    to   w-tes-cod-cmn-des (1)  .
           move      w-let-arc-gxc-prv    to   w-tes-cod-cmn-prv (1)  .
           move      w-let-arc-gxc-cap    to   w-tes-cod-cmn-cap (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-cmn-000      thru vis-des-cmn-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione provincia                   *
      *                  *---------------------------------------------*
           perform   vis-cod-prv-000      thru vis-cod-prv-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-gxc-flg    not  = spaces
                     go to acc-cod-cmn-100.
       acc-cod-cmn-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore cambiato oppure no           *
      *                  *---------------------------------------------*
           if        w-tes-cod-cmn (1)    =    w-sav-cod-cmn
                     go to acc-cod-cmn-625
           else      go to acc-cod-cmn-650.
       acc-cod-cmn-625.
      *                  *---------------------------------------------*
      *                  * Se valore inalterato                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     acc-cod-cmn-800.
       acc-cod-cmn-650.
      *                  *---------------------------------------------*
      *                  * Se valore cambiato rispetto al precedente   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione, se   *
      *                      * necessario di :                         *
      *                      * - Frazione                              *
      *                      * - Localita'                             *
      *                      *-----------------------------------------*
       acc-cod-cmn-652.
      *                          *-------------------------------------*
      *                          * Frazione                            *
      *                          *-------------------------------------*
           if        w-tes-cod-fzn (1)    =    zero
                     go to acc-cod-cmn-654.
           move      zero                 to   w-tes-cod-fzn (1)      .
           move      spaces               to   w-tes-cod-fzn-des (1)  .
           perform   vis-cod-fzn-000      thru vis-cod-fzn-999        .
           perform   vis-des-fzn-000      thru vis-des-fzn-999        .
       acc-cod-cmn-654.
      *                          *-------------------------------------*
      *                          * Localita'                           *
      *                          *-------------------------------------*
           if        w-tes-cod-lct (1)    =    zero
                     go to acc-cod-cmn-656.
           move      zero                 to   w-tes-cod-lct (1)      .
           move      spaces               to   w-tes-cod-lct-des (1)  .
           perform   vis-cod-lct-000      thru vis-cod-lct-999        .
           perform   vis-des-lct-000      thru vis-des-lct-999        .
       acc-cod-cmn-656.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     acc-cod-cmn-800.
       acc-cod-cmn-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-cmn-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-cmn-100.
       acc-cod-cmn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice Comune             *
      *    *-----------------------------------------------------------*
       vis-cod-cmn-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      11                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-tes-cod-cmn (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-cmn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione Comune        *
      *    *-----------------------------------------------------------*
       vis-des-cmn-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-cmn-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-cmn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice provincia          *
      *    *-----------------------------------------------------------*
       vis-cod-prv-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      w-tes-cod-cmn-prv (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-prv-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice frazione              *
      *    *-----------------------------------------------------------*
       acc-cod-fzn-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cod-cmn (1)    =    zero
                     go to acc-cod-fzn-999.
      *                  *---------------------------------------------*
      *                  * Se il valore attuale e' zero ma esiste un   *
      *                  * valore di default generale si forza e si    *
      *                  * visualizza il valore di default generale    *
      *                  *---------------------------------------------*
           if        w-tes-cod-fzn (1)    not  = zero
                     go to acc-cod-fzn-050.
           move      w-def-cod-fzn        to   w-tes-cod-fzn (1)      .
           perform   vis-cod-fzn-000      thru vis-cod-fzn-999        .
       acc-cod-fzn-050.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-cod-fzn (1)    to   w-sav-cod-fzn          .
       acc-cod-fzn-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-fra-geo-ope      .
           move      w-tes-cod-cmn (1)    to   w-cod-fra-geo-cmn      .
           move      w-tes-cod-fzn (1)    to   w-cod-fra-geo-fzn      .
           move      13                   to   w-cod-fra-geo-lin      .
           move      34                   to   w-cod-fra-geo-pos      .
           move      13                   to   w-cod-fra-geo-dln      .
           move      41                   to   w-cod-fra-geo-dps      .
           move      "9B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-fra-geo-cll-000  thru cod-fra-geo-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-fra-geo-foi-000  thru cod-fra-geo-foi-999    .
       acc-cod-fzn-110.
           perform   cod-fra-geo-cll-000  thru cod-fra-geo-cll-999    .
           if        w-cod-fra-geo-ope    =    "F+"
                     go to acc-cod-fzn-115.
           if        w-cod-fra-geo-ope    =    "AC"
                     go to acc-cod-fzn-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-fzn-115.
           perform   cod-fra-geo-foi-000  thru cod-fra-geo-foi-999    .
           go to     acc-cod-fzn-110.
       acc-cod-fzn-120.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-fzn-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-fzn-999.
       acc-cod-fzn-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      w-cod-fra-geo-fzn    to   w-tes-cod-fzn (1)      .
       acc-cod-fzn-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura di controllo                        *
      *                  *---------------------------------------------*
           move      "F"                  to   w-let-arc-gxc-tip      .
           move      w-tes-cod-cmn (1)    to   w-let-arc-gxc-cmn      .
           move      w-tes-cod-fzn (1)    to   w-let-arc-gxc-fzn      .
           move      zero                 to   w-let-arc-gxc-lct      .
           perform   let-arc-gxc-000      thru let-arc-gxc-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione dati letti                   *
      *                  *---------------------------------------------*
           move      w-let-arc-gxc-des    to   w-tes-cod-fzn-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-fzn-000      thru vis-des-fzn-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-gxc-flg    not  = spaces
                     go to acc-cod-fzn-100.
       acc-cod-fzn-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore cambiato oppure no           *
      *                  *---------------------------------------------*
           if        w-tes-cod-fzn (1)    =    w-sav-cod-fzn
                     go to acc-cod-fzn-625
           else      go to acc-cod-fzn-650.
       acc-cod-fzn-625.
      *                  *---------------------------------------------*
      *                  * Se valore inalterato                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     acc-cod-fzn-800.
       acc-cod-fzn-650.
      *                  *---------------------------------------------*
      *                  * Se valore cambiato rispetto al precedente   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione, se   *
      *                      * necessario di :                         *
      *                      * - Localita'                             *
      *                      *-----------------------------------------*
       acc-cod-fzn-652.
      *                          *-------------------------------------*
      *                          * Localita'                           *
      *                          *-------------------------------------*
           if        w-tes-cod-lct (1)    =    zero
                     go to acc-cod-fzn-654.
           move      zero                 to   w-tes-cod-lct (1)      .
           move      spaces               to   w-tes-cod-lct-des (1)  .
           perform   vis-cod-lct-000      thru vis-cod-lct-999        .
           perform   vis-des-lct-000      thru vis-des-lct-999        .
       acc-cod-fzn-654.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     acc-cod-fzn-800.
       acc-cod-fzn-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-fzn-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-fzn-100.
       acc-cod-fzn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice frazione           *
      *    *-----------------------------------------------------------*
       vis-cod-fzn-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      13                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-tes-cod-fzn (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-fzn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione frazione      *
      *    *-----------------------------------------------------------*
       vis-des-fzn-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-fzn-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-fzn-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice localita'             *
      *    *-----------------------------------------------------------*
       acc-cod-lct-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cod-cmn (1)    =    zero
                     go to acc-cod-lct-999.
      *                  *---------------------------------------------*
      *                  * Se il valore attuale e' zero ma esiste un   *
      *                  * valore di default generale si forza e si    *
      *                  * visualizza il valore di default generale    *
      *                  *---------------------------------------------*
           if        w-tes-cod-lct (1)    not  = zero
                     go to acc-cod-lct-100.
           move      w-def-cod-lct        to   w-tes-cod-lct (1)      .
           perform   vis-cod-lct-000      thru vis-cod-lct-999        .
       acc-cod-lct-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-loc-geo-ope      .
           move      w-tes-cod-cmn (1)    to   w-cod-loc-geo-cmn      .
           move      w-tes-cod-fzn (1)    to   w-cod-loc-geo-fzn      .
           move      w-tes-cod-lct (1)    to   w-cod-loc-geo-lct      .
           move      15                   to   w-cod-loc-geo-lin      .
           move      34                   to   w-cod-loc-geo-pos      .
           move      15                   to   w-cod-loc-geo-dln      .
           move      41                   to   w-cod-loc-geo-dps      .
           move      "9B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-loc-geo-cll-000  thru cod-loc-geo-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-loc-geo-foi-000  thru cod-loc-geo-foi-999    .
       acc-cod-lct-110.
           perform   cod-loc-geo-cll-000  thru cod-loc-geo-cll-999    .
           if        w-cod-loc-geo-ope    =    "F+"
                     go to acc-cod-lct-115.
           if        w-cod-loc-geo-ope    =    "AC"
                     go to acc-cod-lct-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-lct-115.
           perform   cod-loc-geo-foi-000  thru cod-loc-geo-foi-999    .
           go to     acc-cod-lct-110.
       acc-cod-lct-120.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-lct-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-lct-999.
       acc-cod-lct-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      w-cod-loc-geo-lct    to   w-tes-cod-lct (1)      .
       acc-cod-lct-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura di controllo                        *
      *                  *---------------------------------------------*
           move      "L"                  to   w-let-arc-gxc-tip      .
           move      w-tes-cod-cmn (1)    to   w-let-arc-gxc-cmn      .
           move      w-tes-cod-fzn (1)    to   w-let-arc-gxc-fzn      .
           move      w-tes-cod-lct (1)    to   w-let-arc-gxc-lct      .
           perform   let-arc-gxc-000      thru let-arc-gxc-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione dati letti                   *
      *                  *---------------------------------------------*
           move      w-let-arc-gxc-des    to   w-tes-cod-lct-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-lct-000      thru vis-des-lct-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-gxc-flg    not  = spaces
                     go to acc-cod-lct-100.
       acc-cod-lct-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-lct-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-lct-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-lct-100.
       acc-cod-lct-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice localita'          *
      *    *-----------------------------------------------------------*
       vis-cod-lct-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      15                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-tes-cod-lct (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-lct-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione localita'     *
      *    *-----------------------------------------------------------*
       vis-des-lct-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-lct-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-lct-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : CAP per lo stradario         *
      *    *-----------------------------------------------------------*
       acc-cap-avp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-des-key        =    spaces  and
                     w-tes-spc-key        =    spaces
                     go to acc-cap-avp-999.
       acc-cap-avp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-cap-avp (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cap-avp-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cap-avp-999.
       acc-cap-avp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-cap-avp (1)      .
       acc-cap-avp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se a spaces : reimpostazione                *
      *                  *---------------------------------------------*
           if        w-tes-cap-avp (1)    =    spaces
                     go to acc-cap-avp-100.
      *                  *---------------------------------------------*
      *                  * Test che il CAP sia numerico                *
      *                  *---------------------------------------------*
           move      w-tes-cap-avp (1)    to   w-ctl-cap-avp-alf      .
           perform   ctl-cap-avp-000      thru ctl-cap-avp-999        .
           if        w-ctl-cap-avp-flg    not  = spaces
                     go to acc-cap-avp-100.
       acc-cap-avp-440.
      *                  *---------------------------------------------*
      *                  * Test che il C.a.p. non sia a '00000'        *
      *                  *---------------------------------------------*
           if        w-tes-cap-avp (1)    =    "00000"
                     go to acc-cap-avp-100.
       acc-cap-avp-460.
      *                  *---------------------------------------------*
      *                  * Controllo con il C.a.p. letto dal comune    *
      *                  *---------------------------------------------*
           if        w-tes-cod-cmn-cap (1)
                                          =    spaces
                     go to acc-cap-avp-600.
           move      w-tes-cap-avp (1)    to   w-wrk-cap-str          .
           move      w-tes-cod-cmn-cap (1)
                                          to   w-wrk-cap-cmn          .
           if        w-wrk-str-002        =    w-wrk-cmn-002
                     go to acc-cap-avp-600.
      *                  *---------------------------------------------*
      *                  * Se C.a.p. non congruente con CAP letto dal  *
      *                  * codice comune : box di segnalazione         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Box di attenzione                       *
      *                      *-----------------------------------------*
           perform   box-err-cap-000      thru box-err-cap-999        .
      *                      *-----------------------------------------*
      *                      * Se scelta reimpostazione                *
      *                      *-----------------------------------------*
           if        w-wrk-box-err        =    2
                     go to acc-cap-avp-100
           else      go to acc-cap-avp-600.
       acc-cap-avp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cap-avp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cap-avp-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cap-avp-100.
       acc-cap-avp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : CAP per lo stradario      *
      *    *-----------------------------------------------------------*
       vis-cap-avp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-tes-cap-avp (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cap-avp-999.
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
      *              * Controllo su specifica in chiave                *
      *              *-------------------------------------------------*
           if        w-tes-des-key        =    spaces
                     go to cnt-tdo-key-999.
           if        w-tes-spc-key        =    spaces
                     move  "#"            to   w-cnt-tdo-key-flg      .
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
           if        w-tes-sgl-std        =    spaces
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
       cnt-tdo-nok-100.
      *              *-------------------------------------------------*
      *              * Controlli bloccanti                             *
      *              *-------------------------------------------------*
       cnt-tdo-nok-120.
      *                  *---------------------------------------------*
      *                  * Controllo su descrizione per esteso         *
      *                  *---------------------------------------------*
           if        w-tes-des-str (1)    not  = spaces
                     go to cnt-tdo-nok-140.
           move      "ME"                 to   v-ope                  .
           move      "Manca la descrizione dello stradario !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-nok-flg      .
           go to     cnt-tdo-nok-999.
       cnt-tdo-nok-140.
      *                  *---------------------------------------------*
      *                  * Controllo su comune di riferimento          *
      *                  *---------------------------------------------*
           if        w-tes-cod-cmn (1)    not  = zero
                     go to cnt-tdo-nok-160.
           move      "ME"                 to   v-ope                  .
           move      "Manca il codice comune di riferimento !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-nok-flg      .
           go to     cnt-tdo-nok-999.
       cnt-tdo-nok-160.
      *                  *---------------------------------------------*
      *                  * Controllo su CAP                            *
      *                  *---------------------------------------------*
           if        w-tes-des-key        =    spaces
                     go to cnt-tdo-nok-500.
           if        w-tes-cap-avp (1)    not  = spaces
                     go to cnt-tdo-nok-500.
           move      "ME"                 to   v-ope                  .
           move      "Manca il C.A.P. !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-nok-flg      .
           go to     cnt-tdo-nok-999.
       cnt-tdo-nok-500.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
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
           move      spaces               to   w-tes-sgl-std          .
           move      spaces               to   w-tes-des-key          .
           move      spaces               to   w-tes-spc-key          .
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *-----------------------------------------------------------*
       nor-nok-tes-000.
           move      spaces               to   w-tes-des-str (1)      .
           move      spaces               to   w-tes-cap-avp (1)      .
           move      zero                 to   w-tes-cod-cmn (1)      .
           move      spaces               to   w-tes-cod-cmn-des (1)  .
           move      spaces               to   w-tes-cod-cmn-prv (1)  .
           move      spaces               to   w-tes-cod-cmn-cap (1)  .
           move      zero                 to   w-tes-cod-fzn (1)      .
           move      spaces               to   w-tes-cod-fzn-des (1)  .
           move      zero                 to   w-tes-cod-lct (1)      .
           move      spaces               to   w-tes-cod-lct-des (1)  .
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
      *              * Lettura                                         *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "SGLDES    "         to   f-key                  .
           move      w-tes-sgl-std        to   rf-gxs-sgl-std         .
           move      w-tes-des-key        to   rf-gxs-des-key         .
           move      w-tes-spc-key        to   rf-gxs-spc-key         .
           move      "pgm/geo/fls/ioc/obj/iofgxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxs                 .
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
      *                          * record [gxs]                        *
      *                          *-------------------------------------*
           move      rf-gxs-des-str       to   w-tes-des-str (1)      .
           move      rf-gxs-cap-avp       to   w-tes-cap-avp (1)      .
           move      rf-gxs-cod-cmn       to   w-tes-cod-cmn (1)      .
           move      rf-gxs-cod-fzn       to   w-tes-cod-fzn (1)      .
           move      rf-gxs-cod-lct       to   w-tes-cod-lct (1)      .
           move      rf-gxs-alx-exp       to   w-tes-alx-exp (1)      .
      *                          *-------------------------------------*
      *                          * Valori contenuti indirettamente in  *
      *                          * record [gxs]                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura tabella [gxc] per il    *
      *                              * codice comune o frazione        *
      *                              *---------------------------------*
           move      "C"                  to   w-let-arc-gxc-tip      .
           move      w-tes-cod-cmn (1)    to   w-let-arc-gxc-cmn      .
           move      zero                 to   w-let-arc-gxc-lct      .
           move      zero                 to   w-let-arc-gxc-lct      .
           perform   let-arc-gxc-000      thru let-arc-gxc-999        .
           move      w-let-arc-gxc-des    to   w-tes-cod-cmn-des (1)  .
           move      w-let-arc-gxc-prv    to   w-tes-cod-cmn-prv (1)  .
           move      w-let-arc-gxc-cap    to   w-tes-cod-cmn-cap (1)  .
      *                              *---------------------------------*
      *                              * Lettura tabella [gxc] per il    *
      *                              * codice frazione                 *
      *                              *---------------------------------*
           move      "F"                  to   w-let-arc-gxc-tip      .
           move      w-tes-cod-cmn (1)    to   w-let-arc-gxc-cmn      .
           move      w-tes-cod-fzn (1)    to   w-let-arc-gxc-fzn      .
           move      zero                 to   w-let-arc-gxc-lct      .
           perform   let-arc-gxc-000      thru let-arc-gxc-999        .
           move      w-let-arc-gxc-des    to   w-tes-cod-fzn-des (1)  .
      *                              *---------------------------------*
      *                              * Lettura tabella [gxc] per il    *
      *                              * codice localita'                *
      *                              *---------------------------------*
           move      "L"                  to   w-let-arc-gxc-tip      .
           move      w-tes-cod-cmn (1)    to   w-let-arc-gxc-cmn      .
           move      w-tes-cod-fzn (1)    to   w-let-arc-gxc-fzn      .
           move      w-tes-cod-lct (1)    to   w-let-arc-gxc-lct      .
           perform   let-arc-gxc-000      thru let-arc-gxc-999        .
           move      w-let-arc-gxc-des    to   w-tes-cod-lct-des (1)  .
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
       pre-acc-ins-100.
      *              *-------------------------------------------------*
      *              * Duplicazione record precedente se richiesto     *
      *              *-------------------------------------------------*
           if        w-cnt-dup-rec-flg    =    spaces
                     go to pre-acc-ins-200.
      *                  *---------------------------------------------*
      *                  * Routine di duplicazione record              *
      *                  *---------------------------------------------*
           perform   rou-dup-rec-000      thru rou-dup-rec-999        .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pre-acc-ins-900.
       pre-acc-ins-200.
      *              *-------------------------------------------------*
      *              * Se si tratta di record normale e non di nome    *
      *              * per lo stradario, si leggono e propongono come  *
      *              * valori di default i codici Comune, Frazione e   *
      *              * Localita' letti dal record con la stessa Sigla  *
      *              * stradario ma con descrizione e specifica a spa- *
      *              * ces                                             *
      *              *-------------------------------------------------*
           perform   det-val-def-000      thru det-val-def-999        .
       pre-acc-ins-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pre-acc-ins-999.
       pre-acc-ins-999.
           exit.

      *    *===========================================================*
      *    * Routine duplicazione record                               *
      *    *-----------------------------------------------------------*
       rou-dup-rec-000.
      *              *-------------------------------------------------*
      *              * Copia dei valori di testata precedenti          *
      *              *-------------------------------------------------*
           move      w-tes-val-aep (2)    to   w-tes-val-aep (1)      .
       rou-dup-rec-100.
      *              *-------------------------------------------------*
      *              * Normalizzazione flags di controllo              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di impostazione pagine di testata      *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-tes      .
      *                  *---------------------------------------------*
      *                  * Flags di impostazione pagine di testata     *
      *                  * eseguita                                    *
      *                  *---------------------------------------------*
           move      all "#"              to   w-cnt-sts-imp-pte      .
      *                  *---------------------------------------------*
      *                  * Flags di ingresso in pagine di testata      *
      *                  * eseguito                                    *
      *                  *---------------------------------------------*
           move      all "#"              to   w-cnt-sts-ing-pte      .
      *                  *---------------------------------------------*
      *                  * Flags di visualizzazione per le pagine      *
      *                  *---------------------------------------------*
           move      spaces               to   w-cnt-sts-vis-ptx (2)  .
           move      spaces               to   w-cnt-sts-vis-ptx (3)  .
           move      spaces               to   w-cnt-sts-vis-ptx (4)  .
           move      spaces               to   w-cnt-sts-vis-ptx (5)  .
           move      spaces               to   w-cnt-sts-vis-ptx (6)  .
           move      spaces               to   w-cnt-sts-vis-ptx (7)  .
           move      spaces               to   w-cnt-sts-vis-ptx (8)  .
           move      spaces               to   w-cnt-sts-vis-ptx (9)  .
      *              *-------------------------------------------------*
      *              * Normalizzazione segnale di duplicazione         *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-dup-rec-flg      .
      *              *-------------------------------------------------*
      *              * Visualizzazione pagine di testata               *
      *              *-------------------------------------------------*
           perform   vis-tes-reg-000      thru vis-tes-reg-999        .
       rou-dup-rec-999.
           exit.

      *    *===========================================================*
      *    * Routine di determinazione valori di default               *
      *    *-----------------------------------------------------------*
       det-val-def-000.
      *              *-------------------------------------------------*
      *              * Lettura record [gxs]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "SGLDES    "         to   f-key                  .
           move      w-tes-sgl-std        to   rf-gxs-sgl-std         .
           move      spaces               to   rf-gxs-des-key         .
           move      spaces               to   rf-gxs-spc-key         .
           move      "pgm/geo/fls/ioc/obj/iofgxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxs                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato : normalizzazione va- *
      *                  * lori di default ed uscita                   *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  zero           to   w-def-cod-cmn
                     move  zero           to   w-def-cod-fzn
                     move  zero           to   w-def-cod-lct
                     go to det-val-def-999.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori letti                    *
      *              *-------------------------------------------------*
           move      rf-gxs-cod-cmn       to   w-def-cod-cmn          .
           move      rf-gxs-cod-fzn       to   w-def-cod-fzn          .
           move      rf-gxs-cod-lct       to   w-def-cod-lct          .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-val-def-999.
       det-val-def-999.
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
      *              * Normalizzazione segnale di duplicazione         *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-dup-rec-flg      .
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
      *              * Trattamento file [gxs]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-500.
      *                      *-----------------------------------------*
      *                      * Write record [gxs]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-gxs-000      thru wrt-rec-gxs-999        .
           go to     scr-mov-fil-999.
       scr-mov-fil-500.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rewrite record [gxs]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-gxs-000      thru rew-rec-gxs-999        .
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *-----------------------------------------------------------*
       del-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Delete record [gxs]                             *
      *              *-------------------------------------------------*
           perform   del-rec-gxs-000      thru del-rec-gxs-999        .
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [gxs]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-gxs-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxs                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-tes-sgl-std        to   rf-gxs-sgl-std         .
           move      w-tes-des-key        to   rf-gxs-des-key         .
           move      w-tes-spc-key        to   rf-gxs-spc-key         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      w-tes-des-str (1)    to   rf-gxs-des-str         .
           move      w-tes-cap-avp (1)    to   rf-gxs-cap-avp         .
           move      w-tes-cod-cmn (1)    to   rf-gxs-cod-cmn         .
           move      w-tes-cod-fzn (1)    to   rf-gxs-cod-fzn         .
           move      w-tes-cod-lct (1)    to   rf-gxs-cod-lct         .
           move      w-tes-alx-exp (1)    to   rf-gxs-alx-exp         .
       cmp-rec-gxs-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [gxs]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-gxs-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-gxs-000      thru cmp-rec-gxs-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxs                 .
       wrt-rec-gxs-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [gxs]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-gxs-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-gxs-000      thru cmp-rec-gxs-999        .
      *              *-------------------------------------------------*
      *              * Forced put record                               *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxs                 .
       rew-rec-gxs-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [gxs]                                *
      *    *-----------------------------------------------------------*
       del-rec-gxs-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-gxs-000      thru cmp-rec-gxs-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxs                 .
       del-rec-gxs-999.
           exit.

      *    *===========================================================*
      *    * Find su archivio [gxs]                                    *
      *    *-----------------------------------------------------------*
       fnd-arc-gxs-000.
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pgeo5010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     move  "#"            to   w-fnd-arc-gxs-sel
                     go to  fnd-arc-gxs-999.
      *              *-------------------------------------------------*
      *              * Preparazione variabile di i.p.c. per possibili- *
      *              * ta' di function-key "SLCT" durante l'interroga- *
      *              * zione                                           *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "fkselect"           to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      04                   to   s-car                  .
           move      "SLCT"               to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Preparazione variabile di i.p.c. "sgl-std"      *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "sgl-std"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      05                   to   s-car                  .
           move      w-fnd-arc-gxs-sgl    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Preparazione variabile di i.p.c. "des-key"      *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "des-key"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      30                   to   s-car                  .
           move      w-fnd-arc-gxs-key    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Richiamo programma di interrogazione            *
      *              *-------------------------------------------------*
           move      "pgm/geo/prg/obj/pgeo5010"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
      *              *-------------------------------------------------*
      *              * Estrazione di eventuale variabile di i.p.c. de- *
      *              * terminata da function-key "SLCT" durante l'in-  *
      *              * terrogazione                                    *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "des-key"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  spaces         to   w-fnd-arc-gxs-sel
                     move  s-alf          to   w-fnd-arc-gxs-key
           else      move  "#"            to   w-fnd-arc-gxs-sel      .
           move      "CV"                 to   s-ope                  .
           move      "spc-key"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  spaces         to   w-fnd-arc-gxs-sel
                     move  s-alf          to   w-fnd-arc-gxs-spc
           else      move  "#"            to   w-fnd-arc-gxs-sel      .
       fnd-arc-gxs-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [gxc]                         *
      *    *-----------------------------------------------------------*
       let-arc-gxc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-gxc-flg      .
      *              *-------------------------------------------------*
      *              * Deviazione secondo il tipo elemento             *
      *              *-------------------------------------------------*
           if        w-let-arc-gxc-tip    =    "C"
                     go to let-arc-gxc-100
           else if   w-let-arc-gxc-tip    =    "F"
                     go to let-arc-gxc-200
           else if   w-let-arc-gxc-tip    =    "L"
                     go to let-arc-gxc-300.
       let-arc-gxc-100.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : Comune                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se codice a zero                       *
      *                  *---------------------------------------------*
           if        w-let-arc-gxc-cmn    =    zero
                     go to let-arc-gxc-400.
      *                  *---------------------------------------------*
      *                  * Lettura Comune                              *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCFL    "         to   f-key                  .
           move      w-let-arc-gxc-cmn    to   rf-gxc-cod-cmn         .
           move      zero                 to   rf-gxc-cod-fzn         .
           move      zero                 to   rf-gxc-cod-lct         .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se record esistente    *
      *                  * oppure no                                   *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to let-arc-gxc-600
           else      go to let-arc-gxc-500.
       let-arc-gxc-200.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : Frazione                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se codice a zero                       *
      *                  *---------------------------------------------*
           if        w-let-arc-gxc-cmn    =    zero or
                     w-let-arc-gxc-fzn    =    zero
                     go to let-arc-gxc-400.
      *                  *---------------------------------------------*
      *                  * Lettura Frazione                            *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCFL    "         to   f-key                  .
           move      w-let-arc-gxc-cmn    to   rf-gxc-cod-cmn         .
           move      w-let-arc-gxc-fzn    to   rf-gxc-cod-fzn         .
           move      zero                 to   rf-gxc-cod-lct         .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se record esistente    *
      *                  * oppure no                                   *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to let-arc-gxc-600
           else      go to let-arc-gxc-500.
       let-arc-gxc-300.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : Localita'                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se codice a zero                       *
      *                  *---------------------------------------------*
           if        w-let-arc-gxc-cmn    =    zero or
                     w-let-arc-gxc-lct    =    zero
                     go to let-arc-gxc-400.
      *                  *---------------------------------------------*
      *                  * Lettura Localita'                           *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCFL    "         to   f-key                  .
           move      w-let-arc-gxc-cmn    to   rf-gxc-cod-cmn         .
           move      w-let-arc-gxc-fzn    to   rf-gxc-cod-fzn         .
           move      w-let-arc-gxc-lct    to   rf-gxc-cod-lct         .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se record esistente    *
      *                  * oppure no                                   *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to let-arc-gxc-600
           else      go to let-arc-gxc-500.
       let-arc-gxc-400.
      *              *-------------------------------------------------*
      *              * Se codice elemento a zero                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione work area                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-gxc-des      .
           move      spaces               to   w-let-arc-gxc-prv      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-gxc-999.
       let-arc-gxc-500.
      *              *-------------------------------------------------*
      *              * Se codice elemento non esistente                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione work area                   *
      *                  *---------------------------------------------*
           move      all   "."            to   w-let-arc-gxc-des      .
           move      spaces               to   w-let-arc-gxc-prv      .
           move      spaces               to   w-let-arc-gxc-cap      .
      *                  *---------------------------------------------*
      *                  * Flag di uscita a non trovato                *
      *                  *---------------------------------------------*
           move      "#"                  to   w-let-arc-gxc-flg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-gxc-999.
       let-arc-gxc-600.
      *              *-------------------------------------------------*
      *              * Se codice elemento esistente                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Bufferizzazione in work area                *
      *                  *---------------------------------------------*
           move      rf-gxc-des-cfl       to   w-let-arc-gxc-des      .
           move      rf-gxc-cod-prv       to   w-let-arc-gxc-prv      .
           move      rf-gxc-cap-avp       to   w-let-arc-gxc-cap      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-gxc-999.
       let-arc-gxc-999.
           exit.

      *    *===========================================================*
      *    * Test se CAP numerico                                      *
      *    *-----------------------------------------------------------*
       ctl-cap-avp-000.
           move      spaces               to   w-ctl-cap-avp-flg      .
           move      zero                 to   w-ctl-cap-avp-ctr      .
       ctl-cap-avp-100.
           add       1                    to   w-ctl-cap-avp-ctr      .
           if        w-ctl-cap-avp-ctr    >    5
                     go to ctl-cap-avp-999.
           if        w-ctl-cap-avp-chr
                    (w-ctl-cap-avp-ctr)   not  < "0"  and
                     w-ctl-cap-avp-chr
                    (w-ctl-cap-avp-ctr)   not  > "9"
                     go to ctl-cap-avp-100.
           move      "#"                  to   w-ctl-cap-avp-flg      .
       ctl-cap-avp-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice Comune 'geo'        *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/acomgeo0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice Frazione 'geo'      *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/afrageo0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice Localita' 'geo'     *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/alocgeo0.acs"                   .

      *    *===========================================================*
      *    * Box di errore per incongruenza con CAP del comune         *
      *    *-----------------------------------------------------------*
       box-err-cap-000.
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
           move      07                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      20                   to   v-lto                  .
           move      72                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      32                   to   v-pos                  .
           move      "A T T E N Z I O N E"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      26                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      "E' assegnato al comune di "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-tes-cod-cmn-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      "il numero di CAP :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-cmn-cap (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      61                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      "Il CAP assegnato allo stradario risulta pertanto i
      -              "ncongru-  "         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      21                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      "ente con tale valore."
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      "Azioni possibili  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      zero                 to   w-wrk-box-err          .
       box-err-cap-800.
      *              *-------------------------------------------------*
      *              * Accettazione campo di conferma                  *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-box-err-lun    to   v-car                  .
           move      w-exp-box-err-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      17                   to   v-lin                  .
           move      31                   to   v-pos                  .
           move      w-exp-box-err-tbl    to   v-txt                  .
           move      w-wrk-box-err        to   v-num                  .
           move      "DO  "               to   v-pfk (05)             .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-wrk-box-err          .
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        w-wrk-box-err        =    zero
                     go to box-err-cap-800.
       box-err-cap-900.
      *              *-------------------------------------------------*
      *              * Restore video                                   *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       box-err-cap-999.
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
           move      13                   to   v-pos                  .
           move      14                   to   v-lto                  .
           move      68                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      48                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      15                   to   v-pos                  .
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
           move      64                   to   v-pos                  .
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
           move      65                   to   v-pos                  .
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
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .
