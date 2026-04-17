       Identification Division.
       Program-Id.                                 pxpg5100           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    swd                 *
      *                        Area gestionale:    xpg                 *
      *                                Settore:    per                 *
      *                                   Fase:    xpg510              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 31/03/93    *
      *                       Versione attuale:    NdK del 20/07/99    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione Personalizzazioni                  *
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
      *    * File Control [auc]                                        *
      *    *-----------------------------------------------------------*
           select  optional  auc   assign to disk         f-auc-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is auc-key
                             file status  is f-auc-sts                .

      *    *===========================================================*
      *    * File Control [prs]                                        *
      *    *-----------------------------------------------------------*
           select  optional  prs   assign to disk         f-prs-pat
                             organization is indexed
                             access  mode is dynamic
                             record  key  is prs-rck
                             file  status is f-prs-sts                .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [auc]                                    *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/fls/rec/rfauc"                          .

      *    *===========================================================*
      *    * File Description [prs]                                    *
      *    *-----------------------------------------------------------*
       fd  prs  label record standard.
       01  prs-rec.
           05  prs-rck.
               10  prs-tre                pic  x(04)                  .
               10  prs-cod                pic  x(40)                  .
               10  prs-prg                pic  9(03)                  .
           05  prs-rcd.
               10  prs-r00.
                   15  filler occurs 400  pic  x(01)                  .
               10  prs-r10 redefines
                   prs-r00.
                   15  prs-alf            pic  x(80)                  .
                   15  filler occurs 320  pic  x(01)                  .
               10  prs-r20 redefines
                   prs-r00.
                   15  prs-num            pic s9(13)v9(05) trailing
                                                           separate
                                                           character  .
                   15  filler occurs 381  pic  x(01)                  .
               10  prs-r30 redefines
                   prs-r00.
                   15  prs-dat            pic  9(07)                  .
                   15  filler occurs 393  pic  x(01)                  .
               10  prs-r40 redefines
                   prs-r00.
                   15  prs-txt.
                       20  filler
                              occurs 400  pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area ausiliaria per controllo i-o su [auc]                *
      *    *-----------------------------------------------------------*
       01  f-auc.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-auc-nam                  pic  x(04) value "auc "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-auc-pat                  pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-auc-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Area ausiliaria per controllo i-o su [prs]                *
      *    *-----------------------------------------------------------*
       01  f-prs.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-prs-nam                  pic  x(04) value "prs "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-prs-pat                  pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-prs-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Area di identificazione                                   *
      *    *-----------------------------------------------------------*
       01  i-ide.
      *        *-------------------------------------------------------*
      *        * Sistema applicativo                                   *
      *        *-------------------------------------------------------*
           05  i-ide-sap                  pic  x(03) value
                     "swd"                                            .
      *        *-------------------------------------------------------*
      *        * Area gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-arg                  pic  x(03) value
                     "xpg"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "per"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "xpg510"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pxpg5100"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "       GESTIONE PERSONALIZZAZIONI       "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mopsys"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/o"                                  .

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
      *        *-------------------------------------------------------*
      *        * Area di controllo per duplicazione record             *
      *        *-------------------------------------------------------*
           05  w-cnt-dup.
               10  w-cnt-dup-rec-flg      pic  x(01)     value spaces .

      *    *===========================================================*
      *    * Work-area per valori di defaults generali                 *
      *    *-----------------------------------------------------------*
       01  w-def.
      *        *-------------------------------------------------------*
      *        * Tipo operazione di default                            *
      *        *-------------------------------------------------------*
           05  w-def-tip-ope              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Nome personalizzazione di default                     *
      *        *-------------------------------------------------------*
           05  w-def-nom-prs              pic  x(32)                  .
      *        *-------------------------------------------------------*
      *        * Numero progressivo di default                         *
      *        *-------------------------------------------------------*
           05  w-def-num-prg              pic  9(03)                  .

      *    *===========================================================*
      *    * Work per records di [auc] 'dps'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/waucdps0.cpw"                   .

      *    *===========================================================*
      *    * Work-area per bufferizzazione testata                     *
      *    *-----------------------------------------------------------*
       01  w-tes.
      *        *-------------------------------------------------------*
      *        * Valori chiave                                         *
      *        *-------------------------------------------------------*
           05  w-tes-val-key.
               10  w-tes-top-prs          pic  x(01)                  .
               10  w-tes-cod-prs          pic  x(32)                  .
               10  w-tes-prg-prs          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
               10  w-tes-des-prs          pic  x(80)                  .
               10  w-tes-tip-prs          pic  x(01)                  .
               10  w-tes-car-prs          pic  9(02)                  .
               10  w-tes-dec-prs          pic  9(01)                  .
               10  w-tes-sgn-prs          pic  x(01)                  .
               10  w-tes-som-prs          pic  x(01)                  .
               10  w-tes-alx-prs          pic  x(80)                  .
               10  w-tes-com-prs.
                   15  w-tes-rco-prs
                               occurs 16  pic  x(80)                  .
               10  w-tes-alf-prs          pic  x(80)                  .
               10  w-tes-num-prs          pic s9(13)v9(05) trailing
                                                           separate
                                                           character  .
               10  w-tes-dat-prs          pic  9(07)                  .
               10  w-tes-txt-prs.
                   15  filler  occurs 400 pic  x(01)                  .

      *    *===========================================================*
      *    * Work per clonazioni                                       *
      *    *-----------------------------------------------------------*
       01  w-clo.
      *        *-------------------------------------------------------*
      *        * Work per clonazione personalizzazione                 *
      *        *-------------------------------------------------------*
           05  w-clo-prs.
      *            *---------------------------------------------------*
      *            * Tipo operazione per salvataggio                   *
      *            *---------------------------------------------------*
               10  w-clo-prs-top-prs      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Record valori attuali di testata                  *
      *            *---------------------------------------------------*
               10  w-clo-prs-wte-001.
                   15  filler occurs 2048 pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Tipo operazione                                       *
      *        *-------------------------------------------------------*
           05  w-sav-tip-ope              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Nome personalizzazione                                *
      *        *-------------------------------------------------------*
           05  w-sav-nom-prs              pic  x(32)                  .
      *        *-------------------------------------------------------*
      *        * Tipo personalizzazione                                *
      *        *-------------------------------------------------------*
           05  w-sav-def-tip              pic  x(01)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo operazione                            *
      *        *-------------------------------------------------------*
           05  w-exp-tip-ope.
               10  w-exp-tip-ope-num      pic  9(02) value 02         .
               10  w-exp-tip-ope-lun      pic  9(02) value 35         .
               10  w-exp-tip-ope-tbl.
                   15  filler             pic  x(35) value
                            "Attribuzione del valore            "     .
                   15  filler             pic  x(35) value
                            "Definizione della personalizzazione"     .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo personalizzazione                     *
      *        *-------------------------------------------------------*
           05  w-exp-def-tip.
               10  w-exp-def-tip-num      pic  9(02) value 03         .
               10  w-exp-def-tip-lun      pic  9(02) value 12         .
               10  w-exp-def-tip-tbl.
                   15  filler             pic  x(12) value
                            "Alfanumerica"                            .
                   15  filler             pic  x(12) value
                            "Numerica    "                            .
                   15  filler             pic  x(12) value
                            "Data        "                            .
      *        *-------------------------------------------------------*
      *        * Work per : Singola/multipla                           *
      *        *-------------------------------------------------------*
           05  w-exp-def-som.
               10  w-exp-def-som-num      pic  9(02) value 02         .
               10  w-exp-def-som-lun      pic  9(02) value 10         .
               10  w-exp-def-som-tbl.
                   15  filler             pic  x(10) value
                            "Singola   "                              .
                   15  filler             pic  x(10) value
                            "Multipla  "                              .

      *    *===========================================================*
      *    * Work per subroutines di Find                              *
      *    *-----------------------------------------------------------*
       01  w-fnd.
      *        *-------------------------------------------------------*
      *        * Work per Find su archivio personalizzazioni           *
      *        *-------------------------------------------------------*
           05  w-fnd-arc-prs.
               10  w-fnd-arc-prs-sel      pic  x(01)                  .
               10  w-fnd-arc-prs-ope      pic  x(01)                  .
               10  w-fnd-arc-prs-val.
                   15  w-fnd-arc-prs-nom  pic  x(32)                  .
                   15  w-fnd-arc-prs-prg  pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area per test se blanks embedded                     *
      *    *-----------------------------------------------------------*
       01  w-bla-emb.
           05  w-bla-emb-flg              pic  x(01)                  .
           05  w-bla-emb-str.
               10  w-bla-emb-chr occurs 40
                                          pic  x(01)                  .
           05  w-bla-emb-ctr              pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per chiave personalizzazione                    *
      *    *-----------------------------------------------------------*
       01  w-key-prs.
      *        *-------------------------------------------------------*
      *        * Chiave completa                                       *
      *        *-------------------------------------------------------*
           05  w-key-prs-key              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Porzione precedente l'eventuale chiocciola            *
      *        *-------------------------------------------------------*
           05  w-key-prs-pre              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Porzione seguente l'eventuale chiocciola              *
      *        *-------------------------------------------------------*
           05  w-key-prs-pos              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Puntatori di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-key-prs-pnt              pic  9(02)                  .
           05  w-key-prs-lun              pic  9(02)                  .

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
      *              * Tasto di funzione Pf4 :                         *
      *              *  - se Impostazione chiave    : non abilitato    *
      *              *  - se Inserimento            : non abilitato    *
      *              *  - se Modifica               : abilitato solo   *
      *              *                                se nessuna mo-   *
      *              *                                difica           *
      *              *  - se Visualizzazione        : abilitato solo   *
      *              *                                se nessuna mo-   *
      *              *                                difica           *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    not  = "K"    and
                    (w-cnt-mfu-tip-fun    =    "M"   or
                     w-cnt-mfu-tip-fun    =    "V"    ) and
                     w-cnt-acc-flg-aum    =    spaces
                     move  "[4] "         to   v-pfk (18)             .
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
           move      "Conferma operazione di annullamento (S/N) ?"
                                          to   v-not                  .
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
       exe-acc-cmp-900.
      *              *-------------------------------------------------*
      *              * Se Pf4                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        v-key                not  = "[4] "
                     go to exe-acc-cmp-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio tipo operazione                 *
      *                  *---------------------------------------------*
           move      w-tes-top-prs        to   w-clo-prs-top-prs      .
      *                  *---------------------------------------------*
      *                  * Salvataggio valori attuali di testata       *
      *                  *---------------------------------------------*
           move      w-tes-val-aep (1)    to   w-clo-prs-wte-001      .
      *                  *---------------------------------------------*
      *                  * Set del flag                                *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-dup-rec-flg      .
      *                  *---------------------------------------------*
      *                  * Forzatura del tasto Exit                    *
      *                  *---------------------------------------------*
           move      "EXIT"               to   v-key                  .
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
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      66                   to   v-pos                  .
           if        w-cnt-mfu-tip-fun    =    "I"
                     move  "    Inserimento"
                                          to   v-alf
           else if   w-cnt-mfu-tip-fun    =    "M"
                     move  "       Modifica"
                                          to   v-alf
           else if   w-cnt-mfu-tip-fun    =    "V"
                     move  "Visualizzazione"
                                          to   v-alf
           else      move  spaces         to   v-alf                  .
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
      *              * Preparazione iniziale dei valori di defaults    *
      *              * generali                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo operazione                             *
      *                  *---------------------------------------------*
           move      spaces               to   w-def-tip-ope          .
      *                  *---------------------------------------------*
      *                  * Nome personalizzazione                      *
      *                  *---------------------------------------------*
           move      spaces               to   w-def-nom-prs          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo di default               *
      *                  *---------------------------------------------*
           move      zero                 to   w-def-num-prg          .
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
      *              * Open [auc]                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione pathname per [auc]             *
      *                  *---------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "fpx"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "auc"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   f-auc-pat              .
      *                  *---------------------------------------------*
      *                  * Operazione di i-o di Open                   *
      *                  *---------------------------------------------*
           open      i-o    auc                                       .
       rou-opn-fls-100.
      *              *-------------------------------------------------*
      *              * Open [prs]                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione pathname per [prs]             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Pathname di base in s-pat               *
      *                      *-----------------------------------------*
           move      ".F"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Concatenamento                          *
      *                      *-----------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      s-pat                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "prs"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "C9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   f-prs-pat              .
      *                  *---------------------------------------------*
      *                  * Funzione open                               *
      *                  *---------------------------------------------*
           open      i-o    prs                                       .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * Close [auc]                                     *
      *              *-------------------------------------------------*
           close     auc                                              .
       rou-cls-fls-100.
      *              *-------------------------------------------------*
      *              * Close [prs]                                     *
      *              *-------------------------------------------------*
           close     prs                                              .
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
      *                  * Tipo operazione                             *
      *                  *---------------------------------------------*
           perform   acc-tip-ope-000      thru acc-tip-ope-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
       acc-key-reg-200.
      *                  *---------------------------------------------*
      *                  * Nome personalizzazione                      *
      *                  *---------------------------------------------*
           perform   acc-nom-prs-000      thru acc-nom-prs-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
           if        v-key                =    "UP  "
                     go to acc-key-reg-100.
       acc-key-reg-300.
      *                  *---------------------------------------------*
      *                  * Numero progressivo                          *
      *                  *---------------------------------------------*
           perform   acc-num-prg-000      thru acc-num-prg-999        .
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
      *              * Tipo operazione                                 *
      *              *-------------------------------------------------*
           perform   vis-tip-ope-000      thru vis-tip-ope-999        .
      *              *-------------------------------------------------*
      *              * Nome personalizzazione                          *
      *              *-------------------------------------------------*
           perform   vis-nom-prs-000      thru vis-nom-prs-999        .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        w-tes-top-prs        =    "D"
                     go to vis-key-reg-200
           else if   w-tes-top-prs        =    "V"
                     go to vis-key-reg-400
           else      go to vis-key-reg-999.
       vis-key-reg-200.
      *              *-------------------------------------------------*
      *              * Se operazione di definizione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-key-reg-999.
       vis-key-reg-400.
      *              *-------------------------------------------------*
      *              * Se operazione di attribuzione valore            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Descrizione personalizzazione               *
      *                  *---------------------------------------------*
           perform   vis-des-prs-000      thru vis-des-prs-999        .
      *                  *---------------------------------------------*
      *                  * Tipo personalizzazione                      *
      *                  *---------------------------------------------*
           perform   vis-def-tip-000      thru vis-def-tip-999        .
      *                  *---------------------------------------------*
      *                  * Caratteri personalizzazione                 *
      *                  *---------------------------------------------*
           perform   vis-def-car-000      thru vis-def-car-999        .
      *                  *---------------------------------------------*
      *                  * Decimali personalizzazione                  *
      *                  *---------------------------------------------*
           perform   vis-def-dec-000      thru vis-def-dec-999        .
      *                  *---------------------------------------------*
      *                  * Segno algebrico personalizzazione           *
      *                  *---------------------------------------------*
           perform   vis-def-sgn-000      thru vis-def-sgn-999        .
      *                  *---------------------------------------------*
      *                  * Singola o multipla                          *
      *                  *---------------------------------------------*
           perform   vis-def-som-000      thru vis-def-som-999        .
      *                  *---------------------------------------------*
      *                  * Numero progressivo                          *
      *                  *---------------------------------------------*
           perform   vis-num-prg-000      thru vis-num-prg-999        .
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
      *              * Erase linee impegnate                           *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Tipo operazione                                 *
      *              *-------------------------------------------------*
           perform   pmt-tip-ope-000      thru pmt-tip-ope-999        .
      *              *-------------------------------------------------*
      *              * Nome personalizzazione                          *
      *              *-------------------------------------------------*
           perform   pmt-nom-prs-000      thru pmt-nom-prs-999        .
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
      *              *-------------------------------------------------*
      *              * Descrizione personalizzazione                   *
      *              *-------------------------------------------------*
           perform   pmt-des-prs-000      thru pmt-des-prs-999        .
      *              *-------------------------------------------------*
      *              * Linea di trattini                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Literal per parametri di personalizzazione      *
      *              *-------------------------------------------------*
           perform   pmt-lit-prm-000      thru pmt-lit-prm-999        .
      *              *-------------------------------------------------*
      *              * Tipo personalizzazione                          *
      *              *-------------------------------------------------*
           perform   pmt-def-tip-000      thru pmt-def-tip-999        .
      *              *-------------------------------------------------*
      *              * Caratteri personalizzazione                     *
      *              *-------------------------------------------------*
           perform   pmt-def-car-000      thru pmt-def-car-999        .
      *              *-------------------------------------------------*
      *              * Decimali personalizzazione                      *
      *              *-------------------------------------------------*
           perform   pmt-def-dec-000      thru pmt-def-dec-999        .
      *              *-------------------------------------------------*
      *              * Segno algebrico personalizzazione               *
      *              *-------------------------------------------------*
           perform   pmt-def-sgn-000      thru pmt-def-sgn-999        .
      *              *-------------------------------------------------*
      *              * Personalizzazione singola o multipla            *
      *              *-------------------------------------------------*
           perform   pmt-def-som-000      thru pmt-def-som-999        .
      *              *-------------------------------------------------*
      *              * Linea di trattini                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Numero progressivo                              *
      *              *-------------------------------------------------*
           perform   pmt-num-prg-000      thru pmt-num-prg-999        .
      *              *-------------------------------------------------*
      *              * Linea di trattini                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Valore personalizzazione                        *
      *              *-------------------------------------------------*
           perform   pmt-val-prs-000      thru pmt-val-prs-999        .
       pmt-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Tipo operazione               *
      *    *-----------------------------------------------------------*
       pmt-tip-ope-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo operazione                :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-tip-ope-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Nome personalizzazione        *
      *    *-----------------------------------------------------------*
       pmt-nom-prs-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Nome personalizzazione         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-nom-prs-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Descrizione personalizzazione *
      *    *-----------------------------------------------------------*
       pmt-des-prs-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      26                   to   v-pos                  .
           move      "Descrizione personalizzazione "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-des-prs-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Literal per parametri di per- *
      *    * sonalizzazione                                            *
      *    *-----------------------------------------------------------*
       pmt-lit-prm-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      26                   to   v-pos                  .
           move      " Parametri personalizzazione  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-lit-prm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Tipo personalizzazione        *
      *    *-----------------------------------------------------------*
       pmt-def-tip-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo      :"        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-def-tip-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Caratteri personalizzazione   *
      *    *-----------------------------------------------------------*
       pmt-def-car-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Caratteri :"        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-def-car-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Decimali personalizzazione    *
      *    *-----------------------------------------------------------*
       pmt-def-dec-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Decimali  :"        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-def-dec-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Segno algebrico personalizza- *
      *    * zione                                                     *
      *    *-----------------------------------------------------------*
       pmt-def-sgn-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Segno     :"        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-def-sgn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Singola o multipla            *
      *    *-----------------------------------------------------------*
       pmt-def-som-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      22                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      47                   to   v-pos                  .
           move      "Singola o multipla   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-def-som-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Numero progressivo            *
      *    *-----------------------------------------------------------*
       pmt-num-prg-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero progressivo :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-num-prg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Valore personalizzazione      *
      *    *-----------------------------------------------------------*
       pmt-val-prs-000.
      *              *-------------------------------------------------*
      *              * Prompt                                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      29                   to   v-pos                  .
           move      "Valore personalizzazione"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Riga numeri 20                                  *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "123456789 123456789 123456789 123456789 123456789 
      -              "123456789 123456789 123456789 "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-val-prs-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Tipo operazione               *
      *    *-----------------------------------------------------------*
       acc-tip-ope-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuale preparazione default              *
      *                  *---------------------------------------------*
           if        w-tes-top-prs        not  = spaces
                     go to acc-tip-ope-050.
           move      w-def-tip-ope        to   w-tes-top-prs          .
       acc-tip-ope-050.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-top-prs        to   w-sav-tip-ope          .
       acc-tip-ope-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-ope-lun    to   v-car                  .
           move      w-exp-tip-ope-num    to   v-ldt                  .
           move      "AD#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-ope-tbl    to   v-txt                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      04                   to   v-lin                  .
           move      34                   to   v-pos                  .
           if        w-tes-top-prs        =    "V"
                     move  01             to   v-num
           else if   w-tes-top-prs        =    "D"
                     move  02             to   v-num
           else      move  01             to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-tip-ope-999.
       acc-tip-ope-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "V"            to   w-tes-top-prs
           else if   v-num                =    02
                     move  "D"            to   w-tes-top-prs
           else      move  spaces         to   w-tes-top-prs          .
       acc-tip-ope-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        w-tes-top-prs        not  = "D" and
                     w-tes-top-prs        not  = "V"
                     go to acc-tip-ope-100.
       acc-tip-ope-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-ope-610.
      *                  *---------------------------------------------*
      *                  * Eventuali normalizzazioni                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se il valore e' cambiato           *
      *                      *-----------------------------------------*
           if        w-tes-top-prs        =    w-sav-tip-ope
                     go to acc-tip-ope-650.
      *                      *-----------------------------------------*
      *                      * Se il valore e' cambiato                *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se il valore precedente era Spaces  *
      *                          * no normalizzazioni                  *
      *                          *-------------------------------------*
           if        w-sav-tip-ope        =    spaces
                     go to acc-tip-ope-650.
      *                          *-------------------------------------*
      *                          * Se il nome della personalizzazione  *
      *                          * e' a Spaces : no normalizzazioni    *
      *                          *-------------------------------------*
           if        w-tes-cod-prs        =    spaces
                     go to acc-tip-ope-650.
      *                          *-------------------------------------*
      *                          * Se il valore attuale indica che si  *
      *                          * tratta di un valore : no normaliz-  *
      *                          * zazioni                             *
      *                          *-------------------------------------*
           if        w-tes-top-prs        =    "V"
                     go to acc-tip-ope-650.
      *                          *-------------------------------------*
      *                          * Normalizzazioni                     *
      *                          *-------------------------------------*
       acc-tip-ope-630.
      *                              *---------------------------------*
      *                              * Numero progressivo              *
      *                              *---------------------------------*
           if        w-tes-prg-prs        =    zero
                     go to acc-tip-ope-650.
           move      zero                 to   w-tes-prg-prs          .
           perform   vis-num-prg-000      thru vis-num-prg-999        .
       acc-tip-ope-650.
      *                  *---------------------------------------------*
      *                  * Prosecuzione dopo normalizzazioni           *
      *                  *---------------------------------------------*
           go to     acc-tip-ope-800.
       acc-tip-ope-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-tip-ope-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-tip-ope-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-tip-ope-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-tip-ope-999.
       acc-tip-ope-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Tipo operazione            *
      *    *-----------------------------------------------------------*
       vis-tip-ope-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-ope-lun    to   v-car                  .
           move      w-exp-tip-ope-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-ope-tbl    to   v-txt                  .
           move      04                   to   v-lin                  .
           move      34                   to   v-pos                  .
           if        w-tes-top-prs        =    "V"
                     move  01             to   v-num
           else if   w-tes-top-prs        =    "D"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-ope-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Nome personalizzazione        *
      *    *-----------------------------------------------------------*
       acc-nom-prs-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuale preparazione default              *
      *                  *---------------------------------------------*
           if        w-tes-cod-prs        not  = spaces
                     go to acc-nom-prs-050.
           move      w-def-nom-prs        to   w-tes-cod-prs          .
       acc-nom-prs-050.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-cod-prs        to   w-sav-nom-prs          .
       acc-nom-prs-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      05                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-tes-cod-prs        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-nom-prs-999.
       acc-nom-prs-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-cod-prs          .
       acc-nom-prs-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-nom-prs-400.
      *                  *---------------------------------------------*
      *                  * Find su archivio [prs]                      *
      *                  *---------------------------------------------*
           move      w-tes-top-prs        to   w-fnd-arc-prs-ope      .
           move      w-tes-cod-prs        to   w-fnd-arc-prs-nom      .
           perform   fnd-arc-prs-000      thru fnd-arc-prs-999        .
           if        w-fnd-arc-prs-sel    not  = spaces
                     go to acc-nom-prs-100.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo operazione  *
      *                  *---------------------------------------------*
           if        w-tes-top-prs        =    "D"
                     go to acc-nom-prs-325
           else      go to acc-nom-prs-350.
       acc-nom-prs-325.
      *                  *---------------------------------------------*
      *                  * Se operazione di definizione personalizz.   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Memorizza codice selezionato            *
      *                      *-----------------------------------------*
           move      w-fnd-arc-prs-nom    to   w-tes-cod-prs          .
      *                      *-----------------------------------------*
      *                      * Visualizza codice selezionato           *
      *                      *-----------------------------------------*
           perform   vis-nom-prs-000      thru vis-nom-prs-999        .
      *                      *-----------------------------------------*
      *                      * Forzatura del tasto Return              *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Riciclo a reimpostazione                *
      *                      *-----------------------------------------*
           go to     acc-nom-prs-100.
       acc-nom-prs-350.
      *                  *---------------------------------------------*
      *                  * Se operazione di attribuzione valore        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Memorizza codice selezionato            *
      *                      *-----------------------------------------*
           move      w-fnd-arc-prs-nom    to   w-tes-cod-prs          .
      *                      *-----------------------------------------*
      *                      * Visualizza codice selezionato           *
      *                      *-----------------------------------------*
           perform   vis-nom-prs-000      thru vis-nom-prs-999        .
      *                      *-----------------------------------------*
      *                      * Memorizza nr progressivo selezionato    *
      *                      *-----------------------------------------*
           move      w-fnd-arc-prs-prg    to   w-tes-prg-prs          .
      *                      *-----------------------------------------*
      *                      * Visualizza nr progressivo selezionato   *
      *                      *-----------------------------------------*
           perform   vis-num-prg-000      thru vis-num-prg-999        .
      *                      *-----------------------------------------*
      *                      * Forzatura del tasto Return              *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Riciclo a reimpostazione                *
      *                      *-----------------------------------------*
           go to     acc-nom-prs-100.
       acc-nom-prs-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           move      w-tes-cod-prs        to   w-bla-emb-str          .
           perform   tst-bla-emb-000      thru tst-bla-emb-999        .
           if        w-bla-emb-flg        not  = spaces
                     go to acc-nom-prs-100.
           if        w-tes-cod-prs        not  = spaces
                     go to acc-nom-prs-600.
           if        v-key                not  = "UP  "
                     go to acc-nom-prs-100.
       acc-nom-prs-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo operazione  *
      *                  *---------------------------------------------*
           if        w-tes-top-prs        =    "D"
                     go to acc-nom-prs-650
           else      go to acc-nom-prs-700.
       acc-nom-prs-650.
      *                  *---------------------------------------------*
      *                  * Se operazione di definizione                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazioni                         *
      *                      *-----------------------------------------*
       acc-nom-prs-670.
      *                              *---------------------------------*
      *                              * Numero progressivo              *
      *                              *---------------------------------*
           if        w-tes-prg-prs        =    zero
                     go to acc-nom-prs-690.
           move      zero                 to   w-tes-prg-prs          .
           perform   vis-num-prg-000      thru vis-num-prg-999        .
       acc-nom-prs-690.
      *                  *---------------------------------------------*
      *                  * Prosecuzione dopo normalizzazioni           *
      *                  *---------------------------------------------*
           go to     acc-nom-prs-800.
       acc-nom-prs-700.
      *                  *---------------------------------------------*
      *                  * Se operazione di attribuzione valore        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura della definizione               *
      *                      *-----------------------------------------*
           if        w-tes-cod-prs        =    spaces
                     go to acc-nom-prs-740.
           move      "dps "               to   auc-tre                .
           move      w-tes-cod-prs        to   auc-kre                .
           read      auc   with no lock
                           invalid key
                           go to   acc-nom-prs-760.
       acc-nom-prs-720.
      *                      *-----------------------------------------*
      *                      * Se definizione esistente                *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Record letto in comodo di ridefini- *
      *                          * zione                               *
      *                          *-------------------------------------*
           move      auc-dat              to   w-dps                  .
      *                          *-------------------------------------*
      *                          * Bufferizzazione valori letti        *
      *                          *-------------------------------------*
           move      w-dps-des-dps        to   w-tes-des-prs (1)      .
           move      w-dps-tip-dps        to   w-tes-tip-prs (1)      .
           move      w-dps-car-dps        to   w-tes-car-prs (1)      .
           move      w-dps-dec-dps        to   w-tes-dec-prs (1)      .
           move      w-dps-sgn-dps        to   w-tes-sgn-prs (1)      .
           move      w-dps-som-dps        to   w-tes-som-prs (1)      .
           move      w-dps-alx-dps        to   w-tes-alx-prs (1)      .
           move      w-dps-com-dps        to   w-tes-com-prs (1)      .
      *                          *-------------------------------------*
      *                          * Visualizzazione valori letti        *
      *                          *-------------------------------------*
           perform   vis-des-prs-000      thru vis-des-prs-999        .
           perform   vis-def-tip-000      thru vis-def-tip-999        .
           perform   vis-def-car-000      thru vis-def-car-999        .
           perform   vis-def-dec-000      thru vis-def-dec-999        .
           perform   vis-def-sgn-000      thru vis-def-sgn-999        .
           perform   vis-def-som-000      thru vis-def-som-999        .
      *                          *-------------------------------------*
      *                          * Prosecuzione                        *
      *                          *-------------------------------------*
           go to     acc-nom-prs-800.
       acc-nom-prs-740.
      *                      *-----------------------------------------*
      *                      * Se nome personalizzazione a spaces      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Normalizzazione valori              *
      *                          *-------------------------------------*
           move      spaces               to   w-tes-des-prs (1)      .
           move      spaces               to   w-tes-tip-prs (1)      .
           move      zero                 to   w-tes-car-prs (1)      .
           move      zero                 to   w-tes-dec-prs (1)      .
           move      spaces               to   w-tes-sgn-prs (1)      .
           move      spaces               to   w-tes-som-prs (1)      .
           move      spaces               to   w-tes-alx-prs (1)      .
           move      spaces               to   w-tes-com-prs (1)      .
      *                          *-------------------------------------*
      *                          * Visualizzazione valori normalizzati *
      *                          *-------------------------------------*
           perform   vis-des-prs-000      thru vis-des-prs-999        .
           perform   vis-def-tip-000      thru vis-def-tip-999        .
           perform   vis-def-car-000      thru vis-def-car-999        .
           perform   vis-def-dec-000      thru vis-def-dec-999        .
           perform   vis-def-sgn-000      thru vis-def-sgn-999        .
           perform   vis-def-som-000      thru vis-def-som-999        .
      *                          *-------------------------------------*
      *                          * Prosecuzione                        *
      *                          *-------------------------------------*
           go to     acc-nom-prs-800.
       acc-nom-prs-760.
      *                      *-----------------------------------------*
      *                      * Se definizione non esistente            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Normalizzazione valori              *
      *                          *-------------------------------------*
           move      all   "."            to   w-tes-des-prs (1)      .
       acc-nom-prs-780.
           move      spaces               to   w-tes-tip-prs (1)      .
           move      zero                 to   w-tes-car-prs (1)      .
           move      zero                 to   w-tes-dec-prs (1)      .
           move      spaces               to   w-tes-sgn-prs (1)      .
           move      spaces               to   w-tes-som-prs (1)      .
           move      spaces               to   w-tes-alx-prs (1)      .
           move      spaces               to   w-tes-com-prs (1)      .
      *                          *-------------------------------------*
      *                          * Visualizzazione valori normalizzati *
      *                          *-------------------------------------*
           perform   vis-des-prs-000      thru vis-des-prs-999        .
           perform   vis-def-tip-000      thru vis-def-tip-999        .
           perform   vis-def-car-000      thru vis-def-car-999        .
           perform   vis-def-dec-000      thru vis-def-dec-999        .
           perform   vis-def-sgn-000      thru vis-def-sgn-999        .
           perform   vis-def-som-000      thru vis-def-som-999        .
      *                          *-------------------------------------*
      *                          * Reimpostazione                      *
      *                          *-------------------------------------*
           go to     acc-nom-prs-100.
       acc-nom-prs-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-nom-prs-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-nom-prs-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-nom-prs-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-nom-prs-999.
       acc-nom-prs-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Nome personalizzazione     *
      *    *-----------------------------------------------------------*
       vis-nom-prs-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-tes-cod-prs        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-nom-prs-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Numero progressivo            *
      *    *-----------------------------------------------------------*
       acc-num-prg-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore da accettare ed eventuale    *
      *                  * normalizzazione se non e' da accettare      *
      *                  *---------------------------------------------*
           if        w-tes-top-prs        =    "V"
                     go to acc-num-prg-075.
       acc-num-prg-025.
           if        w-tes-prg-prs        =    zero
                     go to acc-num-prg-050.
           move      zero                 to   w-tes-prg-prs          .
           perform   vis-num-prg-000      thru vis-num-prg-999        .
       acc-num-prg-050.
           go to     acc-num-prg-999.
       acc-num-prg-075.
           if        w-tes-som-prs (1)    not  = "M"
                     go to acc-num-prg-025.
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        w-tes-prg-prs        not  = zero
                     go to acc-num-prg-100.
           move      w-def-num-prg        to   w-tes-prg-prs          .
       acc-num-prg-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      17                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-tes-prg-prs        to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-num-prg-999.
       acc-num-prg-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-prg-prs          .
       acc-num-prg-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        w-tes-prg-prs        not  = zero
                     go to acc-num-prg-600.
           if        v-key                not  = "UP  "
                     go to acc-num-prg-100.
       acc-num-prg-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-num-prg-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-num-prg-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-num-prg-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-num-prg-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-num-prg-999.
       acc-num-prg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Numero progressivo         *
      *    *-----------------------------------------------------------*
       vis-num-prg-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      17                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-tes-prg-prs        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-prg-999.
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
           move      "[Exit] per fine visualizzazione :"
                                          to   v-not                  .
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
           move      "Conferma impostazioni (S/N/E) ?"
                                          to   v-not                  .
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
      *                  * Descrizione personalizzazione               *
      *                  *---------------------------------------------*
           perform   acc-des-prs-000      thru acc-des-prs-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-200.
      *                  *---------------------------------------------*
      *                  * Tipo personalizzazione                      *
      *                  *---------------------------------------------*
           perform   acc-def-tip-000      thru acc-def-tip-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-100.
       acc-tes-reg-300.
      *                  *---------------------------------------------*
      *                  * Caratteri personalizzazione                 *
      *                  *---------------------------------------------*
           perform   acc-def-car-000      thru acc-def-car-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-200.
       acc-tes-reg-400.
      *                  *---------------------------------------------*
      *                  * Decimali personalizzazione                  *
      *                  *---------------------------------------------*
           perform   acc-def-dec-000      thru acc-def-dec-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-300.
       acc-tes-reg-500.
      *                  *---------------------------------------------*
      *                  * Segno algebrico personalizzazione           *
      *                  *---------------------------------------------*
           perform   acc-def-sgn-000      thru acc-def-sgn-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-400.
       acc-tes-reg-600.
      *                  *---------------------------------------------*
      *                  * Singola o multipla                          *
      *                  *---------------------------------------------*
           perform   acc-def-som-000      thru acc-def-som-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-500.
       acc-tes-reg-700.
      *                  *---------------------------------------------*
      *                  * Valore della personalizzazione              *
      *                  *---------------------------------------------*
           perform   acc-val-prs-000      thru acc-val-prs-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-600.
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
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        w-tes-top-prs        =    "D"
                     go to vis-tes-reg-200
           else if   w-tes-top-prs        =    "V"
                     go to vis-tes-reg-400
           else      go to vis-tes-reg-999.
       vis-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Se operazione di definizione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Descrizione personalizzazione               *
      *                  *---------------------------------------------*
           perform   vis-des-prs-000      thru vis-des-prs-999        .
      *                  *---------------------------------------------*
      *                  * Tipo personalizzazione                      *
      *                  *---------------------------------------------*
           perform   vis-def-tip-000      thru vis-def-tip-999        .
      *                  *---------------------------------------------*
      *                  * Caratteri personalizzazione                 *
      *                  *---------------------------------------------*
           perform   vis-def-car-000      thru vis-def-car-999        .
      *                  *---------------------------------------------*
      *                  * Decimali personalizzazione                  *
      *                  *---------------------------------------------*
           perform   vis-def-dec-000      thru vis-def-dec-999        .
      *                  *---------------------------------------------*
      *                  * Segno algebrico personalizzazione           *
      *                  *---------------------------------------------*
           perform   vis-def-sgn-000      thru vis-def-sgn-999        .
      *                  *---------------------------------------------*
      *                  * Singola o multipla                          *
      *                  *---------------------------------------------*
           perform   vis-def-som-000      thru vis-def-som-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-tes-reg-999.
       vis-tes-reg-400.
      *              *-------------------------------------------------*
      *              * Se operazione di attribuzione valore            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore della personalizzazione              *
      *                  *---------------------------------------------*
           perform   vis-val-prs-000      thru vis-val-prs-999        .
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
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Descrizione della persona-   *
      *    *                              lizzazione                   *
      *    *-----------------------------------------------------------*
       acc-des-prs-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore da accettare                 *
      *                  *---------------------------------------------*
           if        w-tes-top-prs        not  = "D"
                     go to acc-des-prs-999.
       acc-des-prs-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tes-des-prs (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-des-prs-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-des-prs-999.
       acc-des-prs-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-des-prs (1)      .
       acc-des-prs-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        w-tes-des-prs (1)    =    spaces
                     go to acc-des-prs-100.
       acc-des-prs-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-des-prs-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-des-prs-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-des-prs-100.
       acc-des-prs-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione della perso-  *
      *    *                                 nalizzazione              *
      *    *-----------------------------------------------------------*
       vis-des-prs-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-tes-des-prs (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-prs-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Tipo personalizzazione       *
      *    *-----------------------------------------------------------*
       acc-def-tip-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore da accettare                 *
      *                  *---------------------------------------------*
           if        w-tes-top-prs        not  = "D"
                     go to acc-def-tip-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-tip-prs (1)    to   w-sav-def-tip          .
       acc-def-tip-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-def-tip-lun    to   v-car                  .
           move      w-exp-def-tip-num    to   v-ldt                  .
           move      "AND#"               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      w-exp-def-tip-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      12                   to   v-lin                  .
           move      13                   to   v-pos                  .
           if        w-tes-tip-prs (1)    =    "A"
                     move  01             to   v-num
           else if   w-tes-tip-prs (1)    =    "N"
                     move  02             to   v-num
           else if   w-tes-tip-prs (1)    =    "D"
                     move  03             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-def-tip-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-def-tip-999.
       acc-def-tip-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "A"            to   w-tes-tip-prs (1)
           else if   v-num                =    02
                     move  "N"            to   w-tes-tip-prs (1)
           else if   v-num                =    03
                     move  "D"            to   w-tes-tip-prs (1)
           else      move  zero           to   w-tes-tip-prs (1)      .
       acc-def-tip-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        w-tes-tip-prs (1)    =    "A" or
                     w-tes-tip-prs (1)    =    "N" or
                     w-tes-tip-prs (1)    =    "D"
                     go to acc-def-tip-600.
           if        w-tes-tip-prs (1)    not  = spaces
                     go to acc-def-tip-100.
           if        v-key                not  = "UP  "
                     go to acc-def-tip-100.
       acc-def-tip-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazioni se valore cambiato          *
      *                  *---------------------------------------------*
           if        w-tes-tip-prs (1)    =    w-sav-def-tip
                     go to acc-def-tip-650.
           if        w-sav-def-tip        =    spaces
                     go to acc-def-tip-650.
       acc-def-tip-620.
      *                      *-----------------------------------------*
      *                      * Caratteri della personalizzazione       *
      *                      *-----------------------------------------*
           if        w-tes-car-prs (1)    =    zero
                     go to acc-def-tip-630.
           move      zero                 to   w-tes-car-prs (1)      .
           perform   vis-def-car-000      thru vis-def-car-999        .
       acc-def-tip-630.
      *                      *-----------------------------------------*
      *                      * Decimali della personalizzazione        *
      *                      *-----------------------------------------*
           if        w-tes-dec-prs (1)    =    zero
                     go to acc-def-tip-640.
           move      zero                 to   w-tes-dec-prs (1)      .
           perform   vis-def-dec-000      thru vis-def-dec-999        .
       acc-def-tip-640.
      *                      *-----------------------------------------*
      *                      * Segno algebrico della personalizzazione *
      *                      *-----------------------------------------*
           if        w-tes-sgn-prs (1)    =    spaces
                     go to acc-def-tip-650.
           move      spaces               to   w-tes-sgn-prs (1)      .
           perform   vis-def-sgn-000      thru vis-def-sgn-999        .
       acc-def-tip-650.
      *                  *---------------------------------------------*
      *                  * Prosecuzione dopo normalizzazioni           *
      *                  *---------------------------------------------*
           go to     acc-def-tip-800.
       acc-def-tip-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-def-tip-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-def-tip-100.
       acc-def-tip-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Tipo personalizzazione    *
      *    *-----------------------------------------------------------*
       vis-def-tip-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-def-tip-lun    to   v-car                  .
           move      w-exp-def-tip-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-def-tip-tbl    to   v-txt                  .
           move      12                   to   v-lin                  .
           move      13                   to   v-pos                  .
           if        w-tes-tip-prs (1)    =    "A"
                     move  01             to   v-num
           else if   w-tes-tip-prs (1)    =    "N"
                     move  02             to   v-num
           else if   w-tes-tip-prs (1)    =    "D"
                     move  03             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-def-tip-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Caratteri personalizzazione  *
      *    *-----------------------------------------------------------*
       acc-def-car-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore da accettare                 *
      *                  *---------------------------------------------*
           if        w-tes-top-prs        not  = "D"
                     go to acc-def-car-999.
           if        w-tes-tip-prs (1)    not  = "A" and
                     w-tes-tip-prs (1)    not  = "N"
                     go to acc-def-car-999.
       acc-def-car-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      13                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      w-tes-car-prs (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-def-car-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-def-car-999.
       acc-def-car-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-car-prs (1)      .
       acc-def-car-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        w-tes-tip-prs (1)    =    "A" and
                     w-tes-car-prs (1)    >    80
                     go to acc-def-car-100.
           if        w-tes-tip-prs (1)    =    "N" and
                     w-tes-car-prs (1)    >    13
                     go to acc-def-car-100.
           if        w-tes-car-prs (1)    >    zero
                     go to acc-def-car-600.
           if        v-key                not  = "UP  "
                     go to acc-def-car-100.
       acc-def-car-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-def-car-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-def-car-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-def-car-100.
       acc-def-car-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Caratteri personalizzaz.  *
      *    *-----------------------------------------------------------*
       vis-def-car-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      13                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      w-tes-car-prs (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-def-car-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Decimali personalizzazione   *
      *    *-----------------------------------------------------------*
       acc-def-dec-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore da accettare                 *
      *                  *---------------------------------------------*
           if        w-tes-top-prs        not  = "D"
                     go to acc-def-dec-999.
           if        w-tes-tip-prs (1)    not  = "N"
                     go to acc-def-dec-999.
       acc-def-dec-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      14                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      w-tes-dec-prs (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-def-dec-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-def-dec-999.
       acc-def-dec-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-dec-prs (1)      .
       acc-def-dec-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        w-tes-dec-prs (1)    >    5
                     go to acc-def-dec-100.
       acc-def-dec-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-def-dec-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-def-dec-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-def-dec-100.
       acc-def-dec-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Decimali personalizzaz.   *
      *    *-----------------------------------------------------------*
       vis-def-dec-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      14                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      w-tes-dec-prs (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-def-dec-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Segno algebrico per la per-  *
      *    *                              sonalizzazione               *
      *    *-----------------------------------------------------------*
       acc-def-sgn-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore da accettare                 *
      *                  *---------------------------------------------*
           if        w-tes-top-prs        not  = "D"
                     go to acc-def-sgn-999.
           if        w-tes-tip-prs (1)    not  = "N"
                     go to acc-def-sgn-999.
       acc-def-sgn-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      15                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      w-tes-sgn-prs (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-def-sgn-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-def-sgn-999.
       acc-def-sgn-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-sgn-prs (1)      .
       acc-def-sgn-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        w-tes-sgn-prs (1)    not  = spaces and
                     w-tes-sgn-prs (1)    not  = "S"
                     go to acc-def-sgn-100.
       acc-def-sgn-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-def-sgn-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-def-sgn-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-def-sgn-100.
       acc-def-sgn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Segno algebrico per la    *
      *    *                                 personalizzazione         *
      *    *-----------------------------------------------------------*
       vis-def-sgn-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      w-tes-sgn-prs (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-def-sgn-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Singola o multipla           *
      *    *-----------------------------------------------------------*
       acc-def-som-000.
      *              *-------------------------------------------------*
      *              * Forzatura e visualizzazione a : 'Singola'       *
      *              *-------------------------------------------------*
           move      "S"                  to   w-tes-som-prs (1)      .
           perform   vis-def-som-000      thru vis-def-som-999        .
       acc-def-som-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Singola o multipla        *
      *    *-----------------------------------------------------------*
       vis-def-som-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-def-som-lun    to   v-car                  .
           move      w-exp-def-som-num    to   v-ldt                  .
           move      "SM#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      w-exp-def-som-tbl    to   v-txt                  .
           move      15                   to   v-lin                  .
           move      70                   to   v-pos                  .
           if        w-tes-som-prs (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-som-prs (1)    =    "M"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-def-som-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Valore personalizzazione     *
      *    *-----------------------------------------------------------*
       acc-val-prs-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore da accettare                 *
      *                  *---------------------------------------------*
           if        w-tes-top-prs        not  = "V"
                     go to acc-val-prs-999.
       acc-val-prs-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo di persona- *
      *                  * lizzazione                                  *
      *                  *---------------------------------------------*
           if        w-tes-tip-prs (1)    =    "A"
                     go to acc-val-prs-120
           else if   w-tes-tip-prs (1)    =    "N"
                     go to acc-val-prs-140
           else if   w-tes-tip-prs (1)    =    "D"
                     go to acc-val-prs-160.
       acc-val-prs-120.
      *                  *---------------------------------------------*
      *                  * Se di tipo alfanumerico                     *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      w-tes-car-prs (1)    to   v-car                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-tes-alf-prs (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           go to     acc-val-prs-180.
       acc-val-prs-140.
      *                  *---------------------------------------------*
      *                  * Se di tipo numerico                         *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      w-tes-car-prs (1)    to   v-car                  .
           move      w-tes-dec-prs (1)    to   v-dec                  .
           move      w-tes-sgn-prs (1)    to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-tes-num-prs (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           go to     acc-val-prs-180.
       acc-val-prs-160.
      *                  *---------------------------------------------*
      *                  * Se di tipo data                             *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      ">"                  to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-tes-dat-prs (1)    to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           go to     acc-val-prs-180.
       acc-val-prs-180.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-val-prs-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-val-prs-999.
       acc-val-prs-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        w-tes-tip-prs (1)    =    "N"
                     move  v-num          to   w-tes-num-prs (1)
           else if   w-tes-tip-prs (1)    =    "D"
                     move  v-dat          to   w-tes-dat-prs (1)
           else      move  v-alf          to   w-tes-alf-prs (1)      .
       acc-val-prs-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-val-prs-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-val-prs-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-val-prs-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-val-prs-100.
       acc-val-prs-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Valore personalizzazione  *
      *    *-----------------------------------------------------------*
       vis-val-prs-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo personalizzaz.  *
      *              *-------------------------------------------------*
           if        w-tes-tip-prs (1)    =    "A"
                     go to vis-val-prs-200
           else if   w-tes-tip-prs (1)    =    "N"
                     go to vis-val-prs-400
           else if   w-tes-tip-prs (1)    =    "D"
                     go to vis-val-prs-600
           else      go to vis-val-prs-999.
       vis-val-prs-200.
      *              *-------------------------------------------------*
      *              * Se personalizzazione di tipo alfanumerico       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione del valore alfanumerico     *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      w-tes-car-prs (1)    to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-tes-alf-prs (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-val-prs-999.
       vis-val-prs-400.
      *              *-------------------------------------------------*
      *              * Se personalizzazione di tipo numerico           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione del valore numerico         *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      w-tes-car-prs (1)    to   v-car                  .
           move      w-tes-dec-prs (1)    to   v-dec                  .
           move      w-tes-sgn-prs (1)    to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-tes-num-prs (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-val-prs-999.
       vis-val-prs-600.
      *              *-------------------------------------------------*
      *              * Se personalizzazione di tipo data               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione del valore data             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-tes-dat-prs (1)    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-val-prs-999.
       vis-val-prs-999.
           exit.

      *    *===========================================================*
      *    * Test se blanks embedded in w-bla-emb-str                  *
      *    *-----------------------------------------------------------*
       tst-bla-emb-000.
           move      spaces               to   w-bla-emb-flg          .
           if        w-bla-emb-str        =    spaces
                     go to tst-bla-emb-999.
           if        w-bla-emb-chr (1)    =    spaces
                     move  "#"            to   w-bla-emb-flg
                     go to tst-bla-emb-999.
           move      1                    to   w-bla-emb-ctr          .
       tst-bla-emb-100.
           add       1                    to   w-bla-emb-ctr          .
           if        w-bla-emb-ctr        >    40
                     go to tst-bla-emb-999.
           if        w-bla-emb-chr
                    (w-bla-emb-ctr)       not  = spaces
                     go to tst-bla-emb-100.
       tst-bla-emb-200.
           add       1                    to   w-bla-emb-ctr          .
           if        w-bla-emb-ctr        >    40
                     go to tst-bla-emb-999.
           if        w-bla-emb-chr
                    (w-bla-emb-ctr)       =    spaces
                     go to tst-bla-emb-200.
           move      "#"                  to   w-bla-emb-flg          .
       tst-bla-emb-999.
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
      *              * Test su tipo operazione                         *
      *              *-------------------------------------------------*
           if        w-tes-top-prs        =    "D"
                     go to cnt-tdo-key-200.
           if        w-tes-top-prs        =    "V"
                     go to cnt-tdo-key-500.
           if        w-tes-top-prs        =    spaces
                     go to cnt-tdo-key-120
           else      go to cnt-tdo-key-140.
       cnt-tdo-key-120.
           move      "ME"                 to   v-ope                  .
           move      "Manca il tipo operazione !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-key-flg      .
           go to     cnt-tdo-key-999.
       cnt-tdo-key-140.
           move      "ME"                 to   v-ope                  .
           move      "Tipo operazione errato !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-key-flg      .
           go to     cnt-tdo-key-999.
       cnt-tdo-key-200.
      *              *-------------------------------------------------*
      *              * Se operazione di definizione                    *
      *              *-------------------------------------------------*
       cnt-tdo-key-220.
      *                  *---------------------------------------------*
      *                  * Test su nome personalizzazione, se manca    *
      *                  *---------------------------------------------*
           if        w-tes-cod-prs        not  = spaces
                     go to cnt-tdo-key-240.
           move      "ME"                 to   v-ope                  .
           move      "Manca il nome della personalizzazione !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-key-flg      .
           go to     cnt-tdo-key-999.
       cnt-tdo-key-240.
      *                  *---------------------------------------------*
      *                  * Test su nome personalizzazione, se errato   *
      *                  *---------------------------------------------*
           move      w-tes-cod-prs        to   w-key-prs-key          .
           perform   sep-key-prs-000      thru sep-key-prs-999        .
           if        w-key-prs-pos        =    spaces
                     go to cnt-tdo-key-260.
           move      "ME"                 to   v-ope                  .
           move      "Nome della personalizzazione errato!"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-key-flg      .
           go to     cnt-tdo-key-999.
       cnt-tdo-key-260.
      *                  *---------------------------------------------*
      *                  * Fine controlli                              *
      *                  *---------------------------------------------*
           go to     cnt-tdo-key-300.
       cnt-tdo-key-300.
      *                  *---------------------------------------------*
      *                  * Eventuali normalizzazioni                   *
      *                  *---------------------------------------------*
       cnt-tdo-key-320.
      *                      *-----------------------------------------*
      *                      * Numero progressivo                      *
      *                      *-----------------------------------------*
           if        w-tes-prg-prs        =    zero
                     go to cnt-tdo-key-330.
           move      zero                 to   w-tes-prg-prs          .
           perform   vis-num-prg-000      thru vis-num-prg-999        .
       cnt-tdo-key-330.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     cnt-tdo-key-999.
       cnt-tdo-key-500.
      *              *-------------------------------------------------*
      *              * Se operazione di attribuzione valori            *
      *              *-------------------------------------------------*
       cnt-tdo-key-520.
      *                  *---------------------------------------------*
      *                  * Test su nome personalizzazione, se manca    *
      *                  *---------------------------------------------*
           if        w-tes-cod-prs        not  = spaces
                     go to cnt-tdo-key-540.
           move      "ME"                 to   v-ope                  .
           move      "Manca il nome della personalizzazione !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-key-flg      .
           go to     cnt-tdo-key-999.
       cnt-tdo-key-540.
      *                  *---------------------------------------------*
      *                  * Test su nome personalizzazione, se errato   *
      *                  *---------------------------------------------*
           move      w-tes-cod-prs        to   w-key-prs-key          .
           perform   sep-key-prs-000      thru sep-key-prs-999        .
           if        w-key-prs-pre        not  = spaces
                     go to cnt-tdo-key-560.
           move      "ME"                 to   v-ope                  .
           move      "Nome della personalizzazione errato!"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-key-flg      .
           go to     cnt-tdo-key-999.
       cnt-tdo-key-560.
      *                  *---------------------------------------------*
      *                  * Test su Numero progressivo                  *
      *                  *---------------------------------------------*
           if        w-tes-som-prs (1)    not  = "M"
                     go to cnt-tdo-key-580.
           if        w-tes-prg-prs        not  = zero
                     go to cnt-tdo-key-580.
           move      "ME"                 to   v-ope                  .
           move      "Manca il numero progressivo !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-key-flg      .
           go to     cnt-tdo-key-999.
       cnt-tdo-key-580.
      *                  *---------------------------------------------*
      *                  * Fine controlli                              *
      *                  *---------------------------------------------*
           go to     cnt-tdo-key-600.
       cnt-tdo-key-600.
      *                  *---------------------------------------------*
      *                  * Eventuali normalizzazioni                   *
      *                  *---------------------------------------------*
       cnt-tdo-key-620.
      *                      *-----------------------------------------*
      *                      * Numero progressivo                      *
      *                      *-----------------------------------------*
           if        w-tes-som-prs (1)    =    "M"
                     go to cnt-tdo-key-650.
           if        w-tes-prg-prs        =    zero
                     go to cnt-tdo-key-650.
           move      zero                 to   w-tes-prg-prs          .
           perform   vis-num-prg-000      thru vis-num-prg-999        .
       cnt-tdo-key-650.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
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
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        w-tes-top-prs        =    "D"
                     go to cnt-tdo-nok-100
           else      go to cnt-tdo-nok-600.
       cnt-tdo-nok-100.
      *              *-------------------------------------------------*
      *              * Se operazione di definizione personalizzazione  *
      *              *-------------------------------------------------*
       cnt-tdo-nok-120.
      *                  *---------------------------------------------*
      *                  * Test su descrizione personalizzazione       *
      *                  *---------------------------------------------*
           if        w-tes-des-prs (1)    not  = spaces
                     go to cnt-tdo-nok-140.
           move      "ME"                 to   v-ope                  .
           move      "Manca la descrizione della personalizzazione !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-nok-flg      .
           go to     cnt-tdo-nok-999.
       cnt-tdo-nok-140.
      *                  *---------------------------------------------*
      *                  * Test su tipo personalizzazione              *
      *                  *---------------------------------------------*
           if        w-tes-tip-prs (1)    not  = spaces
                     go to cnt-tdo-nok-142.
           move      "ME"                 to   v-ope                  .
           move      "Manca il tipo della personalizzazione !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-nok-flg      .
           go to     cnt-tdo-nok-999.
       cnt-tdo-nok-142.
           if        w-tes-tip-prs (1)    =    "A" or
                     w-tes-tip-prs (1)    =    "N" or
                     w-tes-tip-prs (1)    =    "D"
                     go to cnt-tdo-nok-160.
           move      "ME"                 to   v-ope                  .
           move      "Tipo personalizzazione errato !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-nok-flg      .
           go to     cnt-tdo-nok-999.
       cnt-tdo-nok-160.
      *                  *---------------------------------------------*
      *                  * Test su caratteri personalizzazione         *
      *                  *---------------------------------------------*
           if        w-tes-tip-prs (1)    =    "A"
                     go to cnt-tdo-nok-162
           else if   w-tes-tip-prs (1)    =    "N"
                     go to cnt-tdo-nok-164
           else      go to cnt-tdo-nok-180.
       cnt-tdo-nok-162.
           if        w-tes-car-prs (1)    =    zero
                     go to cnt-tdo-nok-166
           else if   w-tes-car-prs (1)    >    80
                     go to cnt-tdo-nok-168
           else      go to cnt-tdo-nok-180.
       cnt-tdo-nok-164.
           if        w-tes-car-prs (1)    =    zero
                     go to cnt-tdo-nok-166
           else if   w-tes-car-prs (1)    >    13
                     go to cnt-tdo-nok-168
           else      go to cnt-tdo-nok-180.
       cnt-tdo-nok-166.
           move      "ME"                 to   v-ope                  .
           move      "Manca il numero di caratteri !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-nok-flg      .
           go to     cnt-tdo-nok-999.
       cnt-tdo-nok-168.
           move      "ME"                 to   v-ope                  .
           move      "Numero di caratteri errato !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-nok-flg      .
           go to     cnt-tdo-nok-999.
       cnt-tdo-nok-180.
      *                  *---------------------------------------------*
      *                  * Test su decimali personalizzazione          *
      *                  *---------------------------------------------*
           if        w-tes-tip-prs (1)    =    "N"
                     go to cnt-tdo-nok-182
           else      go to cnt-tdo-nok-200.
       cnt-tdo-nok-182.
           if        w-tes-dec-prs (1)    not  > 5
                     go to cnt-tdo-nok-200.
           move      "ME"                 to   v-ope                  .
           move      "Numero di decimali errato !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-nok-flg      .
           go to     cnt-tdo-nok-999.
       cnt-tdo-nok-200.
      *                  *---------------------------------------------*
      *                  * Test su segno algebrico personalizzazione   *
      *                  *---------------------------------------------*
           if        w-tes-tip-prs (1)    =    "N"
                     go to cnt-tdo-nok-202
           else      go to cnt-tdo-nok-220.
       cnt-tdo-nok-202.
           if        w-tes-sgn-prs (1)    =    spaces or
                     w-tes-sgn-prs (1)    =    "S"
                     go to cnt-tdo-nok-220.
           move      "ME"                 to   v-ope                  .
           move      "Segno algebrico errato !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-nok-flg      .
           go to     cnt-tdo-nok-999.
       cnt-tdo-nok-220.
      *                  *---------------------------------------------*
      *                  * Test su singola o multipla                  *
      *                  *---------------------------------------------*
           if        w-tes-som-prs (1)    =    "S" or
                     w-tes-som-prs (1)    =    "M"
                     go to cnt-tdo-nok-300.
           if        w-tes-som-prs (1)    =    spaces
                     go to cnt-tdo-nok-222
           else      go to cnt-tdo-nok-224.
       cnt-tdo-nok-222.
           move      "ME"                 to   v-ope                  .
           move      "Manca la definizione Singola/Multipla !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-nok-flg      .
           go to     cnt-tdo-nok-999.
       cnt-tdo-nok-224.
           move      "ME"                 to   v-ope                  .
           move      "Definizione Singola/Multipla errata !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-nok-flg      .
           go to     cnt-tdo-nok-999.
       cnt-tdo-nok-300.
      *                  *---------------------------------------------*
      *                  * Normalizzazioni se controlli superati       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se di tipo alfanumerico                 *
      *                      *-----------------------------------------*
           if        w-tes-tip-prs (1)    =    "A"
                     move  zero           to   w-tes-dec-prs (1)
                     move  spaces         to   w-tes-sgn-prs (1)      .
      *                      *-----------------------------------------*
      *                      * Se di tipo data                         *
      *                      *-----------------------------------------*
           if        w-tes-tip-prs (1)    =    "D"
                     move  zero           to   w-tes-car-prs (1)
                     move  zero           to   w-tes-dec-prs (1)
                     move  spaces         to   w-tes-sgn-prs (1)      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cnt-tdo-nok-999.
       cnt-tdo-nok-600.
      *              *-------------------------------------------------*
      *              * Se operazione di attribuzione valore            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Nessun controllo                            *
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
           move      spaces               to   w-tes-top-prs          .
           move      spaces               to   w-tes-cod-prs          .
           move      zero                 to   w-tes-prg-prs          .
           move      spaces               to   w-tes-des-prs (1)      .
           move      spaces               to   w-tes-tip-prs (1)      .
           move      zero                 to   w-tes-car-prs (1)      .
           move      zero                 to   w-tes-dec-prs (1)      .
           move      spaces               to   w-tes-sgn-prs (1)      .
           move      spaces               to   w-tes-som-prs (1)      .
           move      spaces               to   w-tes-alx-prs (1)      .
           move      spaces               to   w-tes-com-prs (1)      .
           move      spaces               to   w-tes-alf-prs (1)      .
           move      zero                 to   w-tes-num-prs (1)      .
           move      zero                 to   w-tes-dat-prs (1)      .
           move      zero                 to   w-tes-txt-prs (1)      .
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *-----------------------------------------------------------*
       nor-nok-tes-000.
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
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        w-tes-top-prs        =    "D"
                     go to rou-let-reg-200
           else      go to rou-let-reg-600.
       rou-let-reg-200.
      *              *-------------------------------------------------*
      *              * Se operazione di definizione personalizzazione  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura definizione                         *
      *                  *---------------------------------------------*
           move      "dps "               to   auc-tre                .
           move      w-tes-cod-prs        to   auc-kre                .
           read      auc   with no lock
                           invalid key
                           go to   rou-let-reg-250.
           go to     rou-let-reg-300.
       rou-let-reg-250.
      *                  *---------------------------------------------*
      *                  * Se definizione non esistente                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo funzionamento : Inserimento        *
      *                      *-----------------------------------------*
           move      "I"                  to   w-cnt-mfu-tip-fun      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     rou-let-reg-999.
       rou-let-reg-300.
      *                  *---------------------------------------------*
      *                  * Se definizione esistente                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo funzionamento : Modifica           *
      *                      *-----------------------------------------*
           move      "M"                  to   w-cnt-mfu-tip-fun      .
      *                      *-----------------------------------------*
      *                      * Determinazione valori attuali           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Record letto in comodo di ridefini- *
      *                          * zione                               *
      *                          *-------------------------------------*
           move      auc-dat              to   w-dps                  .
      *                          *-------------------------------------*
      *                          * Bufferizzazione valori letti        *
      *                          *-------------------------------------*
           move      w-dps-des-dps        to   w-tes-des-prs (1)      .
           move      w-dps-tip-dps        to   w-tes-tip-prs (1)      .
           move      w-dps-car-dps        to   w-tes-car-prs (1)      .
           move      w-dps-dec-dps        to   w-tes-dec-prs (1)      .
           move      w-dps-sgn-dps        to   w-tes-sgn-prs (1)      .
           move      w-dps-som-dps        to   w-tes-som-prs (1)      .
           move      w-dps-alx-dps        to   w-tes-alx-prs (1)      .
           move      w-dps-com-dps        to   w-tes-com-prs (1)      .
      *                      *-----------------------------------------*
      *                      * Valori precedenti anagrafica            *
      *                      *-----------------------------------------*
           move      w-tes-val-aep (1)    to   w-tes-val-aep (2)      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     rou-let-reg-999.
       rou-let-reg-600.
      *              *-------------------------------------------------*
      *              * Se operazione di attribuzione valore            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura valore                              *
      *                  *---------------------------------------------*
           move      "prs "               to   prs-tre                .
           move      w-tes-cod-prs        to   prs-cod                .
           move      w-tes-prg-prs        to   prs-prg                .
           read      prs   with no lock
                           invalid key
                           go to   rou-let-reg-650.
           go to     rou-let-reg-700.
       rou-let-reg-650.
      *                  *---------------------------------------------*
      *                  * Se valore non esistente                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo funzionamento : Inserimento        *
      *                      *-----------------------------------------*
           move      "I"                  to   w-cnt-mfu-tip-fun      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     rou-let-reg-999.
       rou-let-reg-700.
      *                  *---------------------------------------------*
      *                  * Se valore esistente                         *
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
      *                          * record [prs]                        *
      *                          *-------------------------------------*
           if        w-tes-tip-prs (1)    =    "N"
                     move  prs-num        to   w-tes-num-prs (1)
                     move  spaces         to   w-tes-alf-prs (1)
                     move  zero           to   w-tes-dat-prs (1)
           else if   w-tes-tip-prs (1)    =    "D"
                     move  prs-dat        to   w-tes-dat-prs (1)
                     move  spaces         to   w-tes-alf-prs (1)
                     move  zero           to   w-tes-num-prs (1)
           else      move  prs-alf        to   w-tes-alf-prs (1)
                     move  zero           to   w-tes-num-prs (1)
                     move  zero           to   w-tes-dat-prs (1)      .
      *                      *-----------------------------------------*
      *                      * Valori precedenti anagrafica            *
      *                      *-----------------------------------------*
           move      w-tes-val-aep (1)    to   w-tes-val-aep (2)      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
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
       pre-acc-ins-110.
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-cnt-dup-rec-flg    =    spaces
                     go to pre-acc-ins-200.
       pre-acc-ins-120.
      *                  *---------------------------------------------*
      *                  * Normalizzazione segnale di duplicazione     *
      *                  *---------------------------------------------*
           move      spaces               to   w-cnt-dup-rec-flg      .
       pre-acc-ins-130.
      *                  *---------------------------------------------*
      *                  * Se il tipo operazione non e' pari al tipo   *
      *                  * operazione precedente : non si accetta la   *
      *                  * clonazione                                  *
      *                  *---------------------------------------------*
           if        w-clo-prs-top-prs    not  = w-tes-top-prs
                     go to pre-acc-ins-999.
       pre-acc-ins-140.
      *                  *---------------------------------------------*
      *                  * Deviazione secondo il tipo operazione       *
      *                  *---------------------------------------------*
           if        w-tes-top-prs        =    "D"
                     go to pre-acc-ins-150
           else if   w-tes-top-prs        =    "V"
                     go to pre-acc-ins-160
           else      go to pre-acc-ins-999.
       pre-acc-ins-150.
      *                  *---------------------------------------------*
      *                  * Se tipo operazione : definizione            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Valori di testata (1) salvati in valori *
      *                      * di testata (2)                          *
      *                      *-----------------------------------------*
           move      w-clo-prs-wte-001    to   w-tes-val-aep (2)      .
      *                      *-----------------------------------------*
      *                      * Clonazione dei seguenti valori :        *
      *                      *                                         *
      *                      *  - Descrizione                          *
      *                      *  - Tipo                                 *
      *                      *  - Caratteri                            *
      *                      *  - Decimali                             *
      *                      *  - Segno                                *
      *                      *  - Singola o multipla                   *
      *                      *  - Area libera per espansioni           *
      *                      *  - Commento                             *
      *                      *                                         *
      *                      *-----------------------------------------*
           move      w-tes-des-prs (2)    to   w-tes-des-prs (1)      .
           move      w-tes-tip-prs (2)    to   w-tes-tip-prs (1)      .
           move      w-tes-car-prs (2)    to   w-tes-car-prs (1)      .
           move      w-tes-dec-prs (2)    to   w-tes-dec-prs (1)      .
           move      w-tes-sgn-prs (2)    to   w-tes-sgn-prs (1)      .
           move      w-tes-som-prs (2)    to   w-tes-som-prs (1)      .
           move      w-tes-alx-prs (2)    to   w-tes-alx-prs (1)      .
           move      w-tes-com-prs (2)    to   w-tes-com-prs (1)      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     pre-acc-ins-180.
       pre-acc-ins-160.
      *                  *---------------------------------------------*
      *                  * Se tipo operazione : attribuzione valore    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Valori di testata (1) salvati in valori *
      *                      * di testata (2)                          *
      *                      *-----------------------------------------*
           move      w-clo-prs-wte-001    to   w-tes-val-aep (2)      .
      *                      *-----------------------------------------*
      *                      * Se il record da clonare differisce dal  *
      *                      * record attuale per uno dei seguenti va- *
      *                      * lori :                                  *
      *                      *                                         *
      *                      *  - Tipo                                 *
      *                      *  - Caratteri                            *
      *                      *  - Decimali                             *
      *                      *  - Segno                                *
      *                      *  - Singola o multipla                   *
      *                      *                                         *
      *                      * non si accetta la clonazione            *
      *                      *-----------------------------------------*
           if        w-tes-tip-prs (1)    not  = w-tes-tip-prs (2) or
                     w-tes-car-prs (1)    not  = w-tes-car-prs (2) or
                     w-tes-dec-prs (1)    not  = w-tes-dec-prs (2) or
                     w-tes-sgn-prs (1)    not  = w-tes-sgn-prs (2) or
                     w-tes-som-prs (1)    not  = w-tes-som-prs (2)
                     go to pre-acc-ins-999.
      *                      *-----------------------------------------*
      *                      * Clonazione dei seguenti valori :        *
      *                      *                                         *
      *                      *  - Valore alfa                          *
      *                      *  - Valore numerico                      *
      *                      *  - Valore data                          *
      *                      *  - Valore text                          *
      *                      *                                         *
      *                      *-----------------------------------------*
           move      w-tes-alf-prs (2)    to   w-tes-alf-prs (1)      .
           move      w-tes-num-prs (2)    to   w-tes-num-prs (1)      .
           move      w-tes-dat-prs (2)    to   w-tes-dat-prs (1)      .
           move      w-tes-txt-prs (2)    to   w-tes-txt-prs (1)      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     pre-acc-ins-180.
       pre-acc-ins-180.
      *                  *---------------------------------------------*
      *                  * Normalizzazione flags di controllo          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di impostazione pagine di testata  *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-tes      .
       pre-acc-ins-190.
      *                  *---------------------------------------------*
      *                  * Visualizzazione pagina di testata           *
      *                  *---------------------------------------------*
           perform   vis-tes-reg-000      thru vis-tes-reg-999        .
       pre-acc-ins-200.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
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
      *              *-------------------------------------------------*
      *              * Normalizzazione segnale di duplicazione         *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-dup-rec-flg      .
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
      *              * Aggiornamento dei valori di defaults generali : *
      *              * - Tipo operazione                               *
      *              * - Nome personalizzazione                        *
      *              * - Numero progressivo                            *
      *              *-------------------------------------------------*
           move      w-tes-top-prs        to   w-def-tip-ope          .
           move      w-tes-cod-prs        to   w-def-nom-prs          .
           if        w-def-tip-ope        =    "V"   and
                     w-tes-som-prs (1)    =    "M"
                     move  w-tes-prg-prs  to   w-def-num-prg
           else      move  zero           to   w-def-num-prg          .
       pos-cnf-ins-200.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo operazione        *
      *              *-------------------------------------------------*
           if        w-tes-top-prs        =    "V"
                     go to pos-cnf-ins-600.
       pos-cnf-ins-400.
      *              *-------------------------------------------------*
      *              * Se definizione                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Composizione record 'dps' di [auc]          *
      *                  *---------------------------------------------*
           perform   cmp-dps-auc-000      thru cmp-dps-auc-999        .
      *                  *---------------------------------------------*
      *                  * Scrittura record 'dps' di [auc]             *
      *                  *---------------------------------------------*
           move      "dps "               to   auc-tre                .
           move      w-dps-cod-dps        to   auc-kre                .
           move      w-dps                to   auc-dat                .
           write     auc-rec invalid key
                             go to   pos-cnf-ins-999.
           go to     pos-cnf-ins-999.
       pos-cnf-ins-600.
      *              *-------------------------------------------------*
      *              * Se attribuzione del valore                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Composizione record 'prs' di [prs]          *
      *                  *---------------------------------------------*
           perform   cmp-prs-prs-000      thru cmp-prs-prs-999        .
      *                  *---------------------------------------------*
      *                  * Scrittura record 'prs' di [prs]             *
      *                  *---------------------------------------------*
           write     prs-rec invalid key
                             go to   pos-cnf-ins-999.
           go to     pos-cnf-ins-999.
       pos-cnf-ins-999.
           exit.

      *    *===========================================================*
      *    * Routine post-conferma di modifica                         *
      *    *-----------------------------------------------------------*
       pos-cnf-mod-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo operazione        *
      *              *-------------------------------------------------*
           if        w-tes-top-prs        =    "V"
                     go to pos-cnf-mod-600.
       pos-cnf-mod-400.
      *              *-------------------------------------------------*
      *              * Se definizione                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Composizione record 'dps' di [auc]          *
      *                  *---------------------------------------------*
           perform   cmp-dps-auc-000      thru cmp-dps-auc-999        .
      *                  *---------------------------------------------*
      *                  * Riscrittura record 'dps' di [auc]           *
      *                  *---------------------------------------------*
           move      "dps "               to   auc-tre                .
           move      w-dps-cod-dps        to   auc-kre                .
           move      w-dps                to   auc-dat                .
           rewrite   auc-rec invalid key
                             go to   pos-cnf-mod-999.
           go to     pos-cnf-mod-999.
       pos-cnf-mod-600.
      *              *-------------------------------------------------*
      *              * Se attribuzione del valore                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Composizione record 'prs' di [prs]          *
      *                  *---------------------------------------------*
           perform   cmp-prs-prs-000      thru cmp-prs-prs-999        .
      *                  *---------------------------------------------*
      *                  * Riscrittura record 'prs' di [prs]           *
      *                  *---------------------------------------------*
           rewrite   prs-rec invalid key
                             go to   pos-cnf-mod-999.
           go to     pos-cnf-mod-999.
       pos-cnf-mod-999.
           exit.

      *    *===========================================================*
      *    * Routine post-conferma di annullamento                     *
      *    *-----------------------------------------------------------*
       pos-cnf-ann-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo operazione        *
      *              *-------------------------------------------------*
           if        w-tes-top-prs        =    "V"
                     go to pos-cnf-ann-600.
       pos-cnf-ann-400.
      *              *-------------------------------------------------*
      *              * Se definizione                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Composizione record 'dps' di [auc]          *
      *                  *---------------------------------------------*
           perform   cmp-dps-auc-000      thru cmp-dps-auc-999        .
      *                  *---------------------------------------------*
      *                  * Delete record 'dps' di [auc]                *
      *                  *---------------------------------------------*
           move      "dps "               to   auc-tre                .
           move      w-dps-cod-dps        to   auc-kre                .
           move      w-dps                to   auc-dat                .
           delete    auc     invalid key
                             go to   pos-cnf-ann-999.
           go to     pos-cnf-ann-999.
       pos-cnf-ann-600.
      *              *-------------------------------------------------*
      *              * Se attribuzione del valore                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Composizione record 'prs' di [prs]          *
      *                  *---------------------------------------------*
           perform   cmp-prs-prs-000      thru cmp-prs-prs-999        .
      *                  *---------------------------------------------*
      *                  * Delete record 'prs' di [prs]                *
      *                  *---------------------------------------------*
           delete    prs     invalid key
                             go to   pos-cnf-ann-999.
           go to     pos-cnf-ann-999.
       pos-cnf-ann-999.
           exit.

      *    *===========================================================*
      *    * Composizione record 'dps' di [auc]                        *
      *    *-----------------------------------------------------------*
       cmp-dps-auc-000.
           move      spaces               to   w-dps                  .
           move      w-tes-cod-prs        to   w-dps-cod-dps          .
           move      w-tes-des-prs (1)    to   w-dps-des-dps          .
           move      w-tes-tip-prs (1)    to   w-dps-tip-dps          .
           move      w-tes-car-prs (1)    to   w-dps-car-dps          .
           move      w-tes-dec-prs (1)    to   w-dps-dec-dps          .
           move      w-tes-sgn-prs (1)    to   w-dps-sgn-dps          .
           move      w-tes-som-prs (1)    to   w-dps-som-dps          .
           move      w-tes-alx-prs (1)    to   w-dps-alx-dps          .
           move      w-tes-com-prs (1)    to   w-dps-com-dps          .
       cmp-dps-auc-999.
           exit.

      *    *===========================================================*
      *    * Composizione record 'prs' di [prs]                        *
      *    *-----------------------------------------------------------*
       cmp-prs-prs-000.
           move      spaces               to   prs-rec                .
           move      "prs "               to   prs-tre                .
           move      w-tes-cod-prs        to   prs-cod                .
           move      w-tes-prg-prs        to   prs-prg                .
           if        w-tes-tip-prs (1)    =    "A"
                     move  w-tes-alf-prs (1)
                                          to   prs-alf
           else if   w-tes-tip-prs (1)    =    "N"
                     move  w-tes-num-prs (1)
                                          to   prs-num
           else if   w-tes-tip-prs (1)    =    "D"
                     move  w-tes-dat-prs (1)
                                          to   prs-dat                .
       cmp-prs-prs-999.
           exit.

      *    *===========================================================*
      *    * Find su archivio [prs]                                    *
      *    *-----------------------------------------------------------*
       fnd-arc-prs-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-fnd-arc-prs-sel      .
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pxpg5110"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     move  "#"            to   w-fnd-arc-prs-sel
                     go to  fnd-arc-prs-999.
      *              *-------------------------------------------------*
      *              * Preparazione variabile di i.p.c. per tipo re-   *
      *              * cord da considerare                             *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "tip-rec"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      w-fnd-arc-prs-ope    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Preparazione variabile di i.p.c. per nome per-  *
      *              * sonalizzazione da considerare                   *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "nom-prs"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      40                   to   s-car                  .
           move      w-fnd-arc-prs-nom    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       fnd-arc-prs-500.
      *              *-------------------------------------------------*
      *              * Richiamo programma di interrogazione            *
      *              *-------------------------------------------------*
           call      "swd/xpg/prg/obj/pxpg5110"                       .
           cancel    "swd/xpg/prg/obj/pxpg5110"                       .
      *              *-------------------------------------------------*
      *              * Estrazione di eventuale variabile di i.p.c. de- *
      *              * terminata da function-key "SLCT" durante l'in-  *
      *              * terrogazione                                    *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "nep-prs"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     move  "#"            to   w-fnd-arc-prs-sel
                     go to fnd-arc-prs-999.
           move      s-alf                to   w-fnd-arc-prs-val      .
       fnd-arc-prs-999.
           exit.

      *    *===========================================================*
      *    * Separazione della chiave della personalizzazione in due   *
      *    * componenti, quella prima e quella dopo l'eventuale ca-    *
      *    * rattere chiocciola di separazione                         *
      *    *-----------------------------------------------------------*
       sep-key-prs-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione delle due componenti            *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-prs-pre          .
           move      spaces               to   w-key-prs-pos          .
      *              *-------------------------------------------------*
      *              * Inizializzazione del contatore del numero di    *
      *              * caratteri che precedono la chiocciola           *
      *              *-------------------------------------------------*
           move      zero                 to   w-key-prs-pnt          .
      *              *-------------------------------------------------*
      *              * Conteggio del numero di caratteri che precedono *
      *              * la chiocciola                                   *
      *              *-------------------------------------------------*
           inspect   w-key-prs-key    tallying w-key-prs-pnt
                                for characters
                                before initial "@"                    .
      *              *-------------------------------------------------*
      *              * Se il carattere chiocciola non e' presente : si *
      *              * prepara solo la parte precedente e si esce      *
      *              *-------------------------------------------------*
           if        w-key-prs-pnt        =    40
                     move  w-key-prs-key
                                          to   w-key-prs-pre
                     go to sep-key-prs-999.
      *              *-------------------------------------------------*
      *              * Preparazione della componente che precede la    *
      *              * chiocciola                                      *
      *              *-------------------------------------------------*
           move      w-key-prs-key
                    (1 : w-key-prs-pnt)   to   w-key-prs-pre          .
      *              *-------------------------------------------------*
      *              * Preparazione del contatore caratteri in modo da *
      *              * farlo puntare all'inizio della seconda parte    *
      *              *-------------------------------------------------*
           add       2                    to   w-key-prs-pnt          .
      *              *-------------------------------------------------*
      *              * Se non ci sono caratteri da separare come se-   *
      *              * conda parte si esce lasciando a spaces la se-   *
      *              * conda componente                                *
      *              *-------------------------------------------------*
           if        w-key-prs-pnt        >    40
                     go to sep-key-prs-999.
      *              *-------------------------------------------------*
      *              * Preparazione della componente che segue la      *
      *              * chiocciola                                      *
      *              *-------------------------------------------------*
           move      40                   to   w-key-prs-lun          .
           subtract  w-key-prs-pnt        from w-key-prs-lun          .
           add       1                    to   w-key-prs-lun          .
           move      w-key-prs-key
                    (w-key-prs-pnt :
                     w-key-prs-lun)       to   w-key-prs-pos          .
       sep-key-prs-999.
           exit.

