       Identification Division.
       Program-Id.                                 pxpg5200           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    swd                 *
      *                        Area gestionale:    xpg                 *
      *                                Settore:    per                 *
      *                                   Fase:    xpg520              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 31/03/93    *
      *                       Versione attuale:    NdK del 30/04/10    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione Referenze                          *
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
      *    * File Control [ref]                                        *
      *    *-----------------------------------------------------------*
           select  optional  ref   assign to disk         f-ref-pat
                             organization is indexed
                             access  mode is dynamic
                             record  key  is ref-rck
                             file  status is f-ref-sts                .

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
      *    * File Description [ref]                                    *
      *    *-----------------------------------------------------------*
       fd  ref  label record standard.
       01  ref-rec.
           05  ref-rck.
               10  ref-tre                pic  x(04)                  .
               10  ref-cod                pic  x(40)                  .
               10  ref-prg                pic  9(03)                  .
           05  ref-rcd.
               10  ref-r00.
                   15  filler occurs 400  pic  x(01)                  .
               10  ref-r10 redefines
                   ref-r00.
                   15  ref-alf            pic  x(80)                  .
                   15  filler occurs 320  pic  x(01)                  .
               10  ref-r20 redefines
                   ref-r00.
                   15  ref-num            pic s9(13)v9(05) trailing
                                                           separate
                                                           character  .
                   15  filler occurs 381  pic  x(01)                  .
               10  ref-r30 redefines
                   ref-r00.
                   15  ref-dat            pic  9(07)                  .
                   15  filler occurs 393  pic  x(01)                  .
               10  ref-r40 redefines
                   ref-r00.
                   15  ref-txt.
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
      *    * Area ausiliaria per controllo i-o su [ref]                *
      *    *-----------------------------------------------------------*
       01  f-ref.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-ref-nam                  pic  x(04) value "ref "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-ref-pat                  pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-ref-sts                  pic  x(02)                  .

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
                     "xpg520"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pxpg5200"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "           GESTIONE REFERENZE           "       .

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
      *        * Nome referenza di default                             *
      *        *-------------------------------------------------------*
           05  w-def-nom-ref              pic  x(32)                  .
      *        *-------------------------------------------------------*
      *        * Numero progressivo di default                         *
      *        *-------------------------------------------------------*
           05  w-def-num-prg              pic  9(03)                  .

      *    *===========================================================*
      *    * Work per records di [auc] 'drf'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/waucdrf0.cpw"                   .

      *    *===========================================================*
      *    * Work-area per bufferizzazione testata                     *
      *    *-----------------------------------------------------------*
       01  w-tes.
      *        *-------------------------------------------------------*
      *        * Valori chiave                                         *
      *        *-------------------------------------------------------*
           05  w-tes-val-key.
               10  w-tes-top-ref          pic  x(01)                  .
               10  w-tes-cod-ref          pic  x(32)                  .
               10  w-tes-prg-ref          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
               10  w-tes-des-ref          pic  x(80)                  .
               10  w-tes-tip-ref          pic  x(01)                  .
               10  w-tes-car-ref          pic  9(02)                  .
               10  w-tes-dec-ref          pic  9(01)                  .
               10  w-tes-sgn-ref          pic  x(01)                  .
               10  w-tes-som-ref          pic  x(01)                  .
               10  w-tes-alx-ref          pic  x(80)                  .
               10  w-tes-com-ref.
                   15  w-tes-rco-ref
                               occurs 16  pic  x(80)                  .
               10  w-tes-alf-ref          pic  x(80)                  .
               10  w-tes-num-ref          pic s9(13)v9(05) trailing
                                                           separate
                                                           character  .
               10  w-tes-dat-ref          pic  9(07)                  .
               10  w-tes-txt-ref.
                   15  filler  occurs 400 pic  x(01)                  .

      *    *===========================================================*
      *    * Work per clonazioni                                       *
      *    *-----------------------------------------------------------*
       01  w-clo.
      *        *-------------------------------------------------------*
      *        * Work per clonazione referenza                         *
      *        *-------------------------------------------------------*
           05  w-clo-ref.
      *            *---------------------------------------------------*
      *            * Tipo operazione per salvataggio                   *
      *            *---------------------------------------------------*
               10  w-clo-ref-top-ref      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Record valori attuali di testata                  *
      *            *---------------------------------------------------*
               10  w-clo-ref-wte-001.
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
      *        * Nome referenza                                        *
      *        *-------------------------------------------------------*
           05  w-sav-nom-ref              pic  x(32)                  .
      *        *-------------------------------------------------------*
      *        * Tipo referenza                                        *
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
                            "Definizione della referenza        "     .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo referenza                             *
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
      *        * Work per Find su archivio referenze                   *
      *        *-------------------------------------------------------*
           05  w-fnd-arc-ref.
               10  w-fnd-arc-ref-sel      pic  x(01)                  .
               10  w-fnd-arc-ref-ope      pic  x(01)                  .
               10  w-fnd-arc-ref-val.
                   15  w-fnd-arc-ref-nom  pic  x(32)                  .
                   15  w-fnd-arc-ref-prg  pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area per chiave referenza                            *
      *    *-----------------------------------------------------------*
       01  w-key-ref.
      *        *-------------------------------------------------------*
      *        * Chiave completa                                       *
      *        *-------------------------------------------------------*
           05  w-key-ref-key              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Porzione precedente l'eventuale chiocciola            *
      *        *-------------------------------------------------------*
           05  w-key-ref-pre              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Porzione seguente l'eventuale chiocciola              *
      *        *-------------------------------------------------------*
           05  w-key-ref-pos              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Puntatori di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-key-ref-pnt              pic  9(02)                  .
           05  w-key-ref-lun              pic  9(02)                  .

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
           move      w-tes-top-ref        to   w-clo-ref-top-ref      .
      *                  *---------------------------------------------*
      *                  * Salvataggio valori attuali di testata       *
      *                  *---------------------------------------------*
           move      w-tes-val-aep (1)    to   w-clo-ref-wte-001      .
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
      *                  * Nome referenza                              *
      *                  *---------------------------------------------*
           move      spaces               to   w-def-nom-ref          .
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
      *              * Open [ref]                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione pathname per [ref]             *
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
           move      "ref"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "C9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   f-ref-pat              .
      *                  *---------------------------------------------*
      *                  * Funzione open                               *
      *                  *---------------------------------------------*
           open      i-o    ref                                       .
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
      *              * Close [ref]                                     *
      *              *-------------------------------------------------*
           close     ref                                              .
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
      *                  * Nome referenza                              *
      *                  *---------------------------------------------*
           perform   acc-nom-ref-000      thru acc-nom-ref-999        .
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
      *              * Nome referenza                                  *
      *              *-------------------------------------------------*
           perform   vis-nom-ref-000      thru vis-nom-ref-999        .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        w-tes-top-ref        =    "D"
                     go to vis-key-reg-200
           else if   w-tes-top-ref        =    "V"
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
      *                  * Descrizione referenza                       *
      *                  *---------------------------------------------*
           perform   vis-des-ref-000      thru vis-des-ref-999        .
      *                  *---------------------------------------------*
      *                  * Tipo referenza                              *
      *                  *---------------------------------------------*
           perform   vis-def-tip-000      thru vis-def-tip-999        .
      *                  *---------------------------------------------*
      *                  * Caratteri referenza                         *
      *                  *---------------------------------------------*
           perform   vis-def-car-000      thru vis-def-car-999        .
      *                  *---------------------------------------------*
      *                  * Decimali referenza                          *
      *                  *---------------------------------------------*
           perform   vis-def-dec-000      thru vis-def-dec-999        .
      *                  *---------------------------------------------*
      *                  * Segno algebrico referenza                   *
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
      *              * Nome referenza                                  *
      *              *-------------------------------------------------*
           perform   pmt-nom-ref-000      thru pmt-nom-ref-999        .
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
      *              * Descrizione referenza                           *
      *              *-------------------------------------------------*
           perform   pmt-des-ref-000      thru pmt-des-ref-999        .
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
      *              * Literal per parametri di referenza              *
      *              *-------------------------------------------------*
           perform   pmt-lit-prm-000      thru pmt-lit-prm-999        .
      *              *-------------------------------------------------*
      *              * Tipo referenza                                  *
      *              *-------------------------------------------------*
           perform   pmt-def-tip-000      thru pmt-def-tip-999        .
      *              *-------------------------------------------------*
      *              * Caratteri referenza                             *
      *              *-------------------------------------------------*
           perform   pmt-def-car-000      thru pmt-def-car-999        .
      *              *-------------------------------------------------*
      *              * Decimali referenza                              *
      *              *-------------------------------------------------*
           perform   pmt-def-dec-000      thru pmt-def-dec-999        .
      *              *-------------------------------------------------*
      *              * Segno algebrico referenza                       *
      *              *-------------------------------------------------*
           perform   pmt-def-sgn-000      thru pmt-def-sgn-999        .
      *              *-------------------------------------------------*
      *              * Singola o multipla                              *
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
      *              * Valore referenza                                *
      *              *-------------------------------------------------*
           perform   pmt-val-ref-000      thru pmt-val-ref-999        .
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
      *    * Visualizzazione prompts per Nome referenza                *
      *    *-----------------------------------------------------------*
       pmt-nom-ref-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Nome referenza                 :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-nom-ref-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Descrizione referenza         *
      *    *-----------------------------------------------------------*
       pmt-des-ref-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      26                   to   v-pos                  .
           move      "    Descrizione referenza     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-des-ref-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Literal per parametri di re-  *
      *    *                                                   ferenza *
      *    *-----------------------------------------------------------*
       pmt-lit-prm-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      26                   to   v-pos                  .
           move      "     Parametri referenza      "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-lit-prm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Tipo referenza                *
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
      *    * Visualizzazione prompts per Caratteri referenza           *
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
      *    * Visualizzazione prompts per Decimali referenza            *
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
      *    * Visualizzazione prompts per Segno algebrico referenza     *
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
      *    * Visualizzazione prompts per Valore referenza              *
      *    *-----------------------------------------------------------*
       pmt-val-ref-000.
      *              *-------------------------------------------------*
      *              * Prompt                                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      29                   to   v-pos                  .
           move      "    Valore referenza    "
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
       pmt-val-ref-999.
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
           if        w-tes-top-ref        not  = spaces
                     go to acc-tip-ope-050.
           move      w-def-tip-ope        to   w-tes-top-ref          .
       acc-tip-ope-050.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-top-ref        to   w-sav-tip-ope          .
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
           if        w-tes-top-ref        =    "V"
                     move  01             to   v-num
           else if   w-tes-top-ref        =    "D"
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
                     move  "V"            to   w-tes-top-ref
           else if   v-num                =    02
                     move  "D"            to   w-tes-top-ref
           else      move  spaces         to   w-tes-top-ref          .
       acc-tip-ope-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        w-tes-top-ref        not  = "D" and
                     w-tes-top-ref        not  = "V"
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
           if        w-tes-top-ref        =    w-sav-tip-ope
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
      *                          * Se il nome della referenza e' a     *
      *                          * Spaces no normalizzazioni           *
      *                          *-------------------------------------*
           if        w-tes-cod-ref        =    spaces
                     go to acc-tip-ope-650.
      *                          *-------------------------------------*
      *                          * Se il valore attuale indica che si  *
      *                          * tratta di un valore : no normaliz-  *
      *                          * zazioni                             *
      *                          *-------------------------------------*
           if        w-tes-top-ref        =    "V"
                     go to acc-tip-ope-650.
      *                          *-------------------------------------*
      *                          * Normalizzazioni                     *
      *                          *-------------------------------------*
       acc-tip-ope-630.
      *                              *---------------------------------*
      *                              * Numero progressivo              *
      *                              *---------------------------------*
           if        w-tes-prg-ref        =    zero
                     go to acc-tip-ope-650.
           move      zero                 to   w-tes-prg-ref          .
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
           if        w-tes-top-ref        =    "V"
                     move  01             to   v-num
           else if   w-tes-top-ref        =    "D"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-ope-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Nome referenza                *
      *    *-----------------------------------------------------------*
       acc-nom-ref-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuale preparazione default              *
      *                  *---------------------------------------------*
           if        w-tes-cod-ref        not  = spaces
                     go to acc-nom-ref-050.
           move      w-def-nom-ref        to   w-tes-cod-ref          .
       acc-nom-ref-050.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-cod-ref        to   w-sav-nom-ref          .
       acc-nom-ref-100.
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
           move      w-tes-cod-ref        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-nom-ref-999.
       acc-nom-ref-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-cod-ref          .
       acc-nom-ref-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-nom-ref-400.
      *                  *---------------------------------------------*
      *                  * Find su archivio [ref]                      *
      *                  *---------------------------------------------*
           move      w-tes-top-ref        to   w-fnd-arc-ref-ope      .
           move      w-tes-cod-ref        to   w-fnd-arc-ref-nom      .
           perform   fnd-arc-ref-000      thru fnd-arc-ref-999        .
           if        w-fnd-arc-ref-sel    not  = spaces
                     go to acc-nom-ref-100.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo operazione  *
      *                  *---------------------------------------------*
           if        w-tes-top-ref        =    "D"
                     go to acc-nom-ref-325
           else      go to acc-nom-ref-350.
       acc-nom-ref-325.
      *                  *---------------------------------------------*
      *                  * Se operazione di definizione referenza      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Memorizza codice selezionato            *
      *                      *-----------------------------------------*
           move      w-fnd-arc-ref-nom    to   w-tes-cod-ref          .
      *                      *-----------------------------------------*
      *                      * Visualizza codice selezionato           *
      *                      *-----------------------------------------*
           perform   vis-nom-ref-000      thru vis-nom-ref-999        .
      *                      *-----------------------------------------*
      *                      * Forzatura del tasto Return              *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Riciclo a reimpostazione                *
      *                      *-----------------------------------------*
           go to     acc-nom-ref-100.
       acc-nom-ref-350.
      *                  *---------------------------------------------*
      *                  * Se operazione di attribuzione valore        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Memorizza codice selezionato            *
      *                      *-----------------------------------------*
           move      w-fnd-arc-ref-nom    to   w-tes-cod-ref          .
      *                      *-----------------------------------------*
      *                      * Visualizza codice selezionato           *
      *                      *-----------------------------------------*
           perform   vis-nom-ref-000      thru vis-nom-ref-999        .
      *                      *-----------------------------------------*
      *                      * Memorizza nr progressivo selezionato    *
      *                      *-----------------------------------------*
           move      w-fnd-arc-ref-prg    to   w-tes-prg-ref          .
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
           go to     acc-nom-ref-100.
       acc-nom-ref-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           move      w-tes-cod-ref        to   w-all-str-alf          .
           move      05                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-nom-ref-100.
           if        w-tes-cod-ref        not  = spaces
                     go to acc-nom-ref-600.
           if        v-key                not  = "UP  "
                     go to acc-nom-ref-100.
       acc-nom-ref-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo operazione  *
      *                  *---------------------------------------------*
           if        w-tes-top-ref        =    "D"
                     go to acc-nom-ref-650
           else      go to acc-nom-ref-700.
       acc-nom-ref-650.
      *                  *---------------------------------------------*
      *                  * Se operazione di definizione                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazioni                         *
      *                      *-----------------------------------------*
       acc-nom-ref-670.
      *                              *---------------------------------*
      *                              * Numero progressivo              *
      *                              *---------------------------------*
           if        w-tes-prg-ref        =    zero
                     go to acc-nom-ref-690.
           move      zero                 to   w-tes-prg-ref          .
           perform   vis-num-prg-000      thru vis-num-prg-999        .
       acc-nom-ref-690.
      *                  *---------------------------------------------*
      *                  * Prosecuzione dopo normalizzazioni           *
      *                  *---------------------------------------------*
           go to     acc-nom-ref-800.
       acc-nom-ref-700.
      *                  *---------------------------------------------*
      *                  * Se operazione di attribuzione valore        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura della definizione               *
      *                      *-----------------------------------------*
           if        w-tes-cod-ref        =    spaces
                     go to acc-nom-ref-740.
           move      "drf "               to   auc-tre                .
           move      w-tes-cod-ref        to   auc-kre                .
           read      auc   with no lock
                           invalid key
                           go to   acc-nom-ref-760.
       acc-nom-ref-720.
      *                      *-----------------------------------------*
      *                      * Se definizione esistente                *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Record letto in comodo di ridefini- *
      *                          * zione                               *
      *                          *-------------------------------------*
           move      auc-dat              to   w-drf                  .
      *                          *-------------------------------------*
      *                          * Bufferizzazione valori letti        *
      *                          *-------------------------------------*
           move      w-drf-des-drf        to   w-tes-des-ref (1)      .
           move      w-drf-tip-drf        to   w-tes-tip-ref (1)      .
           move      w-drf-car-drf        to   w-tes-car-ref (1)      .
           move      w-drf-dec-drf        to   w-tes-dec-ref (1)      .
           move      w-drf-sgn-drf        to   w-tes-sgn-ref (1)      .
           move      w-drf-som-drf        to   w-tes-som-ref (1)      .
           move      w-drf-alx-drf        to   w-tes-alx-ref (1)      .
           move      w-drf-com-drf        to   w-tes-com-ref (1)      .
      *                          *-------------------------------------*
      *                          * Visualizzazione valori letti        *
      *                          *-------------------------------------*
           perform   vis-des-ref-000      thru vis-des-ref-999        .
           perform   vis-def-tip-000      thru vis-def-tip-999        .
           perform   vis-def-car-000      thru vis-def-car-999        .
           perform   vis-def-dec-000      thru vis-def-dec-999        .
           perform   vis-def-sgn-000      thru vis-def-sgn-999        .
           perform   vis-def-som-000      thru vis-def-som-999        .
      *                          *-------------------------------------*
      *                          * Prosecuzione                        *
      *                          *-------------------------------------*
           go to     acc-nom-ref-800.
       acc-nom-ref-740.
      *                      *-----------------------------------------*
      *                      * Se nome referenza a spaces              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Normalizzazione valori              *
      *                          *-------------------------------------*
           move      spaces               to   w-tes-des-ref (1)      .
           move      spaces               to   w-tes-tip-ref (1)      .
           move      zero                 to   w-tes-car-ref (1)      .
           move      zero                 to   w-tes-dec-ref (1)      .
           move      spaces               to   w-tes-sgn-ref (1)      .
           move      spaces               to   w-tes-som-ref (1)      .
           move      spaces               to   w-tes-alx-ref (1)      .
           move      spaces               to   w-tes-com-ref (1)      .
      *                          *-------------------------------------*
      *                          * Visualizzazione valori normalizzati *
      *                          *-------------------------------------*
           perform   vis-des-ref-000      thru vis-des-ref-999        .
           perform   vis-def-tip-000      thru vis-def-tip-999        .
           perform   vis-def-car-000      thru vis-def-car-999        .
           perform   vis-def-dec-000      thru vis-def-dec-999        .
           perform   vis-def-sgn-000      thru vis-def-sgn-999        .
           perform   vis-def-som-000      thru vis-def-som-999        .
      *                          *-------------------------------------*
      *                          * Prosecuzione                        *
      *                          *-------------------------------------*
           go to     acc-nom-ref-800.
       acc-nom-ref-760.
      *                      *-----------------------------------------*
      *                      * Se definizione non esistente            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Normalizzazione valori              *
      *                          *-------------------------------------*
           move      all   "."            to   w-tes-des-ref (1)      .
       acc-nom-ref-780.
           move      spaces               to   w-tes-tip-ref (1)      .
           move      zero                 to   w-tes-car-ref (1)      .
           move      zero                 to   w-tes-dec-ref (1)      .
           move      spaces               to   w-tes-sgn-ref (1)      .
           move      spaces               to   w-tes-som-ref (1)      .
           move      spaces               to   w-tes-alx-ref (1)      .
           move      spaces               to   w-tes-com-ref (1)      .
      *                          *-------------------------------------*
      *                          * Visualizzazione valori normalizzati *
      *                          *-------------------------------------*
           perform   vis-des-ref-000      thru vis-des-ref-999        .
           perform   vis-def-tip-000      thru vis-def-tip-999        .
           perform   vis-def-car-000      thru vis-def-car-999        .
           perform   vis-def-dec-000      thru vis-def-dec-999        .
           perform   vis-def-sgn-000      thru vis-def-sgn-999        .
           perform   vis-def-som-000      thru vis-def-som-999        .
      *                          *-------------------------------------*
      *                          * Reimpostazione                      *
      *                          *-------------------------------------*
           go to     acc-nom-ref-100.
       acc-nom-ref-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-nom-ref-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-nom-ref-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-nom-ref-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-nom-ref-999.
       acc-nom-ref-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Nome referenza             *
      *    *-----------------------------------------------------------*
       vis-nom-ref-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-tes-cod-ref        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-nom-ref-999.
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
           if        w-tes-top-ref        =    "V"
                     go to acc-num-prg-075.
       acc-num-prg-025.
           if        w-tes-prg-ref        =    zero
                     go to acc-num-prg-050.
           move      zero                 to   w-tes-prg-ref          .
           perform   vis-num-prg-000      thru vis-num-prg-999        .
       acc-num-prg-050.
           go to     acc-num-prg-999.
       acc-num-prg-075.
           if        w-tes-som-ref (1)    not  = "M"
                     go to acc-num-prg-025.
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        w-tes-prg-ref        not  = zero
                     go to acc-num-prg-100.
           move      w-def-num-prg        to   w-tes-prg-ref          .
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
           move      w-tes-prg-ref        to   v-num                  .
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
           move      v-num                to   w-tes-prg-ref          .
       acc-num-prg-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        w-tes-prg-ref        not  = zero
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
           move      w-tes-prg-ref        to   v-num                  .
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
      *                  * Descrizione referenza                       *
      *                  *---------------------------------------------*
           perform   acc-des-ref-000      thru acc-des-ref-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-200.
      *                  *---------------------------------------------*
      *                  * Tipo referenza                              *
      *                  *---------------------------------------------*
           perform   acc-def-tip-000      thru acc-def-tip-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-100.
       acc-tes-reg-300.
      *                  *---------------------------------------------*
      *                  * Caratteri referenza                         *
      *                  *---------------------------------------------*
           perform   acc-def-car-000      thru acc-def-car-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-200.
       acc-tes-reg-400.
      *                  *---------------------------------------------*
      *                  * Decimali referenza                          *
      *                  *---------------------------------------------*
           perform   acc-def-dec-000      thru acc-def-dec-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-300.
       acc-tes-reg-500.
      *                  *---------------------------------------------*
      *                  * Segno algebrico referenza                   *
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
      *                  * Valore della referenza                      *
      *                  *---------------------------------------------*
           perform   acc-val-ref-000      thru acc-val-ref-999        .
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
           if        w-tes-top-ref        =    "D"
                     go to vis-tes-reg-200
           else if   w-tes-top-ref        =    "V"
                     go to vis-tes-reg-400
           else      go to vis-tes-reg-999.
       vis-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Se operazione di definizione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Descrizione referenza                       *
      *                  *---------------------------------------------*
           perform   vis-des-ref-000      thru vis-des-ref-999        .
      *                  *---------------------------------------------*
      *                  * Tipo referenza                              *
      *                  *---------------------------------------------*
           perform   vis-def-tip-000      thru vis-def-tip-999        .
      *                  *---------------------------------------------*
      *                  * Caratteri referenza                         *
      *                  *---------------------------------------------*
           perform   vis-def-car-000      thru vis-def-car-999        .
      *                  *---------------------------------------------*
      *                  * Decimali referenza                          *
      *                  *---------------------------------------------*
           perform   vis-def-dec-000      thru vis-def-dec-999        .
      *                  *---------------------------------------------*
      *                  * Segno algebrico referenza                   *
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
      *                  * Valore della referenza                      *
      *                  *---------------------------------------------*
           perform   vis-val-ref-000      thru vis-val-ref-999        .
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
      *    * Accettazione campo testata : Descrizione della referenza  *
      *    *-----------------------------------------------------------*
       acc-des-ref-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore da accettare                 *
      *                  *---------------------------------------------*
           if        w-tes-top-ref        not  = "D"
                     go to acc-des-ref-999.
       acc-des-ref-100.
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
           move      w-tes-des-ref (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-des-ref-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-des-ref-999.
       acc-des-ref-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-des-ref (1)      .
       acc-des-ref-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        w-tes-des-ref (1)    =    spaces
                     go to acc-des-ref-100.
       acc-des-ref-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-des-ref-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-des-ref-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-des-ref-100.
       acc-des-ref-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione referenza     *
      *    *-----------------------------------------------------------*
       vis-des-ref-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-tes-des-ref (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-ref-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Tipo referenza               *
      *    *-----------------------------------------------------------*
       acc-def-tip-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore da accettare                 *
      *                  *---------------------------------------------*
           if        w-tes-top-ref        not  = "D"
                     go to acc-def-tip-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-tip-ref (1)    to   w-sav-def-tip          .
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
           if        w-tes-tip-ref (1)    =    "A"
                     move  01             to   v-num
           else if   w-tes-tip-ref (1)    =    "N"
                     move  02             to   v-num
           else if   w-tes-tip-ref (1)    =    "D"
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
                     move  "A"            to   w-tes-tip-ref (1)
           else if   v-num                =    02
                     move  "N"            to   w-tes-tip-ref (1)
           else if   v-num                =    03
                     move  "D"            to   w-tes-tip-ref (1)
           else      move  zero           to   w-tes-tip-ref (1)      .
       acc-def-tip-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        w-tes-tip-ref (1)    =    "A" or
                     w-tes-tip-ref (1)    =    "N" or
                     w-tes-tip-ref (1)    =    "D"
                     go to acc-def-tip-600.
           if        w-tes-tip-ref (1)    not  = spaces
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
           if        w-tes-tip-ref (1)    =    w-sav-def-tip
                     go to acc-def-tip-650.
           if        w-sav-def-tip        =    spaces
                     go to acc-def-tip-650.
       acc-def-tip-620.
      *                      *-----------------------------------------*
      *                      * Caratteri della referenza               *
      *                      *-----------------------------------------*
           if        w-tes-car-ref (1)    =    zero
                     go to acc-def-tip-630.
           move      zero                 to   w-tes-car-ref (1)      .
           perform   vis-def-car-000      thru vis-def-car-999        .
       acc-def-tip-630.
      *                      *-----------------------------------------*
      *                      * Decimali della referenza                *
      *                      *-----------------------------------------*
           if        w-tes-dec-ref (1)    =    zero
                     go to acc-def-tip-640.
           move      zero                 to   w-tes-dec-ref (1)      .
           perform   vis-def-dec-000      thru vis-def-dec-999        .
       acc-def-tip-640.
      *                      *-----------------------------------------*
      *                      * Segno algebrico della referenza         *
      *                      *-----------------------------------------*
           if        w-tes-sgn-ref (1)    =    spaces
                     go to acc-def-tip-650.
           move      spaces               to   w-tes-sgn-ref (1)      .
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
      *    * Visualizzazione campo testata : Tipo referenza            *
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
           if        w-tes-tip-ref (1)    =    "A"
                     move  01             to   v-num
           else if   w-tes-tip-ref (1)    =    "N"
                     move  02             to   v-num
           else if   w-tes-tip-ref (1)    =    "D"
                     move  03             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-def-tip-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Caratteri referenza          *
      *    *-----------------------------------------------------------*
       acc-def-car-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore da accettare                 *
      *                  *---------------------------------------------*
           if        w-tes-top-ref        not  = "D"
                     go to acc-def-car-999.
           if        w-tes-tip-ref (1)    not  = "A" and
                     w-tes-tip-ref (1)    not  = "N"
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
           move      w-tes-car-ref (1)    to   v-num                  .
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
           move      v-num                to   w-tes-car-ref (1)      .
       acc-def-car-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        w-tes-tip-ref (1)    =    "A" and
                     w-tes-car-ref (1)    >    80
                     go to acc-def-car-100.
           if        w-tes-tip-ref (1)    =    "N" and
                     w-tes-car-ref (1)    >    13
                     go to acc-def-car-100.
           if        w-tes-car-ref (1)    >    zero
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
      *    * Visualizzazione campo testata : Caratteri referenza       *
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
           move      w-tes-car-ref (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-def-car-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Decimali referenza           *
      *    *-----------------------------------------------------------*
       acc-def-dec-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore da accettare                 *
      *                  *---------------------------------------------*
           if        w-tes-top-ref        not  = "D"
                     go to acc-def-dec-999.
           if        w-tes-tip-ref (1)    not  = "N"
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
           move      w-tes-dec-ref (1)    to   v-num                  .
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
           move      v-num                to   w-tes-dec-ref (1)      .
       acc-def-dec-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        w-tes-dec-ref (1)    >    5
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
      *    * Visualizzazione campo testata : Decimali referenza        *
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
           move      w-tes-dec-ref (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-def-dec-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Segno algebrico per la re-   *
      *    *                              ferenza                      *
      *    *-----------------------------------------------------------*
       acc-def-sgn-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore da accettare                 *
      *                  *---------------------------------------------*
           if        w-tes-top-ref        not  = "D"
                     go to acc-def-sgn-999.
           if        w-tes-tip-ref (1)    not  = "N"
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
           move      w-tes-sgn-ref (1)    to   v-alf                  .
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
           move      v-alf                to   w-tes-sgn-ref (1)      .
       acc-def-sgn-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        w-tes-sgn-ref (1)    not  = spaces and
                     w-tes-sgn-ref (1)    not  = "S"
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
      *    *                                 referenza                 *
      *    *-----------------------------------------------------------*
       vis-def-sgn-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      w-tes-sgn-ref (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-def-sgn-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Singola o multipla           *
      *    *-----------------------------------------------------------*
       acc-def-som-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-def-som-025.
      *                  *---------------------------------------------*
      *                  * Test se valore da accettare                 *
      *                  *---------------------------------------------*
           if        w-tes-top-ref        not  = "D"
                     go to acc-def-som-999.
       acc-def-som-050.
      *                  *---------------------------------------------*
      *                  * Eventuale preparazione del default          *
      *                  *---------------------------------------------*
           if        w-tes-som-ref (1)    not  = "S" and
                     w-tes-som-ref (1)    not  = "M"
                     move  "S"            to   w-tes-som-ref (1)      .
       acc-def-som-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-def-som-lun    to   v-car                  .
           move      w-exp-def-som-num    to   v-ldt                  .
           move      "SM#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      w-exp-def-som-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      15                   to   v-lin                  .
           move      70                   to   v-pos                  .
           if        w-tes-som-ref (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-som-ref (1)    =    "M"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-def-som-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-def-som-999.
       acc-def-som-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   w-tes-som-ref (1)
           else if   v-num                =    02
                     move  "M"            to   w-tes-som-ref (1)
           else      move  spaces         to   w-tes-som-ref (1)      .
       acc-def-som-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        w-tes-som-ref (1)    =    "S" or
                     w-tes-som-ref (1)    =    "M"
                     go to acc-def-som-600.
           if        w-tes-som-ref (1)    not  = spaces
                     go to acc-def-som-100.
           if        v-key                not  = "UP  "
                     go to acc-def-som-100.
       acc-def-som-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-def-som-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-def-som-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-def-som-100.
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
           move      spaces               to   v-edm                  .
           move      w-exp-def-som-tbl    to   v-txt                  .
           move      15                   to   v-lin                  .
           move      70                   to   v-pos                  .
           if        w-tes-som-ref (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-som-ref (1)    =    "M"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-def-som-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Valore referenza             *
      *    *-----------------------------------------------------------*
       acc-val-ref-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore da accettare                 *
      *                  *---------------------------------------------*
           if        w-tes-top-ref        not  = "V"
                     go to acc-val-ref-999.
       acc-val-ref-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo referenza   *
      *                  *---------------------------------------------*
           if        w-tes-tip-ref (1)    =    "A"
                     go to acc-val-ref-120
           else if   w-tes-tip-ref (1)    =    "N"
                     go to acc-val-ref-140
           else if   w-tes-tip-ref (1)    =    "D"
                     go to acc-val-ref-160.
       acc-val-ref-120.
      *                  *---------------------------------------------*
      *                  * Se di tipo alfanumerico                     *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      w-tes-car-ref (1)    to   v-car                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-tes-alf-ref (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           go to     acc-val-ref-180.
       acc-val-ref-140.
      *                  *---------------------------------------------*
      *                  * Se di tipo numerico                         *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      w-tes-car-ref (1)    to   v-car                  .
           move      w-tes-dec-ref (1)    to   v-dec                  .
           move      w-tes-sgn-ref (1)    to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-tes-num-ref (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           go to     acc-val-ref-180.
       acc-val-ref-160.
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
           move      w-tes-dat-ref (1)    to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           go to     acc-val-ref-180.
       acc-val-ref-180.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-val-ref-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-val-ref-999.
       acc-val-ref-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        w-tes-tip-ref (1)    =    "N"
                     move  v-num          to   w-tes-num-ref (1)
           else if   w-tes-tip-ref (1)    =    "D"
                     move  v-dat          to   w-tes-dat-ref (1)
           else      move  v-alf          to   w-tes-alf-ref (1)      .
       acc-val-ref-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-val-ref-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-val-ref-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-val-ref-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-val-ref-100.
       acc-val-ref-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Valore referenza          *
      *    *-----------------------------------------------------------*
       vis-val-ref-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo referenza       *
      *              *-------------------------------------------------*
           if        w-tes-tip-ref (1)    =    "A"
                     go to vis-val-ref-200
           else if   w-tes-tip-ref (1)    =    "N"
                     go to vis-val-ref-400
           else if   w-tes-tip-ref (1)    =    "D"
                     go to vis-val-ref-600
           else      go to vis-val-ref-999.
       vis-val-ref-200.
      *              *-------------------------------------------------*
      *              * Se referenza di tipo alfanumerico               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione del valore alfanumerico     *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      w-tes-car-ref (1)    to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-tes-alf-ref (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-val-ref-999.
       vis-val-ref-400.
      *              *-------------------------------------------------*
      *              * Se referenza di tipo numerico                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione del valore numerico         *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      w-tes-car-ref (1)    to   v-car                  .
           move      w-tes-dec-ref (1)    to   v-dec                  .
           move      w-tes-sgn-ref (1)    to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-tes-num-ref (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-val-ref-999.
       vis-val-ref-600.
      *              *-------------------------------------------------*
      *              * Se referenza di tipo data                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione del valore data             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-tes-dat-ref (1)    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-val-ref-999.
       vis-val-ref-999.
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
           if        w-tes-top-ref        =    "D"
                     go to cnt-tdo-key-200.
           if        w-tes-top-ref        =    "V"
                     go to cnt-tdo-key-500.
           if        w-tes-top-ref        =    spaces
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
      *                  * Test su nome referenza, se manca            *
      *                  *---------------------------------------------*
           if        w-tes-cod-ref        not  = spaces
                     go to cnt-tdo-key-240.
           move      "ME"                 to   v-ope                  .
           move      "Manca il nome della referenza !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-key-flg      .
           go to     cnt-tdo-key-999.
       cnt-tdo-key-240.
      *                  *---------------------------------------------*
      *                  * Test su nome referenza, se errato           *
      *                  *---------------------------------------------*
           move      w-tes-cod-ref        to   w-key-ref-key          .
           perform   sep-key-ref-000      thru sep-key-ref-999        .
           if        w-key-ref-pos        =    spaces
                     go to cnt-tdo-key-260.
           move      "ME"                 to   v-ope                  .
           move      "Nome della referenza errato!"
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
           if        w-tes-prg-ref        =    zero
                     go to cnt-tdo-key-330.
           move      zero                 to   w-tes-prg-ref          .
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
      *                  * Test su nome referenza, se manca            *
      *                  *---------------------------------------------*
           if        w-tes-cod-ref        not  = spaces
                     go to cnt-tdo-key-540.
           move      "ME"                 to   v-ope                  .
           move      "Manca il nome della referenza !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-key-flg      .
           go to     cnt-tdo-key-999.
       cnt-tdo-key-540.
      *                  *---------------------------------------------*
      *                  * Test su nome referenza, se errato           *
      *                  *---------------------------------------------*
           move      w-tes-cod-ref        to   w-key-ref-key          .
           perform   sep-key-ref-000      thru sep-key-ref-999        .
           if        w-key-ref-pre        not  = spaces
                     go to cnt-tdo-key-560.
           move      "ME"                 to   v-ope                  .
           move      "Nome della referenza errato!"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-key-flg      .
           go to     cnt-tdo-key-999.
       cnt-tdo-key-560.
      *                  *---------------------------------------------*
      *                  * Test su Numero progressivo                  *
      *                  *---------------------------------------------*
           if        w-tes-som-ref (1)    not  = "M"
                     go to cnt-tdo-key-580.
           if        w-tes-prg-ref        not  = zero
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
           if        w-tes-som-ref (1)    =    "M"
                     go to cnt-tdo-key-650.
           if        w-tes-prg-ref        =    zero
                     go to cnt-tdo-key-650.
           move      zero                 to   w-tes-prg-ref          .
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
           if        w-tes-top-ref        =    "D"
                     go to cnt-tdo-nok-100
           else      go to cnt-tdo-nok-600.
       cnt-tdo-nok-100.
      *              *-------------------------------------------------*
      *              * Se operazione di definizione referenza          *
      *              *-------------------------------------------------*
       cnt-tdo-nok-120.
      *                  *---------------------------------------------*
      *                  * Test su descrizione referenza               *
      *                  *---------------------------------------------*
           if        w-tes-des-ref (1)    not  = spaces
                     go to cnt-tdo-nok-140.
           move      "ME"                 to   v-ope                  .
           move      "Manca la descrizione della referenza !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-nok-flg      .
           go to     cnt-tdo-nok-999.
       cnt-tdo-nok-140.
      *                  *---------------------------------------------*
      *                  * Test su tipo referenza                      *
      *                  *---------------------------------------------*
           if        w-tes-tip-ref (1)    not  = spaces
                     go to cnt-tdo-nok-142.
           move      "ME"                 to   v-ope                  .
           move      "Manca il tipo della referenza !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-nok-flg      .
           go to     cnt-tdo-nok-999.
       cnt-tdo-nok-142.
           if        w-tes-tip-ref (1)    =    "A" or
                     w-tes-tip-ref (1)    =    "N" or
                     w-tes-tip-ref (1)    =    "D"
                     go to cnt-tdo-nok-160.
           move      "ME"                 to   v-ope                  .
           move      "Tipo referenza errato !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-nok-flg      .
           go to     cnt-tdo-nok-999.
       cnt-tdo-nok-160.
      *                  *---------------------------------------------*
      *                  * Test su caratteri referenza                 *
      *                  *---------------------------------------------*
           if        w-tes-tip-ref (1)    =    "A"
                     go to cnt-tdo-nok-162
           else if   w-tes-tip-ref (1)    =    "N"
                     go to cnt-tdo-nok-164
           else      go to cnt-tdo-nok-180.
       cnt-tdo-nok-162.
           if        w-tes-car-ref (1)    =    zero
                     go to cnt-tdo-nok-166
           else if   w-tes-car-ref (1)    >    80
                     go to cnt-tdo-nok-168
           else      go to cnt-tdo-nok-180.
       cnt-tdo-nok-164.
           if        w-tes-car-ref (1)    =    zero
                     go to cnt-tdo-nok-166
           else if   w-tes-car-ref (1)    >    13
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
      *                  * Test su decimali referenza                  *
      *                  *---------------------------------------------*
           if        w-tes-tip-ref (1)    =    "N"
                     go to cnt-tdo-nok-182
           else      go to cnt-tdo-nok-200.
       cnt-tdo-nok-182.
           if        w-tes-dec-ref (1)    not  > 5
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
      *                  * Test su segno algebrico referenza           *
      *                  *---------------------------------------------*
           if        w-tes-tip-ref (1)    =    "N"
                     go to cnt-tdo-nok-202
           else      go to cnt-tdo-nok-220.
       cnt-tdo-nok-202.
           if        w-tes-sgn-ref (1)    =    spaces or
                     w-tes-sgn-ref (1)    =    "S"
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
           if        w-tes-som-ref (1)    =    "S" or
                     w-tes-som-ref (1)    =    "M"
                     go to cnt-tdo-nok-300.
           if        w-tes-som-ref (1)    =    spaces
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
           if        w-tes-tip-ref (1)    =    "A"
                     move  zero           to   w-tes-dec-ref (1)
                     move  spaces         to   w-tes-sgn-ref (1)      .
      *                      *-----------------------------------------*
      *                      * Se di tipo data                         *
      *                      *-----------------------------------------*
           if        w-tes-tip-ref (1)    =    "D"
                     move  zero           to   w-tes-car-ref (1)
                     move  zero           to   w-tes-dec-ref (1)
                     move  spaces         to   w-tes-sgn-ref (1)      .
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
           move      spaces               to   w-tes-top-ref          .
           move      spaces               to   w-tes-cod-ref          .
           move      zero                 to   w-tes-prg-ref          .
           move      spaces               to   w-tes-des-ref (1)      .
           move      spaces               to   w-tes-tip-ref (1)      .
           move      zero                 to   w-tes-car-ref (1)      .
           move      zero                 to   w-tes-dec-ref (1)      .
           move      spaces               to   w-tes-sgn-ref (1)      .
           move      spaces               to   w-tes-som-ref (1)      .
           move      spaces               to   w-tes-alx-ref (1)      .
           move      spaces               to   w-tes-com-ref (1)      .
           move      spaces               to   w-tes-alf-ref (1)      .
           move      zero                 to   w-tes-num-ref (1)      .
           move      zero                 to   w-tes-dat-ref (1)      .
           move      zero                 to   w-tes-txt-ref (1)      .
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
           if        w-tes-top-ref        =    "D"
                     go to rou-let-reg-200
           else      go to rou-let-reg-600.
       rou-let-reg-200.
      *              *-------------------------------------------------*
      *              * Se operazione di definizione referenza          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura definizione                         *
      *                  *---------------------------------------------*
           move      "drf "               to   auc-tre                .
           move      w-tes-cod-ref        to   auc-kre                .
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
           move      auc-dat              to   w-drf                  .
      *                          *-------------------------------------*
      *                          * Bufferizzazione valori letti        *
      *                          *-------------------------------------*
           move      w-drf-des-drf        to   w-tes-des-ref (1)      .
           move      w-drf-tip-drf        to   w-tes-tip-ref (1)      .
           move      w-drf-car-drf        to   w-tes-car-ref (1)      .
           move      w-drf-dec-drf        to   w-tes-dec-ref (1)      .
           move      w-drf-sgn-drf        to   w-tes-sgn-ref (1)      .
           move      w-drf-som-drf        to   w-tes-som-ref (1)      .
           move      w-drf-alx-drf        to   w-tes-alx-ref (1)      .
           move      w-drf-com-drf        to   w-tes-com-ref (1)      .
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
           move      "ref "               to   ref-tre                .
           move      w-tes-cod-ref        to   ref-cod                .
           move      w-tes-prg-ref        to   ref-prg                .
           read      ref   with no lock
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
      *                          * record [ref]                        *
      *                          *-------------------------------------*
           if        w-tes-tip-ref (1)    =    "N"
                     move  ref-num        to   w-tes-num-ref (1)
                     move  spaces         to   w-tes-alf-ref (1)
                     move  zero           to   w-tes-dat-ref (1)
           else if   w-tes-tip-ref (1)    =    "D"
                     move  ref-dat        to   w-tes-dat-ref (1)
                     move  spaces         to   w-tes-alf-ref (1)
                     move  zero           to   w-tes-num-ref (1)
           else      move  ref-alf        to   w-tes-alf-ref (1)
                     move  zero           to   w-tes-num-ref (1)
                     move  zero           to   w-tes-dat-ref (1)      .
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
           if        w-clo-ref-top-ref    not  = w-tes-top-ref
                     go to pre-acc-ins-999.
       pre-acc-ins-140.
      *                  *---------------------------------------------*
      *                  * Deviazione secondo il tipo operazione       *
      *                  *---------------------------------------------*
           if        w-tes-top-ref        =    "D"
                     go to pre-acc-ins-150
           else if   w-tes-top-ref        =    "V"
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
           move      w-clo-ref-wte-001    to   w-tes-val-aep (2)      .
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
           move      w-tes-des-ref (2)    to   w-tes-des-ref (1)      .
           move      w-tes-tip-ref (2)    to   w-tes-tip-ref (1)      .
           move      w-tes-car-ref (2)    to   w-tes-car-ref (1)      .
           move      w-tes-dec-ref (2)    to   w-tes-dec-ref (1)      .
           move      w-tes-sgn-ref (2)    to   w-tes-sgn-ref (1)      .
           move      w-tes-som-ref (2)    to   w-tes-som-ref (1)      .
           move      w-tes-alx-ref (2)    to   w-tes-alx-ref (1)      .
           move      w-tes-com-ref (2)    to   w-tes-com-ref (1)      .
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
           move      w-clo-ref-wte-001    to   w-tes-val-aep (2)      .
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
           if        w-tes-tip-ref (1)    not  = w-tes-tip-ref (2) or
                     w-tes-car-ref (1)    not  = w-tes-car-ref (2) or
                     w-tes-dec-ref (1)    not  = w-tes-dec-ref (2) or
                     w-tes-sgn-ref (1)    not  = w-tes-sgn-ref (2) or
                     w-tes-som-ref (1)    not  = w-tes-som-ref (2)
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
           move      w-tes-alf-ref (2)    to   w-tes-alf-ref (1)      .
           move      w-tes-num-ref (2)    to   w-tes-num-ref (1)      .
           move      w-tes-dat-ref (2)    to   w-tes-dat-ref (1)      .
           move      w-tes-txt-ref (2)    to   w-tes-txt-ref (1)      .
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
      *              * - Nome referenza                                *
      *              * - Numero progressivo                            *
      *              *-------------------------------------------------*
           move      w-tes-top-ref        to   w-def-tip-ope          .
           move      w-tes-cod-ref        to   w-def-nom-ref          .
           if        w-def-tip-ope        =    "V"   and
                     w-tes-som-ref (1)    =    "M"
                     move  w-tes-prg-ref  to   w-def-num-prg
           else      move  zero           to   w-def-num-prg          .
       pos-cnf-ins-200.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo operazione        *
      *              *-------------------------------------------------*
           if        w-tes-top-ref        =    "V"
                     go to pos-cnf-ins-600.
       pos-cnf-ins-400.
      *              *-------------------------------------------------*
      *              * Se definizione                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Composizione record 'drf' di [auc]          *
      *                  *---------------------------------------------*
           perform   cmp-drf-auc-000      thru cmp-drf-auc-999        .
      *                  *---------------------------------------------*
      *                  * Scrittura record 'drf' di [auc]             *
      *                  *---------------------------------------------*
           move      "drf "               to   auc-tre                .
           move      w-drf-cod-drf        to   auc-kre                .
           move      w-drf                to   auc-dat                .
           write     auc-rec invalid key
                             go to   pos-cnf-ins-999.
           go to     pos-cnf-ins-999.
       pos-cnf-ins-600.
      *              *-------------------------------------------------*
      *              * Se attribuzione del valore                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Composizione record 'ref' di [ref]          *
      *                  *---------------------------------------------*
           perform   cmp-ref-ref-000      thru cmp-ref-ref-999        .
      *                  *---------------------------------------------*
      *                  * Scrittura record 'ref' di [ref]             *
      *                  *---------------------------------------------*
           write     ref-rec invalid key
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
           if        w-tes-top-ref        =    "V"
                     go to pos-cnf-mod-600.
       pos-cnf-mod-400.
      *              *-------------------------------------------------*
      *              * Se definizione                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Composizione record 'drf' di [auc]          *
      *                  *---------------------------------------------*
           perform   cmp-drf-auc-000      thru cmp-drf-auc-999        .
      *                  *---------------------------------------------*
      *                  * Riscrittura record 'drf' di [auc]           *
      *                  *---------------------------------------------*
           move      "drf "               to   auc-tre                .
           move      w-drf-cod-drf        to   auc-kre                .
           move      w-drf                to   auc-dat                .
           rewrite   auc-rec invalid key
                             go to   pos-cnf-mod-999.
           go to     pos-cnf-mod-999.
       pos-cnf-mod-600.
      *              *-------------------------------------------------*
      *              * Se attribuzione del valore                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Composizione record 'ref' di [ref]          *
      *                  *---------------------------------------------*
           perform   cmp-ref-ref-000      thru cmp-ref-ref-999        .
      *                  *---------------------------------------------*
      *                  * Riscrittura record 'ref' di [ref]           *
      *                  *---------------------------------------------*
           rewrite   ref-rec invalid key
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
           if        w-tes-top-ref        =    "V"
                     go to pos-cnf-ann-600.
       pos-cnf-ann-400.
      *              *-------------------------------------------------*
      *              * Se definizione                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Composizione record 'drf' di [auc]          *
      *                  *---------------------------------------------*
           perform   cmp-drf-auc-000      thru cmp-drf-auc-999        .
      *                  *---------------------------------------------*
      *                  * Delete record 'drf' di [auc]                *
      *                  *---------------------------------------------*
           move      "drf "               to   auc-tre                .
           move      w-drf-cod-drf        to   auc-kre                .
           move      w-drf                to   auc-dat                .
           delete    auc     invalid key
                             go to   pos-cnf-ann-999.
           go to     pos-cnf-ann-999.
       pos-cnf-ann-600.
      *              *-------------------------------------------------*
      *              * Se attribuzione del valore                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Composizione record 'ref' di [ref]          *
      *                  *---------------------------------------------*
           perform   cmp-ref-ref-000      thru cmp-ref-ref-999        .
      *                  *---------------------------------------------*
      *                  * Delete record 'ref' di [ref]                *
      *                  *---------------------------------------------*
           delete    ref     invalid key
                             go to   pos-cnf-ann-999.
           go to     pos-cnf-ann-999.
       pos-cnf-ann-999.
           exit.

      *    *===========================================================*
      *    * Composizione record 'drf' di [auc]                        *
      *    *-----------------------------------------------------------*
       cmp-drf-auc-000.
           move      spaces               to   w-drf                  .
           move      w-tes-cod-ref        to   w-drf-cod-drf          .
           move      w-tes-des-ref (1)    to   w-drf-des-drf          .
           move      w-tes-tip-ref (1)    to   w-drf-tip-drf          .
           move      w-tes-car-ref (1)    to   w-drf-car-drf          .
           move      w-tes-dec-ref (1)    to   w-drf-dec-drf          .
           move      w-tes-sgn-ref (1)    to   w-drf-sgn-drf          .
           move      w-tes-som-ref (1)    to   w-drf-som-drf          .
           move      w-tes-alx-ref (1)    to   w-drf-alx-drf          .
           move      w-tes-com-ref (1)    to   w-drf-com-drf          .
       cmp-drf-auc-999.
           exit.

      *    *===========================================================*
      *    * Composizione record 'ref' di [ref]                        *
      *    *-----------------------------------------------------------*
       cmp-ref-ref-000.
           move      spaces               to   ref-rec                .
           move      "ref "               to   ref-tre                .
           move      w-tes-cod-ref        to   ref-cod                .
           move      w-tes-prg-ref        to   ref-prg                .
           if        w-tes-tip-ref (1)    =    "A"
                     move  w-tes-alf-ref (1)
                                          to   ref-alf
           else if   w-tes-tip-ref (1)    =    "N"
                     move  w-tes-num-ref (1)
                                          to   ref-num
           else if   w-tes-tip-ref (1)    =    "D"
                     move  w-tes-dat-ref (1)
                                          to   ref-dat                .
       cmp-ref-ref-999.
           exit.

      *    *===========================================================*
      *    * Find su archivio [ref]                                    *
      *    *-----------------------------------------------------------*
       fnd-arc-ref-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-fnd-arc-ref-sel      .
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pxpg5210"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     move  "#"            to   w-fnd-arc-ref-sel
                     go to  fnd-arc-ref-999.
      *              *-------------------------------------------------*
      *              * Preparazione variabile di i.p.c. per tipo re-   *
      *              * cord da considerare                             *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "tip-rec"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      w-fnd-arc-ref-ope    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Preparazione variabile di i.p.c. per nome per-  *
      *              * sonalizzazione da considerare                   *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "nom-ref"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      40                   to   s-car                  .
           move      w-fnd-arc-ref-nom    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       fnd-arc-ref-500.
      *              *-------------------------------------------------*
      *              * Richiamo programma di interrogazione            *
      *              *-------------------------------------------------*
           call      "swd/xpg/prg/obj/pxpg5210"                       .
           cancel    "swd/xpg/prg/obj/pxpg5210"                       .
      *              *-------------------------------------------------*
      *              * Estrazione di eventuale variabile di i.p.c. de- *
      *              * terminata da function-key "SLCT" durante l'in-  *
      *              * terrogazione                                    *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "nep-ref"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     move  "#"            to   w-fnd-arc-ref-sel
                     go to fnd-arc-ref-999.
           move      s-alf                to   w-fnd-arc-ref-val      .
       fnd-arc-ref-999.
           exit.

      *    *===========================================================*
      *    * Separazione della chiave della referenza in due componen- *
      *    * ti, quella prima e quella dopo l'eventuale carattere      *
      *    * chiocciola di separazione                                 *
      *    *-----------------------------------------------------------*
       sep-key-ref-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione delle due componenti            *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-ref-pre          .
           move      spaces               to   w-key-ref-pos          .
      *              *-------------------------------------------------*
      *              * Inizializzazione del contatore del numero di    *
      *              * caratteri che precedono la chiocciola           *
      *              *-------------------------------------------------*
           move      zero                 to   w-key-ref-pnt          .
      *              *-------------------------------------------------*
      *              * Conteggio del numero di caratteri che precedono *
      *              * la chiocciola                                   *
      *              *-------------------------------------------------*
           inspect   w-key-ref-key    tallying w-key-ref-pnt
                                for characters
                                before initial "@"                    .
      *              *-------------------------------------------------*
      *              * Se il carattere chiocciola non e' presente : si *
      *              * prepara solo la parte precedente e si esce      *
      *              *-------------------------------------------------*
           if        w-key-ref-pnt        =    40
                     move  w-key-ref-key
                                          to   w-key-ref-pre
                     go to sep-key-ref-999.
      *              *-------------------------------------------------*
      *              * Preparazione della componente che precede la    *
      *              * chiocciola                                      *
      *              *-------------------------------------------------*
           move      w-key-ref-key
                    (1 : w-key-ref-pnt)   to   w-key-ref-pre          .
      *              *-------------------------------------------------*
      *              * Preparazione del contatore caratteri in modo da *
      *              * farlo puntare all'inizio della seconda parte    *
      *              *-------------------------------------------------*
           add       2                    to   w-key-ref-pnt          .
      *              *-------------------------------------------------*
      *              * Se non ci sono caratteri da separare come se-   *
      *              * conda parte si esce lasciando a spaces la se-   *
      *              * conda componente                                *
      *              *-------------------------------------------------*
           if        w-key-ref-pnt        >    40
                     go to sep-key-ref-999.
      *              *-------------------------------------------------*
      *              * Preparazione della componente che segue la      *
      *              * chiocciola                                      *
      *              *-------------------------------------------------*
           move      40                   to   w-key-ref-lun          .
           subtract  w-key-ref-pnt        from w-key-ref-lun          .
           add       1                    to   w-key-ref-lun          .
           move      w-key-ref-key
                    (w-key-ref-pnt :
                     w-key-ref-lun)       to   w-key-ref-pos          .
       sep-key-ref-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

