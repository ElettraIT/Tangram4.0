       Identification Division.
       Program-Id.                                 pxpg4100           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    swd                 *
      *                        Area gestionale:    xpg                 *
      *                                Settore:    sds                 *
      *                                   Fase:    xpg410              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 31/03/93    *
      *                       Versione attuale:    NdK del 30/04/10    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione tipi stampante                     *
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
      *    * File Control [seq]                                        *
      *    *-----------------------------------------------------------*
           select            seq   assign to input-output   f-seq-pat
                             organization is line sequential
                             access  mode is sequential
                             file  status is                f-seq-sts .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [seq]                                    *
      *    *-----------------------------------------------------------*
       fd  seq  label record omitted.
      *    *-----------------------------------------------------------*
      *    * Record                                                    *
      *    *-----------------------------------------------------------*
       01  seq-rec.
      *        *-------------------------------------------------------*
      *        * Caratteri del record                                  *
      *        *-------------------------------------------------------*
           05  seq-chr  occurs 2048       pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area ausiliaria per controllo i-o su [seq]                *
      *    *-----------------------------------------------------------*
       01  f-seq.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-seq-nam                  pic  x(04) value "seq "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-seq-pat                  pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-seq-sts                  pic  x(02) value "00"       .

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
                     "sds"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "xpg410"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pxpg4100"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "        GESTIONE TIPI STAMPANTE         "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mpslct"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/r"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli                "mppssx"  *
      *    *-----------------------------------------------------------*
       01  j.
      *        *-------------------------------------------------------*
      *        * Tipo operazione                                       *
      *        *-------------------------------------------------------*
           05  j-ope                      pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo record                                           *
      *        *-------------------------------------------------------*
           05  j-tre                      pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Chiave record                                         *
      *        *-------------------------------------------------------*
           05  j-kre                      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Dati record                                           *
      *        *-------------------------------------------------------*
           05  j-dat.
               10  j-chr occurs 2048      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Return status code                                    *
      *        *-------------------------------------------------------*
           05  j-rsc                      pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Return message                                        *
      *        *-------------------------------------------------------*
           05  j-msg                      pic  x(80)                  .

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
      *    * Work per records di [pss] 'tst'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/wpsstst0.cpw"                   .

      *    *===========================================================*
      *    * Work per clonazioni                                       *
      *    *-----------------------------------------------------------*
       01  w-clo.
      *        *-------------------------------------------------------*
      *        * Work per clonazione tipo stampante                    *
      *        *-------------------------------------------------------*
           05  w-clo-tst.
      *            *---------------------------------------------------*
      *            * Codice tipo stampante per salvataggio             *
      *            *---------------------------------------------------*
               10  w-clo-tst-cod-tst      pic  x(08)                  .
      *            *---------------------------------------------------*
      *            * Record tipo stampante                             *
      *            *---------------------------------------------------*
               10  w-clo-tst-rec-tst.
                   15  filler occurs 2048 pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per comodi di lavoro                            *
      *    *-----------------------------------------------------------*
       01  w-wrk.
      *        *-------------------------------------------------------*
      *        * Contatori                                             *
      *        *-------------------------------------------------------*
           05  w-wrk-ctr-00a              pic  9(03)                  .
           05  w-wrk-ctr-00b              pic  9(03)                  .
           05  w-wrk-ctr-00c              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Area editata per aree non stampabili                  *
      *        *-------------------------------------------------------*
           05  w-edt-nxx-are.
               10  w-edt-nxx-ape          pic  x(01)       value "("  .
               10  w-edt-nxx-e04          pic  x(04)                  .
               10  w-edt-nxx-chi          pic  x(01)       value ")"  .
      *        *-------------------------------------------------------*
      *        * Area editata per 16 spaziature orizzontali/verticali  *
      *        *-------------------------------------------------------*
           05  w-edt-sov-are.
               10  w-edt-sov-1o2 occurs 2.
                   15  w-edt-sov-sub occurs 8.
                       20  w-edt-sov-ele  pic  9(02),9(02)            .
                       20  filler         pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area editata per 08 parametri per driver stampa       *
      *        *-------------------------------------------------------*
           05  w-edt-pds-are.
               15  w-edt-pds-sub occurs 8.
                   20  w-edt-pds-ele      pic  x(05)                  .
                   20  filler             pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Find                              *
      *    *-----------------------------------------------------------*
       01  w-fnd.
      *        *-------------------------------------------------------*
      *        * Work per Find su tipi stampante di [pss]              *
      *        *-------------------------------------------------------*
           05  w-fnd-pss-tst.
               10  w-fnd-pss-tst-sel      pic  x(01)                  .
               10  w-fnd-pss-tst-cod      pic  x(08)                  .
               
      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Salvataggi per classe stampante                       *
      *        *-------------------------------------------------------*
           05  w-sav-cla-tst              pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Salvataggi per sottoclasse stampante                  *
      *        *-------------------------------------------------------*
           05  w-sav-scl-tst              pic  x(04)                  .

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
      *    * Work per accettazione campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per classe tipo stampante                        *
      *        *-------------------------------------------------------*
           05  w-exp-cla-tst.
               10  w-exp-cla-tst-num      pic  9(02)       value 5    .
               10  w-exp-cla-tst-lun      pic  9(02)       value 20   .
               10  w-exp-cla-tst-tbl.
                   15  filler             pic  x(20) value
                            "Normale             "                    .
                   15  filler             pic  x(20) value
                            "PostScript          "                    .
                   15  filler             pic  x(20) value
                            "PCL5                "                    .
                   15  filler             pic  x(20) value
                            "Special Printer     "                    .
                   15  filler             pic  x(20) value
                            "Fax                 "                    .
      *        *-------------------------------------------------------*
      *        * Work per sottoclasse tipo stampante                   *
      *        *-------------------------------------------------------*
           05  w-exp-scl-tst.
               10  w-exp-scl-tst-num      pic  9(02)       value 20   .
               10  w-exp-scl-tst-lun      pic  9(02)       value 04   .
               10  w-exp-scl-tst-tbl.
                   15  w-exp-scl-tst-tbx occurs 20
                                          pic  x(04)                  .
               10  w-exp-scl-tst-ctr      pic  9(02)                  .
               10  w-exp-scl-tst-nor.
                   15  w-exp-scl-tst-nor-num
                                          pic  9(02)       value 1    .
                   15  w-exp-scl-tst-nor-tbl.
                       20  filler         pic  x(04) value "    "     .
               10  w-exp-scl-tst-pst.
                   15  w-exp-scl-tst-pst-num
                                          pic  9(02)       value 5    .
                   15  w-exp-scl-tst-pst-tbl.
                       20  filler         pic  x(04) value "A4  "     .
                       20  filler         pic  x(04) value "A3  "     .
                       20  filler         pic  x(04) value "A2  "     .
                       20  filler         pic  x(04) value "A1  "     .
                       20  filler         pic  x(04) value "A0  "     .
               10  w-exp-scl-tst-hpg.
                   15  w-exp-scl-tst-hpg-num
                                          pic  9(02)       value 5    .
                   15  w-exp-scl-tst-hpg-tbl.
                       20  filler         pic  x(04) value "A4  "     .
                       20  filler         pic  x(04) value "A3  "     .
                       20  filler         pic  x(04) value "A2  "     .
                       20  filler         pic  x(04) value "A1  "     .
                       20  filler         pic  x(04) value "A0  "     .
      *        *-------------------------------------------------------*
      *        * Work per : Classe di appartenenza                     *
      *        *-------------------------------------------------------*
           05  w-exp-cla-app.
               10  w-exp-cla-app-num      pic  9(02)       value 2    .
               10  w-exp-cla-app-lun      pic  9(02)       value 20   .
               10  w-exp-cla-app-tbl.
                   15  filler             pic  x(20) value
                            "Standard            "                    .
                   15  filler             pic  x(20) value
                            "Su misura           "                    .

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

      *    *===========================================================*
      *    * Declaratives                                              *
      *    *-----------------------------------------------------------*
       Declaratives.
       Decl Section.
           Use after standard error procedure on seq                  .
       decl-000.
      *              *-------------------------------------------------*
      *              * Traslazione del codice di i-o status contenuto  *
      *              * in f-lsf-sts nel codice di i-o status conven-   *
      *              * zionale                                         *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/miosts"
                                         using f-seq                  .
      *              *-------------------------------------------------*
      *              * Spostamento cobol-file-status in area per defi- *
      *              * nizione codici di errore di i-o                 *
      *              *-------------------------------------------------*
           move      f-seq-sts            to   e-sts                  .
       End Declaratives.

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
      *                  * Salvataggio record                          *
      *                  *---------------------------------------------*
           move      w-tst                to   w-clo-tst-rec-tst      .
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
      *              * Funzione Open  per modulo              "mppssf" *
      *              *-------------------------------------------------*
           move      "OP"                 to   j-ope                  .
           call      "swd/mod/prg/obj/mppssf"
                                        using  r
                                               j                      .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * Funzione Close per modulo              "mppssf" *
      *              *-------------------------------------------------*
           move      "CL"                 to   j-ope                  .
           call      "swd/mod/prg/obj/mppssf"
                                        using  r
                                               j                      .
      *              *-------------------------------------------------*
      *              * Test di cancellabilita' per modulo     "mppssf" *
      *              *-------------------------------------------------*
           move      "X?"                 to   j-ope                  .
           call      "swd/mod/prg/obj/mppssf"
                                        using  r
                                               j                      .
           if        j-rsc                not  = spaces
                     go to rou-cls-fls-999.
      *              *-------------------------------------------------*
      *              * Cancellazione modulo                   "mppssf" *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mppssf"                         .
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
       acc-key-reg-150.
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
       acc-key-reg-200.
      *                  *---------------------------------------------*
      *                  * Codice tipo stampante                       *
      *                  *---------------------------------------------*
           perform   acc-cod-tst-000      thru acc-cod-tst-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
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
      *              * Codice tipo stampante                           *
      *              *-------------------------------------------------*
           perform   vis-cod-tst-000      thru vis-cod-tst-999        .
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
      *              * Codice tipo stampante                           *
      *              *-------------------------------------------------*
           perform   pmt-cod-tst-000      thru pmt-cod-tst-999        .
      *              *-------------------------------------------------*
      *              * Lineette di separazione a linea 05              *
      *              *-------------------------------------------------*
           perform   pmt-lds-l05-000      thru pmt-lds-l05-999        .
       pmt-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Prompt per codice tipo stampante                          *
      *    *-----------------------------------------------------------*
       pmt-cod-tst-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice tipo stampante      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-tst-999.
           exit.

      *    *===========================================================*
      *    * Prompt per lineette di separazione a linea 05             *
      *    *-----------------------------------------------------------*
       pmt-lds-l05-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-lds-l05-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice tipo stampante                      *
      *    *-----------------------------------------------------------*
       acc-cod-tst-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-tst-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tst-cod-tst        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-tst-999.
       acc-cod-tst-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tst-cod-tst          .
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-cod-tst-400.
      *                  *---------------------------------------------*
      *                  * Find su tipi stampante                      *
      *                  *---------------------------------------------*
           move      w-tst-cod-tst        to   w-fnd-pss-tst-cod      .
           perform   fnd-pss-tst-000      thru fnd-pss-tst-999        .
      *                  *---------------------------------------------*
      *                  * Se nessuna selezione : reimpostazione       *
      *                  *---------------------------------------------*
           if        w-fnd-pss-tst-sel    not  = spaces
                     go to acc-cod-tst-100.
      *                  *---------------------------------------------*
      *                  * Memorizzazione codice selezionato           *
      *                  *---------------------------------------------*
           move      w-fnd-pss-tst-cod    to   w-tst-cod-tst          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione codice selezionato          *
      *                  *---------------------------------------------*
           perform   vis-cod-tst-000      thru vis-cod-tst-999        .
      *                  *---------------------------------------------*
      *                  * Normalizzazione function key                *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
       acc-cod-tst-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cod-tst-425.
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      w-tst-cod-tst        to   w-all-str-alf          .
           move      08                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-cod-tst-100.
       acc-cod-tst-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-tst-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-tst-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-tst-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-tst-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-tst-999.
       acc-cod-tst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice tipo stampante                   *
      *    *-----------------------------------------------------------*
       vis-cod-tst-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tst-cod-tst        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-tst-999.
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
       acc-tes-reg-150.
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
       acc-tes-reg-200.
      *                  *---------------------------------------------*
      *                  * Descrizione tipo stampante                  *
      *                  *---------------------------------------------*
           perform   acc-des-tst-000      thru acc-des-tst-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-250.
      *                  *---------------------------------------------*
      *                  * Driver di stampa                            *
      *                  *---------------------------------------------*
           perform   acc-drv-tst-000      thru acc-drv-tst-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-200.
       acc-tes-reg-275.
      *                  *---------------------------------------------*
      *                  * Parametri per il driver di stampa           *
      *                  *---------------------------------------------*
           perform   acc-par-pds-000      thru acc-par-pds-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-250.
       acc-tes-reg-300.
      *                  *---------------------------------------------*
      *                  * Classe tipo stampante                       *
      *                  *---------------------------------------------*
           perform   acc-cla-tst-000      thru acc-cla-tst-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-275.
       acc-tes-reg-350.
      *                  *---------------------------------------------*
      *                  * Sottoclasse tipo stampante                  *
      *                  *---------------------------------------------*
           perform   acc-scl-tst-000      thru acc-scl-tst-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-300.
       acc-tes-reg-400.
      *                  *---------------------------------------------*
      *                  * Larghezza carrello                          *
      *                  *---------------------------------------------*
           perform   acc-l72-tst-000      thru acc-l72-tst-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-350.
       acc-tes-reg-450.
      *                  *---------------------------------------------*
      *                  * Altezza carrello                            *
      *                  *---------------------------------------------*
           perform   acc-a72-tst-000      thru acc-a72-tst-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-400.
       acc-tes-reg-500.
      *                  *---------------------------------------------*
      *                  * Margine superiore                           *
      *                  *---------------------------------------------*
           perform   acc-msu-tst-000      thru acc-msu-tst-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-450.
       acc-tes-reg-525.
      *                  *---------------------------------------------*
      *                  * Area non stampabile superiore               *
      *                  *---------------------------------------------*
           perform   acc-nsu-tst-000      thru acc-nsu-tst-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-500.
       acc-tes-reg-550.
      *                  *---------------------------------------------*
      *                  * Margine inferiore                           *
      *                  *---------------------------------------------*
           perform   acc-min-tst-000      thru acc-min-tst-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-525.
       acc-tes-reg-575.
      *                  *---------------------------------------------*
      *                  * Area non stampabile inferiore               *
      *                  *---------------------------------------------*
           perform   acc-nin-tst-000      thru acc-nin-tst-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-550.
       acc-tes-reg-600.
      *                  *---------------------------------------------*
      *                  * Margine sinistro                            *
      *                  *---------------------------------------------*
           perform   acc-msi-tst-000      thru acc-msi-tst-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-575.
       acc-tes-reg-625.
      *                  *---------------------------------------------*
      *                  * Area non stampabile sinistra                *
      *                  *---------------------------------------------*
           perform   acc-nsi-tst-000      thru acc-nsi-tst-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-600.
       acc-tes-reg-650.
      *                  *---------------------------------------------*
      *                  * Margine destro                              *
      *                  *---------------------------------------------*
           perform   acc-mde-tst-000      thru acc-mde-tst-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-625.
       acc-tes-reg-675.
      *                  *---------------------------------------------*
      *                  * Area non stampabile destra                  *
      *                  *---------------------------------------------*
           perform   acc-nde-tst-000      thru acc-nde-tst-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-650.
       acc-tes-reg-680.
      *                  *---------------------------------------------*
      *                  * Classe di appartenenza                      *
      *                  *---------------------------------------------*
           perform   acc-cla-app-000      thru acc-cla-app-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-675.
       acc-tes-reg-685.
      *                  *---------------------------------------------*
      *                  * Sistema applicativo di appartenenza         *
      *                  *---------------------------------------------*
           perform   acc-sap-app-000      thru acc-sap-app-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-680.
       acc-tes-reg-700.
      *                  *---------------------------------------------*
      *                  * Spaziature orizzontali                      *
      *                  *---------------------------------------------*
           perform   acc-spa-ori-000      thru acc-spa-ori-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-685.
       acc-tes-reg-750.
      *                  *---------------------------------------------*
      *                  * Spaziature verticali                        *
      *                  *---------------------------------------------*
           perform   acc-spa-ver-000      thru acc-spa-ver-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-700.
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
      *              * Descrizione tipo stampante                      *
      *              *-------------------------------------------------*
           perform   vis-des-tst-000      thru vis-des-tst-999        .
      *              *-------------------------------------------------*
      *              * Driver di stampa                                *
      *              *-------------------------------------------------*
           perform   vis-drv-tst-000      thru vis-drv-tst-999        .
      *              *-------------------------------------------------*
      *              * Parametri per il driver di stampa               *
      *              *-------------------------------------------------*
           perform   vis-par-pds-000      thru vis-par-pds-999        .
      *              *-------------------------------------------------*
      *              * Classe tipo stampante                           *
      *              *-------------------------------------------------*
           perform   vis-cla-tst-000      thru vis-cla-tst-999        .
      *              *-------------------------------------------------*
      *              * Sottoclasse tipo stampante                      *
      *              *-------------------------------------------------*
           perform   vis-scl-tst-000      thru vis-scl-tst-999        .
      *              *-------------------------------------------------*
      *              * Larghezza carrello                              *
      *              *-------------------------------------------------*
           perform   vis-l72-tst-000      thru vis-l72-tst-999        .
      *              *-------------------------------------------------*
      *              * Altezza carrello                                *
      *              *-------------------------------------------------*
           perform   vis-a72-tst-000      thru vis-a72-tst-999        .
      *              *-------------------------------------------------*
      *              * Margine superiore                               *
      *              *-------------------------------------------------*
           perform   vis-msu-tst-000      thru vis-msu-tst-999        .
      *              *-------------------------------------------------*
      *              * Area non stampabile superiore                   *
      *              *-------------------------------------------------*
           perform   vis-nsu-tst-000      thru vis-nsu-tst-999        .
      *              *-------------------------------------------------*
      *              * Margine inferiore                               *
      *              *-------------------------------------------------*
           perform   vis-min-tst-000      thru vis-min-tst-999        .
      *              *-------------------------------------------------*
      *              * Area non stampabile inferiore                   *
      *              *-------------------------------------------------*
           perform   vis-nin-tst-000      thru vis-nin-tst-999        .
      *              *-------------------------------------------------*
      *              * Margine sinistro                                *
      *              *-------------------------------------------------*
           perform   vis-msi-tst-000      thru vis-msi-tst-999        .
      *              *-------------------------------------------------*
      *              * Area non stampabile sinistra                    *
      *              *-------------------------------------------------*
           perform   vis-nsi-tst-000      thru vis-nsi-tst-999        .
      *              *-------------------------------------------------*
      *              * Margine destro                                  *
      *              *-------------------------------------------------*
           perform   vis-mde-tst-000      thru vis-mde-tst-999        .
      *              *-------------------------------------------------*
      *              * Area non stampabile destra                      *
      *              *-------------------------------------------------*
           perform   vis-nde-tst-000      thru vis-nde-tst-999        .
      *              *-------------------------------------------------*
      *              * Classe di appartenenza                          *
      *              *-------------------------------------------------*
           perform   vis-cla-app-000      thru vis-cla-app-999        .
      *              *-------------------------------------------------*
      *              * Sistema applicativo di appartenenza             *
      *              *-------------------------------------------------*
           perform   vis-sap-app-000      thru vis-sap-app-999        .
      *              *-------------------------------------------------*
      *              * Spaziature orizzontali                          *
      *              *-------------------------------------------------*
           perform   vis-spa-ori-000      thru vis-spa-ori-999        .
      *              *-------------------------------------------------*
      *              * Spaziature verticali                            *
      *              *-------------------------------------------------*
           perform   vis-spa-ver-000      thru vis-spa-ver-999        .
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
           move      06                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Descrizione tipo stampante                      *
      *              *-------------------------------------------------*
           perform   pmt-des-tst-000      thru pmt-des-tst-999        .
      *              *-------------------------------------------------*
      *              * Driver di stampa                                *
      *              *-------------------------------------------------*
           perform   pmt-drv-tst-000      thru pmt-drv-tst-999        .
      *              *-------------------------------------------------*
      *              * Parametri per il driver di stampa               *
      *              *-------------------------------------------------*
           perform   pmt-par-pds-000      thru pmt-par-pds-999        .
      *              *-------------------------------------------------*
      *              * Classe tipo stampante                           *
      *              *-------------------------------------------------*
           perform   pmt-cla-tst-000      thru pmt-cla-tst-999        .
      *              *-------------------------------------------------*
      *              * Sottoclasse tipo stampante                      *
      *              *-------------------------------------------------*
           perform   pmt-scl-tst-000      thru pmt-scl-tst-999        .
      *              *-------------------------------------------------*
      *              * Larghezza carrello                              *
      *              *-------------------------------------------------*
           perform   pmt-l72-tst-000      thru pmt-l72-tst-999        .
      *              *-------------------------------------------------*
      *              * Altezza carrello                                *
      *              *-------------------------------------------------*
           perform   pmt-a72-tst-000      thru pmt-a72-tst-999        .
      *              *-------------------------------------------------*
      *              * Margine superiore (e top)                       *
      *              *-------------------------------------------------*
           perform   pmt-msu-tst-000      thru pmt-msu-tst-999        .
      *              *-------------------------------------------------*
      *              * Margine inferiore (e bottom)                    *
      *              *-------------------------------------------------*
           perform   pmt-min-tst-000      thru pmt-min-tst-999        .
      *              *-------------------------------------------------*
      *              * Margine sinistro (e left)                       *
      *              *-------------------------------------------------*
           perform   pmt-msi-tst-000      thru pmt-msi-tst-999        .
      *              *-------------------------------------------------*
      *              * Margine destro (e right)                        *
      *              *-------------------------------------------------*
           perform   pmt-mde-tst-000      thru pmt-mde-tst-999        .
      *              *-------------------------------------------------*
      *              * Classe di appartenenza                          *
      *              *-------------------------------------------------*
           perform   pmt-cla-app-000      thru pmt-cla-app-999        .
      *              *-------------------------------------------------*
      *              * Sistema applicativo di appartenenza             *
      *              *-------------------------------------------------*
           perform   pmt-sap-app-000      thru pmt-sap-app-999        .
      *              *-------------------------------------------------*
      *              * Spaziature orizzontali                          *
      *              *-------------------------------------------------*
           perform   pmt-spa-ori-000      thru pmt-spa-ori-999        .
      *              *-------------------------------------------------*
      *              * Spaziature verticali                            *
      *              *-------------------------------------------------*
           perform   pmt-spa-ver-000      thru pmt-spa-ver-999        .
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Descrizione tipo stampante                       *
      *    *-----------------------------------------------------------*
       pmt-des-tst-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Descrizione tipo stampante :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-des-tst-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Driver di stampa                                 *
      *    *-----------------------------------------------------------*
       pmt-drv-tst-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Driver di stampa           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-drv-tst-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Parametri per il driver di stampa                *
      *    *-----------------------------------------------------------*
       pmt-par-pds-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Parametri per il driver    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-par-pds-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Classe tipo stampante                            *
      *    *-----------------------------------------------------------*
       pmt-cla-tst-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Classe stampante           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cla-tst-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Sottoclasse tipo stampante                       *
      *    *-----------------------------------------------------------*
       pmt-scl-tst-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Sottoclasse stampante      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-scl-tst-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Larghezza carrello                               *
      *    *-----------------------------------------------------------*
       pmt-l72-tst-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Larghezza carrello         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-l72-tst-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Altezza carrello                                 *
      *    *-----------------------------------------------------------*
       pmt-a72-tst-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Altezza   carrello         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-a72-tst-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Margine superiore (e top)                        *
      *    *-----------------------------------------------------------*
       pmt-msu-tst-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Margine superiore (e top)  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-msu-tst-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Margine inferiore ( e bottom)                    *
      *    *-----------------------------------------------------------*
       pmt-min-tst-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Margine inferiore (e bottom):"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-min-tst-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Margine sinistro (e left)                        *
      *    *-----------------------------------------------------------*
       pmt-msi-tst-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Margine sinistro (e left)  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-msi-tst-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Margine destro (e right)                         *
      *    *-----------------------------------------------------------*
       pmt-mde-tst-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Margine destro (e right)   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-mde-tst-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Classe di appartenenza                           *
      *    *-----------------------------------------------------------*
       pmt-cla-app-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Classe di appartenenza     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cla-app-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Sistema applicativo di appartenenza              *
      *    *-----------------------------------------------------------*
       pmt-sap-app-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      21                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      56                   to   v-pos                  .
           move      "Sistema applicativo :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-sap-app-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Spaziature orizzontali                           *
      *    *-----------------------------------------------------------*
       pmt-spa-ori-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Spaziature orizzontali     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-spa-ori-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Spaziature verticali                             *
      *    *-----------------------------------------------------------*
       pmt-spa-ver-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Spaziature verticali       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-spa-ver-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Descrizione tipo stampante                 *
      *    *-----------------------------------------------------------*
       acc-des-tst-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-des-tst-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tst-des-tst        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-des-tst-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-des-tst-999.
       acc-des-tst-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tst-des-tst          .
       acc-des-tst-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-des-tst-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-des-tst-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-des-tst-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-des-tst-100.
       acc-des-tst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Descrizione tipo stampante              *
      *    *-----------------------------------------------------------*
       vis-des-tst-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tst-des-tst        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-tst-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Driver di stampa                           *
      *    *-----------------------------------------------------------*
       acc-drv-tst-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-drv-tst-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tst-drv-tst        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-drv-tst-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-drv-tst-999.
       acc-drv-tst-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tst-drv-tst          .
       acc-drv-tst-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-drv-tst-425.
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      w-tst-drv-tst        to   w-all-str-alf          .
           move      10                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-drv-tst-100.
       acc-drv-tst-450.
      *                  *---------------------------------------------*
      *                  * Test che il valore non manchi, a meno che   *
      *                  * non si sia in Up                            *
      *                  *---------------------------------------------*
           if        w-tst-drv-tst        =    spaces
                     if    v-key          =    "UP  "
                           go to acc-drv-tst-600
                     else  go to acc-drv-tst-100.
      *                  *---------------------------------------------*
      *                  * Che il file esista                          *
      *                  *---------------------------------------------*
       acc-drv-tst-452.
           if        w-tst-drv-tst        =    spaces
                     go to acc-drv-tst-600.
       acc-drv-tst-454.
           perform   opn-fil-seq-000      thru opn-fil-seq-999        .
           if        f-seq-sts            =    e-not-err
                     go to acc-drv-tst-456
           else      go to acc-drv-tst-458.
       acc-drv-tst-456.
           perform   cls-fil-seq-000      thru cls-fil-seq-999        .
           go to     acc-drv-tst-600.
       acc-drv-tst-458.
           perform   cls-fil-seq-000      thru cls-fil-seq-999        .
           move      "Non esiste nessun file corrispondente al pathname 
      -              "impostato !    "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     acc-drv-tst-100.
       acc-drv-tst-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-drv-tst-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-drv-tst-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-drv-tst-100.
       acc-drv-tst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Driver di stampa                        *
      *    *-----------------------------------------------------------*
       vis-drv-tst-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tst-drv-tst        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-drv-tst-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Parametri per il driver di stampa          *
      *    *-----------------------------------------------------------*
       acc-par-pds-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-par-pds-050.
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatore 01..08           *
      *                  *---------------------------------------------*
           move      01                   to   w-wrk-ctr-00a          .
       acc-par-pds-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      08                   to   v-lin                  .
           move      w-wrk-ctr-00a        to   v-pos                  .
           multiply  6                    by   v-pos                  .
           add       24                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "TAB "               to   v-pfk (10)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tst-par-pds
                    (w-wrk-ctr-00a)       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-par-pds-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-par-pds-999.
       acc-par-pds-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tst-par-pds
                                              (w-wrk-ctr-00a)         .
       acc-par-pds-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-par-pds-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-par-pds-625.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda della function key     *
      *                  *---------------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-par-pds-650
           else if   v-key                =    spaces or
                     v-key                =    "DOWN"
                     go to acc-par-pds-800
           else if   v-key                =    "DO  "
                     go to acc-par-pds-800
           else      go to acc-par-pds-675.
       acc-par-pds-650.
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
           if        w-wrk-ctr-00a        =    1
                     go to acc-par-pds-800.
           subtract  1                    from w-wrk-ctr-00a          .
           go to     acc-par-pds-100.
       acc-par-pds-675.
      *                  *---------------------------------------------*
      *                  * Se Tab                                      *
      *                  *---------------------------------------------*
           if        w-wrk-ctr-00a        =    08
                     go to acc-par-pds-800.
           add       1                    to   w-wrk-ctr-00a          .
           go to     acc-par-pds-100.
       acc-par-pds-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-par-pds-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-par-pds-100.
       acc-par-pds-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Parametri per il driver di stampa       *
      *    *-----------------------------------------------------------*
       vis-par-pds-000.
           move      spaces               to   w-edt-pds-are          .
           move      zero                 to   w-wrk-ctr-00a          .
           move      zero                 to   w-wrk-ctr-00c          .
       vis-par-pds-200.
           add       1                    to   w-wrk-ctr-00c          .
           if        w-wrk-ctr-00c        >    8
                     go to vis-par-pds-300.
           add       1                    to   w-wrk-ctr-00a          .
           if        w-tst-par-pds
                    (w-wrk-ctr-00a)       =    spaces
                     go to vis-par-pds-200.
           move      w-tst-par-pds
                    (w-wrk-ctr-00a)       to   w-edt-pds-ele
                                              (w-wrk-ctr-00c)         .
           go to     vis-par-pds-200.
       vis-par-pds-300.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      48                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-edt-pds-are        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-par-pds-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Classe tipo stampante                      *
      *    *-----------------------------------------------------------*
       acc-cla-tst-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tst-cla-tst        to   w-sav-cla-tst          .
       acc-cla-tst-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-cla-tst-lun    to   v-car                  .
           move      w-exp-cla-tst-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-exp-cla-tst-tbl    to   v-txt                  .
           if        w-tst-cla-tst        =    spaces
                     move  1              to   v-num
           else if   w-tst-cla-tst        =    "PS  "
                     move  2              to   v-num
           else if   w-tst-cla-tst        =    "PCL5"
                     move  3              to   v-num
           else if   w-tst-cla-tst        =    "SPPX"
                     move  4              to   v-num
           else if   w-tst-cla-tst        =    "FAX "
                     move  5              to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cla-tst-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cla-tst-999.
       acc-cla-tst-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    1
                     move  spaces         to   w-tst-cla-tst
           else if   v-num                =    2
                     move  "PS  "         to   w-tst-cla-tst
           else if   v-num                =    3
                     move  "PCL5"         to   w-tst-cla-tst
           else if   v-num                =    4
                     move  "SPPX"         to   w-tst-cla-tst
           else if   v-num                =    5
                     move  "FAX "         to   w-tst-cla-tst
           else      move  spaces         to   w-tst-cla-tst          .
       acc-cla-tst-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cla-tst-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cla-tst-625.
      *                  *---------------------------------------------*
      *                  * Se valore cambiato si normalizza la sotto-  *
      *                  * classe                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-tst-cla-tst        =    w-sav-cla-tst
                     go to acc-cla-tst-650.
      *                      *-----------------------------------------*
      *                      * Sottoclasse                             *
      *                      *-----------------------------------------*
           move      spaces               to   w-tst-scl-tst          .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cla-tst-650.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione della clase stampan- *
      *                  * te, per la normalizzazione e visualizza-    *
      *                  * zione di :                                  *
      *                  *  - Larghezza carrello                       *
      *                  *  - Altezza carrello                         *
      *                  *  - Spaziature orizzontali                   *
      *                  *  - Spaziature verticali                     *
      *                  *---------------------------------------------*
           if        w-tst-cla-tst        =    spaces
                     go to acc-cla-tst-660
           else if   w-tst-cla-tst        =    "PS  "
                     go to acc-cla-tst-670
           else if   w-tst-cla-tst        =    "PCL5"
                     go to acc-cla-tst-680
           else if   w-tst-cla-tst        =    "SPPX" or
                     w-tst-cla-tst        =    "FAX "
                     go to acc-cla-tst-690.
       acc-cla-tst-660.
      *                  *---------------------------------------------*
      *                  * Se classe : normale                         *
      *                  *---------------------------------------------*
       acc-cla-tst-662.
      *                      *-----------------------------------------*
      *                      * Nessuna normalizzazione                 *
      *                      *-----------------------------------------*
           go to     acc-cla-tst-800.
       acc-cla-tst-670.
      *                  *---------------------------------------------*
      *                  * Se classe : PostScript                      *
      *                  *---------------------------------------------*
       acc-cla-tst-672.
      *                      *-----------------------------------------*
      *                      * Larghezza carrello                      *
      *                      *-----------------------------------------*
           move      zero                 to   w-tst-l72-tst          .
           perform   vis-l72-tst-000      thru vis-l72-tst-999        .
      *                      *-----------------------------------------*
      *                      * Altezza carrello                        *
      *                      *-----------------------------------------*
           move      zero                 to   w-tst-a72-tst          .
           perform   vis-a72-tst-000      thru vis-a72-tst-999        .
      *                      *-----------------------------------------*
      *                      * Spaziature orizzontali                  *
      *                      *-----------------------------------------*
           move      zero                 to   w-tst-spa-ori (01)     .
           move      zero                 to   w-tst-spa-ori (02)     .
           move      zero                 to   w-tst-spa-ori (03)     .
           move      zero                 to   w-tst-spa-ori (04)     .
           move      zero                 to   w-tst-spa-ori (05)     .
           move      zero                 to   w-tst-spa-ori (06)     .
           move      zero                 to   w-tst-spa-ori (07)     .
           move      zero                 to   w-tst-spa-ori (08)     .
           move      zero                 to   w-tst-spa-ori (09)     .
           move      zero                 to   w-tst-spa-ori (10)     .
           move      zero                 to   w-tst-spa-ori (11)     .
           move      zero                 to   w-tst-spa-ori (12)     .
           move      zero                 to   w-tst-spa-ori (13)     .
           move      zero                 to   w-tst-spa-ori (14)     .
           move      zero                 to   w-tst-spa-ori (15)     .
           move      zero                 to   w-tst-spa-ori (16)     .
           move      zero                 to   w-tst-spa-ori (17)     .
           move      zero                 to   w-tst-spa-ori (18)     .
           move      zero                 to   w-tst-spa-ori (18)     .
           move      zero                 to   w-tst-spa-ori (20)     .
           perform   vis-spa-ori-000      thru vis-spa-ori-999        .
      *                      *-----------------------------------------*
      *                      * Spaziature verticali                    *
      *                      *-----------------------------------------*
           move      zero                 to   w-tst-spa-ver (01)     .
           move      zero                 to   w-tst-spa-ver (02)     .
           move      zero                 to   w-tst-spa-ver (03)     .
           move      zero                 to   w-tst-spa-ver (04)     .
           move      zero                 to   w-tst-spa-ver (05)     .
           move      zero                 to   w-tst-spa-ver (06)     .
           move      zero                 to   w-tst-spa-ver (07)     .
           move      zero                 to   w-tst-spa-ver (08)     .
           move      zero                 to   w-tst-spa-ver (09)     .
           move      zero                 to   w-tst-spa-ver (10)     .
           move      zero                 to   w-tst-spa-ver (11)     .
           move      zero                 to   w-tst-spa-ver (12)     .
           move      zero                 to   w-tst-spa-ver (13)     .
           move      zero                 to   w-tst-spa-ver (14)     .
           move      zero                 to   w-tst-spa-ver (15)     .
           move      zero                 to   w-tst-spa-ver (16)     .
           move      zero                 to   w-tst-spa-ver (17)     .
           move      zero                 to   w-tst-spa-ver (18)     .
           move      zero                 to   w-tst-spa-ver (18)     .
           move      zero                 to   w-tst-spa-ver (20)     .
           perform   vis-spa-ver-000      thru vis-spa-ver-999        .
       acc-cla-tst-674.
      *                      *-----------------------------------------*
      *                      * Fine normalizzazione                    *
      *                      *-----------------------------------------*
           go to     acc-cla-tst-800.
       acc-cla-tst-680.
      *                  *---------------------------------------------*
      *                  * Se classe : PCL5                            *
      *                  *---------------------------------------------*
       acc-cla-tst-672.
      *                      *-----------------------------------------*
      *                      * Larghezza carrello                      *
      *                      *-----------------------------------------*
           move      zero                 to   w-tst-l72-tst          .
           perform   vis-l72-tst-000      thru vis-l72-tst-999        .
      *                      *-----------------------------------------*
      *                      * Altezza carrello                        *
      *                      *-----------------------------------------*
           move      zero                 to   w-tst-a72-tst          .
           perform   vis-a72-tst-000      thru vis-a72-tst-999        .
      *                      *-----------------------------------------*
      *                      * Spaziature orizzontali                  *
      *                      *-----------------------------------------*
           move      zero                 to   w-tst-spa-ori (01)     .
           move      zero                 to   w-tst-spa-ori (02)     .
           move      zero                 to   w-tst-spa-ori (03)     .
           move      zero                 to   w-tst-spa-ori (04)     .
           move      zero                 to   w-tst-spa-ori (05)     .
           move      zero                 to   w-tst-spa-ori (06)     .
           move      zero                 to   w-tst-spa-ori (07)     .
           move      zero                 to   w-tst-spa-ori (08)     .
           move      zero                 to   w-tst-spa-ori (09)     .
           move      zero                 to   w-tst-spa-ori (10)     .
           move      zero                 to   w-tst-spa-ori (11)     .
           move      zero                 to   w-tst-spa-ori (12)     .
           move      zero                 to   w-tst-spa-ori (13)     .
           move      zero                 to   w-tst-spa-ori (14)     .
           move      zero                 to   w-tst-spa-ori (15)     .
           move      zero                 to   w-tst-spa-ori (16)     .
           move      zero                 to   w-tst-spa-ori (17)     .
           move      zero                 to   w-tst-spa-ori (18)     .
           move      zero                 to   w-tst-spa-ori (18)     .
           move      zero                 to   w-tst-spa-ori (20)     .
           perform   vis-spa-ori-000      thru vis-spa-ori-999        .
      *                      *-----------------------------------------*
      *                      * Spaziature verticali                    *
      *                      *-----------------------------------------*
           move      zero                 to   w-tst-spa-ver (01)     .
           move      zero                 to   w-tst-spa-ver (02)     .
           move      zero                 to   w-tst-spa-ver (03)     .
           move      zero                 to   w-tst-spa-ver (04)     .
           move      zero                 to   w-tst-spa-ver (05)     .
           move      zero                 to   w-tst-spa-ver (06)     .
           move      zero                 to   w-tst-spa-ver (07)     .
           move      zero                 to   w-tst-spa-ver (08)     .
           move      zero                 to   w-tst-spa-ver (09)     .
           move      zero                 to   w-tst-spa-ver (10)     .
           move      zero                 to   w-tst-spa-ver (11)     .
           move      zero                 to   w-tst-spa-ver (12)     .
           move      zero                 to   w-tst-spa-ver (13)     .
           move      zero                 to   w-tst-spa-ver (14)     .
           move      zero                 to   w-tst-spa-ver (15)     .
           move      zero                 to   w-tst-spa-ver (16)     .
           move      zero                 to   w-tst-spa-ver (17)     .
           move      zero                 to   w-tst-spa-ver (18)     .
           move      zero                 to   w-tst-spa-ver (18)     .
           move      zero                 to   w-tst-spa-ver (20)     .
           perform   vis-spa-ver-000      thru vis-spa-ver-999        .
       acc-cla-tst-684.
      *                      *-----------------------------------------*
      *                      * Fine normalizzazione                    *
      *                      *-----------------------------------------*
           go to     acc-cla-tst-800.
       acc-cla-tst-690.
      *                  *---------------------------------------------*
      *                  * Se classe : Special Printer SPPX            *
      *                  *---------------------------------------------*
       acc-cla-tst-692.
      *                      *-----------------------------------------*
      *                      * Larghezza carrello                      *
      *                      *-----------------------------------------*
           move      zero                 to   w-tst-l72-tst          .
           perform   vis-l72-tst-000      thru vis-l72-tst-999        .
      *                      *-----------------------------------------*
      *                      * Altezza carrello                        *
      *                      *-----------------------------------------*
           move      zero                 to   w-tst-a72-tst          .
           perform   vis-a72-tst-000      thru vis-a72-tst-999        .
      *                      *-----------------------------------------*
      *                      * Spaziature orizzontali                  *
      *                      *-----------------------------------------*
           move      zero                 to   w-tst-spa-ori (01)     .
           move      zero                 to   w-tst-spa-ori (02)     .
           move      zero                 to   w-tst-spa-ori (03)     .
           move      zero                 to   w-tst-spa-ori (04)     .
           move      zero                 to   w-tst-spa-ori (05)     .
           move      zero                 to   w-tst-spa-ori (06)     .
           move      zero                 to   w-tst-spa-ori (07)     .
           move      zero                 to   w-tst-spa-ori (08)     .
           move      zero                 to   w-tst-spa-ori (09)     .
           move      zero                 to   w-tst-spa-ori (10)     .
           move      zero                 to   w-tst-spa-ori (11)     .
           move      zero                 to   w-tst-spa-ori (12)     .
           move      zero                 to   w-tst-spa-ori (13)     .
           move      zero                 to   w-tst-spa-ori (14)     .
           move      zero                 to   w-tst-spa-ori (15)     .
           move      zero                 to   w-tst-spa-ori (16)     .
           move      zero                 to   w-tst-spa-ori (17)     .
           move      zero                 to   w-tst-spa-ori (18)     .
           move      zero                 to   w-tst-spa-ori (18)     .
           move      zero                 to   w-tst-spa-ori (20)     .
           perform   vis-spa-ori-000      thru vis-spa-ori-999        .
      *                      *-----------------------------------------*
      *                      * Spaziature verticali                    *
      *                      *-----------------------------------------*
           move      zero                 to   w-tst-spa-ver (01)     .
           move      zero                 to   w-tst-spa-ver (02)     .
           move      zero                 to   w-tst-spa-ver (03)     .
           move      zero                 to   w-tst-spa-ver (04)     .
           move      zero                 to   w-tst-spa-ver (05)     .
           move      zero                 to   w-tst-spa-ver (06)     .
           move      zero                 to   w-tst-spa-ver (07)     .
           move      zero                 to   w-tst-spa-ver (08)     .
           move      zero                 to   w-tst-spa-ver (09)     .
           move      zero                 to   w-tst-spa-ver (10)     .
           move      zero                 to   w-tst-spa-ver (11)     .
           move      zero                 to   w-tst-spa-ver (12)     .
           move      zero                 to   w-tst-spa-ver (13)     .
           move      zero                 to   w-tst-spa-ver (14)     .
           move      zero                 to   w-tst-spa-ver (15)     .
           move      zero                 to   w-tst-spa-ver (16)     .
           move      zero                 to   w-tst-spa-ver (17)     .
           move      zero                 to   w-tst-spa-ver (18)     .
           move      zero                 to   w-tst-spa-ver (18)     .
           move      zero                 to   w-tst-spa-ver (20)     .
           perform   vis-spa-ver-000      thru vis-spa-ver-999        .
       acc-cla-tst-694.
      *                      *-----------------------------------------*
      *                      * Fine normalizzazione                    *
      *                      *-----------------------------------------*
           go to     acc-cla-tst-800.
       acc-cla-tst-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cla-tst-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cla-tst-100.
       acc-cla-tst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Classe tipo stampante                   *
      *    *-----------------------------------------------------------*
       vis-cla-tst-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-cla-tst-lun    to   v-car                  .
           move      w-exp-cla-tst-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-cla-tst-tbl    to   v-txt                  .
           if        w-tst-cla-tst        =    spaces
                     move  1              to   v-num
           else if   w-tst-cla-tst        =    "PS  "
                     move  2              to   v-num
           else if   w-tst-cla-tst        =    "PCL5"
                     move  3              to   v-num
           else if   w-tst-cla-tst        =    "SPPX"
                     move  4              to   v-num
           else if   w-tst-cla-tst        =    "FAX "
                     move  5              to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cla-tst-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Sottoclasse tipo stampante                 *
      *    *-----------------------------------------------------------*
       acc-scl-tst-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tst-scl-tst        to   w-sav-scl-tst          .
      *                  *---------------------------------------------*
      *                  * Se classe diversa da :                      *
      *                  *  - Postscript                               *
      *                  *  - PCL5                                     *
      *                  * uscita                                      *
      *                  *---------------------------------------------*
           if        w-tst-cla-tst        not  = "PS  " and
                     w-tst-cla-tst        not  = "PCL5"
                     go to acc-scl-tst-999.
       acc-scl-tst-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           if        w-tst-cla-tst        =    spaces
                     move  w-exp-scl-tst-nor-num
                                          to   w-exp-scl-tst-num
                     move  w-exp-scl-tst-nor-tbl
                                          to   w-exp-scl-tst-tbl
           else if   w-tst-cla-tst        =    "PS  "
                     move  w-exp-scl-tst-pst-num
                                          to   w-exp-scl-tst-num
                     move  w-exp-scl-tst-pst-tbl
                                          to   w-exp-scl-tst-tbl
           else if   w-tst-cla-tst        =    "PCL5"
                     move  w-exp-scl-tst-hpg-num
                                          to   w-exp-scl-tst-num
                     move  w-exp-scl-tst-hpg-tbl
                                          to   w-exp-scl-tst-tbl
           else      move  w-exp-scl-tst-nor-num
                                          to   w-exp-scl-tst-num
                     move  w-exp-scl-tst-nor-tbl
                                          to   w-exp-scl-tst-tbl      .
       acc-scl-tst-120.
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-scl-tst-lun    to   v-car                  .
           move      w-exp-scl-tst-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-exp-scl-tst-tbl    to   v-txt                  .
       acc-scl-tst-140.
           move      zero                 to   w-exp-scl-tst-ctr      .
       acc-scl-tst-160.
           add       1                    to   w-exp-scl-tst-ctr      .
           if        w-exp-scl-tst-ctr    >    w-exp-scl-tst-num
                     move  zero           to   v-num
                     go to acc-scl-tst-180.
           if        w-exp-scl-tst-tbx
                    (w-exp-scl-tst-ctr)   not  = w-tst-scl-tst
                     go to acc-scl-tst-160.
           move      w-exp-scl-tst-ctr    to   v-num                  .
       acc-scl-tst-180.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-scl-tst-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-scl-tst-999.
       acc-scl-tst-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-exp-scl-tst-ctr      .
           if        w-exp-scl-tst-ctr    =    zero           or
                     w-exp-scl-tst-ctr    >    w-exp-scl-tst-num
                     move  spaces         to   w-tst-scl-tst
           else      move  w-exp-scl-tst-tbx
                          (w-exp-scl-tst-ctr)
                                          to   w-tst-scl-tst          .
       acc-scl-tst-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-scl-tst-425.
      *                  *---------------------------------------------*
      *                  * Valore a Spaces non ammesso, a meno che     *
      *                  * non si sia in Up                            *
      *                  *---------------------------------------------*
           if        w-tst-scl-tst        =    spaces
                     if    v-key          =    "UP  "
                           go to acc-scl-tst-600
                     else  go to acc-scl-tst-100.
       acc-scl-tst-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-scl-tst-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-scl-tst-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-scl-tst-100.
       acc-scl-tst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Sottoclasse tipo stampante              *
      *    *-----------------------------------------------------------*
       vis-scl-tst-000.
           if        w-tst-cla-tst        =    spaces
                     move  w-exp-scl-tst-nor-num
                                          to   w-exp-scl-tst-num
                     move  w-exp-scl-tst-nor-tbl
                                          to   w-exp-scl-tst-tbl
           else if   w-tst-cla-tst        =    "PS  "
                     move  w-exp-scl-tst-pst-num
                                          to   w-exp-scl-tst-num
                     move  w-exp-scl-tst-pst-tbl
                                          to   w-exp-scl-tst-tbl
           else if   w-tst-cla-tst        =    "PCL5"
                     move  w-exp-scl-tst-hpg-num
                                          to   w-exp-scl-tst-num
                     move  w-exp-scl-tst-hpg-tbl
                                          to   w-exp-scl-tst-tbl
           else if   w-tst-cla-tst        =    "SPPX" or
                     w-tst-cla-tst        =    "FAX "
                     move  w-exp-scl-tst-nor-num
                                          to   w-exp-scl-tst-num
                     move  w-exp-scl-tst-nor-tbl
                                          to   w-exp-scl-tst-tbl
           else      move  w-exp-scl-tst-nor-num
                                          to   w-exp-scl-tst-num
                     move  w-exp-scl-tst-nor-tbl
                                          to   w-exp-scl-tst-tbl      .
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-scl-tst-lun    to   v-car                  .
           move      w-exp-scl-tst-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-scl-tst-tbl    to   v-txt                  .
           move      zero                 to   w-exp-scl-tst-ctr      .
       vis-scl-tst-100.
           add       1                    to   w-exp-scl-tst-ctr      .
           if        w-exp-scl-tst-ctr    >    w-exp-scl-tst-num
                     move  zero           to   v-num
                     go to vis-scl-tst-200.
           if        w-exp-scl-tst-tbx
                    (w-exp-scl-tst-ctr)  not  = w-tst-scl-tst
                     go to vis-scl-tst-100.
           move      w-exp-scl-tst-ctr    to   v-num                  .
       vis-scl-tst-200.
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-scl-tst-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Larghezza carrello                         *
      *    *-----------------------------------------------------------*
       acc-l72-tst-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-l72-tst-025.
      *                  *---------------------------------------------*
      *                  * Se stampante di classe PostScript o PCL5 o  *
      *                  * Special Printer SPPX o Fax : no accettazio- *
      *                  * ne                                          *
      *                  *---------------------------------------------*
           if        w-tst-cla-tst        =    "PS  " or
                     w-tst-cla-tst        =    "PCL5" or
                     w-tst-cla-tst        =    "SPPX" or
                     w-tst-cla-tst        =    "FAX "
                     go to acc-l72-tst-999.
       acc-l72-tst-050.
      *                  *---------------------------------------------*
      *                  * Note operative a fianco del campo           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      "Nota: il valore e' in 72.mi di pollice"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-l72-tst-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tst-l72-tst        to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-l72-tst-900.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-l72-tst-900.
       acc-l72-tst-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tst-l72-tst          .
       acc-l72-tst-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-l72-tst-425.
      *                  *---------------------------------------------*
      *                  * Test che il valore non manchi, a meno che   *
      *                  * la stampante non sia di classe PostScript   *
      *                  * o PCL5, o Special Printer SPPX o Fax, o che *
      *                  * non si sia in Up                            *
      *                  *---------------------------------------------*
           if        w-tst-cla-tst        =    "PS  " or
                     w-tst-cla-tst        =    "PCL5" or
                     w-tst-cla-tst        =    "SPPX" or
                     w-tst-cla-tst        =    "FAX "
                     go to acc-l72-tst-600.
           if        v-key                =    "UP  "
                     go to acc-l72-tst-600.
           if        w-tst-l72-tst        =    zero
                     go to acc-l72-tst-100.
       acc-l72-tst-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-l72-tst-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-l72-tst-900
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-l72-tst-100.
       acc-l72-tst-900.
      *              *-------------------------------------------------*
      *              * Cancellazione note operative a fianco           *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-l72-tst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Larghezza carrello                      *
      *    *-----------------------------------------------------------*
       vis-l72-tst-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tst-l72-tst        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-l72-tst-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Altezza carrello                           *
      *    *-----------------------------------------------------------*
       acc-a72-tst-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-a72-tst-025.
      *                  *---------------------------------------------*
      *                  * Se stampante di classe PostScript o PCL5 o  *
      *                  * Special Printer SPPX o Fax : no accettazio- *
      *                  * ne                                          *
      *                  *---------------------------------------------*
           if        w-tst-cla-tst        =    "PS  " or
                     w-tst-cla-tst        =    "PCL5" or
                     w-tst-cla-tst        =    "SPPX" or
                     w-tst-cla-tst        =    "FAX "
                     go to acc-a72-tst-999.
       acc-a72-tst-050.
      *                  *---------------------------------------------*
      *                  * Note operative a fianco del campo           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      "Nota: il valore e' in 72.mi di pollice"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Note operative a piede                      *
      *                  *---------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "NB : Se stampante a modulo continuo non impostare 
      -              "alcun valore."      to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-a72-tst-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tst-a72-tst        to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-a72-tst-900.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-a72-tst-900.
       acc-a72-tst-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tst-a72-tst          .
       acc-a72-tst-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-a72-tst-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-a72-tst-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-a72-tst-900
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-a72-tst-100.
       acc-a72-tst-900.
      *              *-------------------------------------------------*
      *              * Cancellazione note operative a fianco           *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Cancellazione note operative a piede            *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      spaces               to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-a72-tst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Altezza carrello                        *
      *    *-----------------------------------------------------------*
       vis-a72-tst-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tst-a72-tst        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-a72-tst-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Margine superiore                          *
      *    *-----------------------------------------------------------*
       acc-msu-tst-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-msu-tst-050.
      *                  *---------------------------------------------*
      *                  * Note operative a fianco del campo           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      "Nota: il valore e' in 72.mi di pollice"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-msu-tst-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tst-msu-tst        to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-msu-tst-900.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-msu-tst-900.
       acc-msu-tst-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tst-msu-tst          .
       acc-msu-tst-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-msu-tst-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-msu-tst-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-msu-tst-900
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-msu-tst-100.
       acc-msu-tst-900.
      *              *-------------------------------------------------*
      *              * Cancellazione note operative a fianco           *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-msu-tst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Margine superiore                       *
      *    *-----------------------------------------------------------*
       vis-msu-tst-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tst-msu-tst        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-msu-tst-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Area non stampabile superiore              *
      *    *-----------------------------------------------------------*
       acc-nsu-tst-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-nsu-tst-050.
      *                  *---------------------------------------------*
      *                  * Note operative a fianco del campo           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      "Nota: il valore e' in 72.mi di pollice"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-nsu-tst-075.
      *                  *---------------------------------------------*
      *                  * Pre-visualizzazione del valore              *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      w-tst-nsu-tst        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      v-edt                to   w-edt-nxx-e04          .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-edt-nxx-are        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-nsu-tst-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      13                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tst-nsu-tst        to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-nsu-tst-900.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-nsu-tst-900.
       acc-nsu-tst-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tst-nsu-tst          .
       acc-nsu-tst-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-nsu-tst-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-nsu-tst-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-nsu-tst-900
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-nsu-tst-100.
       acc-nsu-tst-900.
      *              *-------------------------------------------------*
      *              * Cancellazione note operative a fianco           *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-nsu-tst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Area non stampabile superiore           *
      *    *-----------------------------------------------------------*
       vis-nsu-tst-000.
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      w-tst-nsu-tst        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      v-edt                to   w-edt-nxx-e04          .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-edt-nxx-are        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-nsu-tst-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Margine inferiore                          *
      *    *-----------------------------------------------------------*
       acc-min-tst-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-min-tst-050.
      *                  *---------------------------------------------*
      *                  * Note operative a fianco del campo           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      "Nota: il valore e' in 72.mi di pollice"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-min-tst-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tst-min-tst        to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-min-tst-900.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-min-tst-900.
       acc-min-tst-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tst-min-tst          .
       acc-min-tst-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-min-tst-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-min-tst-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-min-tst-900
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-min-tst-100.
       acc-min-tst-900.
      *              *-------------------------------------------------*
      *              * Cancellazione note operative a fianco           *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-min-tst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Margine inferiore                       *
      *    *-----------------------------------------------------------*
       vis-min-tst-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tst-min-tst        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-min-tst-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Area non stampabile inferiore              *
      *    *-----------------------------------------------------------*
       acc-nin-tst-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-nin-tst-050.
      *                  *---------------------------------------------*
      *                  * Note operative a fianco del campo           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      "Nota: il valore e' in 72.mi di pollice"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-nin-tst-075.
      *                  *---------------------------------------------*
      *                  * Pre-visualizzazione del valore              *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      w-tst-nin-tst        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      v-edt                to   w-edt-nxx-e04          .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-edt-nxx-are        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-nin-tst-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      14                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tst-nin-tst        to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-nin-tst-900.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-nin-tst-900.
       acc-nin-tst-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tst-nin-tst          .
       acc-nin-tst-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-nin-tst-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-nin-tst-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-nin-tst-900
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-nin-tst-100.
       acc-nin-tst-900.
      *              *-------------------------------------------------*
      *              * Cancellazione note operative a fianco           *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-nin-tst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Area non stampabile inferiore           *
      *    *-----------------------------------------------------------*
       vis-nin-tst-000.
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      w-tst-nin-tst        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      v-edt                to   w-edt-nxx-e04          .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-edt-nxx-are        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-nin-tst-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Margine sinistro                           *
      *    *-----------------------------------------------------------*
       acc-msi-tst-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-msi-tst-050.
      *                  *---------------------------------------------*
      *                  * Note operative a fianco del campo           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      "Nota: il valore e' in 72.mi di pollice"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-msi-tst-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tst-msi-tst        to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-msi-tst-900.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-msi-tst-900.
       acc-msi-tst-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tst-msi-tst          .
       acc-msi-tst-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-msi-tst-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-msi-tst-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-msi-tst-900
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-msi-tst-100.
       acc-msi-tst-900.
      *              *-------------------------------------------------*
      *              * Cancellazione note operative a fianco           *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-msi-tst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Margine sinistro                        *
      *    *-----------------------------------------------------------*
       vis-msi-tst-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tst-msi-tst        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-msi-tst-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Area non stampabile sinistra               *
      *    *-----------------------------------------------------------*
       acc-nsi-tst-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-nsi-tst-050.
      *                  *---------------------------------------------*
      *                  * Note operative a fianco del campo           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      "Nota: il valore e' in 72.mi di pollice"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-nsi-tst-075.
      *                  *---------------------------------------------*
      *                  * Pre-visualizzazione del valore              *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      w-tst-nsi-tst        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      v-edt                to   w-edt-nxx-e04          .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-edt-nxx-are        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-nsi-tst-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      15                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tst-nsi-tst        to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-nsi-tst-900.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-nsi-tst-900.
       acc-nsi-tst-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tst-nsi-tst          .
       acc-nsi-tst-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-nsi-tst-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-nsi-tst-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-nsi-tst-900
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-nsi-tst-100.
       acc-nsi-tst-900.
      *              *-------------------------------------------------*
      *              * Cancellazione note operative a fianco           *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-nsi-tst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Area non stampabile sinistra            *
      *    *-----------------------------------------------------------*
       vis-nsi-tst-000.
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      w-tst-nsi-tst        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      v-edt                to   w-edt-nxx-e04          .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-edt-nxx-are        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-nsi-tst-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Margine destro                             *
      *    *-----------------------------------------------------------*
       acc-mde-tst-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-mde-tst-050.
      *                  *---------------------------------------------*
      *                  * Note operative a fianco del campo           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      "Nota: il valore e' in 72.mi di pollice"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-mde-tst-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tst-mde-tst        to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-mde-tst-900.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-mde-tst-900.
       acc-mde-tst-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tst-mde-tst          .
       acc-mde-tst-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-mde-tst-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-mde-tst-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-mde-tst-900
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-mde-tst-100.
       acc-mde-tst-900.
      *              *-------------------------------------------------*
      *              * Cancellazione note operative a fianco           *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-mde-tst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Margine destro                          *
      *    *-----------------------------------------------------------*
       vis-mde-tst-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tst-mde-tst        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-mde-tst-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Area non stampabile destra                 *
      *    *-----------------------------------------------------------*
       acc-nde-tst-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-nde-tst-050.
      *                  *---------------------------------------------*
      *                  * Note operative a fianco del campo           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      "Nota: il valore e' in 72.mi di pollice"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-nde-tst-075.
      *                  *---------------------------------------------*
      *                  * Pre-visualizzazione del valore              *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      w-tst-nde-tst        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      v-edt                to   w-edt-nxx-e04          .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-edt-nxx-are        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-nde-tst-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      16                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tst-nde-tst        to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-nde-tst-900.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-nde-tst-900.
       acc-nde-tst-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tst-nde-tst          .
       acc-nde-tst-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-nde-tst-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-nde-tst-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-nde-tst-900
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-nde-tst-100.
       acc-nde-tst-900.
      *              *-------------------------------------------------*
      *              * Cancellazione note operative a fianco           *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-nde-tst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Area non stampabile destra              *
      *    *-----------------------------------------------------------*
       vis-nde-tst-000.
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      w-tst-nde-tst        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      v-edt                to   w-edt-nxx-e04          .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-edt-nxx-are        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-nde-tst-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Classe di appartenenza                     *
      *    *-----------------------------------------------------------*
       acc-cla-app-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cla-app-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-cla-app-lun    to   v-car                  .
           move      w-exp-cla-app-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-exp-cla-app-tbl    to   v-txt                  .
           if        w-tst-cla-app        =    "S"
                     move  01             to   v-num
           else if   w-tst-cla-app        =    "M"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cla-app-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cla-app-999.
       acc-cla-app-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   w-tst-cla-app
           else if   v-num                =    02
                     move  "M"            to   w-tst-cla-app
           else      move  spaces         to   w-tst-cla-app          .
       acc-cla-app-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cla-app-425.
      *                  *---------------------------------------------*
      *                  * Che sia un valore ammesso                   *
      *                  *---------------------------------------------*
           if        w-tst-cla-app        not  = "S" and
                     w-tst-cla-app        not  = "M"
                     go to acc-cla-app-100.
       acc-cla-app-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cla-app-625.
      *                  *---------------------------------------------*
      *                  * Eventuale normalizzazione sistema applica-  *
      *                  * tivo di appartenenza                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se non si tratta di un modulo su misu-  *
      *                      * ra : nessuna normalizzazione            *
      *                      *-----------------------------------------*
           if        w-tst-cla-app        =    "M"
                     go to acc-cla-app-800.
      *                      *-----------------------------------------*
      *                      * Normalizzazione                         *
      *                      *-----------------------------------------*
           move      spaces               to   w-tst-sap-app          .
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           perform   vis-sap-app-000      thru vis-sap-app-999        .
       acc-cla-app-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cla-app-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cla-app-100.
       acc-cla-app-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Classe di appartenenza                  *
      *    *-----------------------------------------------------------*
       vis-cla-app-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-cla-app-lun    to   v-car                  .
           move      w-exp-cla-app-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-cla-app-tbl    to   v-txt                  .
           if        w-tst-cla-app        =    "S"
                     move  01             to   v-num
           else if   w-tst-cla-app        =    "M"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cla-app-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Sistema applicativo di appartenenza        *
      *    *-----------------------------------------------------------*
       acc-sap-app-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-sap-app-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tst-cla-app        not  = "M"
                     go to acc-sap-app-999.
       acc-sap-app-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tst-sap-app        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-sap-app-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-sap-app-999.
       acc-sap-app-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tst-sap-app          .
       acc-sap-app-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-sap-app-425.
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      w-tst-sap-app        to   w-all-str-alf          .
           move      03                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-sap-app-100.
       acc-sap-app-450.
      *                  *---------------------------------------------*
      *                  * Test che il valore non manchi, a meno che   *
      *                  * non si sia in Up                            *
      *                  *---------------------------------------------*
           if        w-tst-sap-app        =    spaces
                     if    v-key          =    "UP  "
                           go to acc-sap-app-600
                     else  go to acc-sap-app-100.
       acc-sap-app-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sap-app-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-sap-app-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-sap-app-100.
       acc-sap-app-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Sistema applicativo di appartenenza     *
      *    *-----------------------------------------------------------*
       vis-sap-app-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      w-tst-sap-app        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sap-app-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Spaziature orizzontali                     *
      *    *-----------------------------------------------------------*
       acc-spa-ori-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-spa-ori-002.
      *                  *---------------------------------------------*
      *                  * Se stampante di classe PostScript o PCL5 o  *
      *                  * Special Printer SPPX o Fax : no accettazio- *
      *                  * ne                                          *
      *                  *---------------------------------------------*
           if        w-tst-cla-tst        =    "PS  " or
                     w-tst-cla-tst        =    "PCL5" or
                     w-tst-cla-tst        =    "SPPX" or
                     w-tst-cla-tst        =    "FAX "
                     go to acc-spa-ori-999.
       acc-spa-ori-005.
      *                  *---------------------------------------------*
      *                  * Preparazione default                        *
      *                  *---------------------------------------------*
           move      zero                 to   w-wrk-ctr-00a          .
       acc-spa-ori-010.
           add       1                    to   w-wrk-ctr-00a          .
           if        w-wrk-ctr-00a        >    20
                     go to acc-spa-ori-020.
           if        w-tst-spa-ori
                    (w-wrk-ctr-00a)       =    zero
                     go to acc-spa-ori-010
           else      go to acc-spa-ori-050.
       acc-spa-ori-020.
           move      10,00                to   w-tst-spa-ori (1)      .
       acc-spa-ori-050.
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatore 01..16           *
      *                  *---------------------------------------------*
           move      01                   to   w-wrk-ctr-00a          .
       acc-spa-ori-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      02                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           if        w-wrk-ctr-00a        >    8
                     move  19             to   v-lin
           else      move  18             to   v-lin                  .
           move      w-wrk-ctr-00a        to   v-pos                  .
           if        w-wrk-ctr-00a        >    8
                     subtract  8          from v-pos                  .
           multiply  6                    by   v-pos                  .
           add       24                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "TAB "               to   v-pfk (10)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tst-spa-ori
                    (w-wrk-ctr-00a)       to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-spa-ori-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-spa-ori-999.
       acc-spa-ori-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tst-spa-ori
                                              (w-wrk-ctr-00a)         .
       acc-spa-ori-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-spa-ori-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-spa-ori-625.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda della function key     *
      *                  *---------------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-spa-ori-650
           else if   v-key                =    spaces or
                     v-key                =    "DOWN"
                     go to acc-spa-ori-800
           else if   v-key                =    "DO  "
                     go to acc-spa-ori-800
           else      go to acc-spa-ori-675.
       acc-spa-ori-650.
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
           if        w-wrk-ctr-00a        =    1
                     go to acc-spa-ori-800.
           subtract  1                    from w-wrk-ctr-00a          .
           go to     acc-spa-ori-100.
       acc-spa-ori-675.
      *                  *---------------------------------------------*
      *                  * Se Tab                                      *
      *                  *---------------------------------------------*
           if        w-wrk-ctr-00a        =    16
                     go to acc-spa-ori-800.
           add       1                    to   w-wrk-ctr-00a          .
           go to     acc-spa-ori-100.
       acc-spa-ori-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-spa-ori-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-spa-ori-100.
       acc-spa-ori-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Spaziature orizzontali                  *
      *    *-----------------------------------------------------------*
       vis-spa-ori-000.
           move      spaces               to   w-edt-sov-are          .
           move      zero                 to   w-wrk-ctr-00a          .
           move      zero                 to   w-wrk-ctr-00b          .
       vis-spa-ori-100.
           add       1                    to   w-wrk-ctr-00b          .
           if        w-wrk-ctr-00b        >    2
                     go to vis-spa-ori-300.
           move      zero                 to   w-wrk-ctr-00c          .
       vis-spa-ori-200.
           add       1                    to   w-wrk-ctr-00c          .
           if        w-wrk-ctr-00c        >    8
                     go to vis-spa-ori-100.
           add       1                    to   w-wrk-ctr-00a          .
           if        w-tst-spa-ori
                    (w-wrk-ctr-00a)       =    zero
                     go to vis-spa-ori-200.
           move      w-tst-spa-ori
                    (w-wrk-ctr-00a)       to   w-edt-sov-ele
                                              (w-wrk-ctr-00b,
                                               w-wrk-ctr-00c)         .
           go to     vis-spa-ori-200.
       vis-spa-ori-300.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      48                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-edt-sov-1o2 (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      48                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-edt-sov-1o2 (2)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-spa-ori-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Spaziature verticali                       *
      *    *-----------------------------------------------------------*
       acc-spa-ver-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-spa-ver-002.
      *                  *---------------------------------------------*
      *                  * Se stampante di classe PostScript o PCL5 o  *
      *                  * Special Printer SPPX o Fax : no accettazio- *
      *                  * ne                                          *
      *                  *---------------------------------------------*
           if        w-tst-cla-tst        =    "PS  " or
                     w-tst-cla-tst        =    "PCL5" or
                     w-tst-cla-tst        =    "SPPX" or
                     w-tst-cla-tst        =    "FAX "
                     go to acc-spa-ver-999.
       acc-spa-ver-005.
      *                  *---------------------------------------------*
      *                  * Preparazione default                        *
      *                  *---------------------------------------------*
           move      zero                 to   w-wrk-ctr-00a          .
       acc-spa-ver-010.
           add       1                    to   w-wrk-ctr-00a          .
           if        w-wrk-ctr-00a        >    20
                     go to acc-spa-ver-020.
           if        w-tst-spa-ver
                    (w-wrk-ctr-00a)       =    zero
                     go to acc-spa-ver-010
           else      go to acc-spa-ver-050.
       acc-spa-ver-020.
           move      06,00                to   w-tst-spa-ver (1)      .
       acc-spa-ver-050.
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatore 01..16           *
      *                  *---------------------------------------------*
           move      01                   to   w-wrk-ctr-00a          .
       acc-spa-ver-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      02                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           if        w-wrk-ctr-00a        >    8
                     move  21             to   v-lin
           else      move  20             to   v-lin                  .
           move      w-wrk-ctr-00a        to   v-pos                  .
           if        w-wrk-ctr-00a        >    8
                     subtract  8          from v-pos                  .
           multiply  6                    by   v-pos                  .
           add       24                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "TAB "               to   v-pfk (10)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      w-tst-spa-ver
                    (w-wrk-ctr-00a)       to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-spa-ver-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-spa-ver-999.
       acc-spa-ver-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tst-spa-ver
                                              (w-wrk-ctr-00a)         .
       acc-spa-ver-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-spa-ver-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-spa-ver-625.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda della function key     *
      *                  *---------------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-spa-ver-650
           else if   v-key                =    spaces or
                     v-key                =    "DOWN"
                     go to acc-spa-ver-800
           else if   v-key                =    "DO  "
                     go to acc-spa-ver-800
           else      go to acc-spa-ver-675.
       acc-spa-ver-650.
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
           if        w-wrk-ctr-00a        =    1
                     go to acc-spa-ver-800.
           subtract  1                    from w-wrk-ctr-00a          .
           go to     acc-spa-ver-100.
       acc-spa-ver-675.
      *                  *---------------------------------------------*
      *                  * Se Tab                                      *
      *                  *---------------------------------------------*
           if        w-wrk-ctr-00a        =    16
                     go to acc-spa-ver-800.
           add       1                    to   w-wrk-ctr-00a          .
           go to     acc-spa-ver-100.
       acc-spa-ver-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-spa-ver-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-spa-ver-100.
       acc-spa-ver-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Spaziature verticali                    *
      *    *-----------------------------------------------------------*
       vis-spa-ver-000.
           move      spaces               to   w-edt-sov-are          .
           move      zero                 to   w-wrk-ctr-00a          .
           move      zero                 to   w-wrk-ctr-00b          .
       vis-spa-ver-100.
           add       1                    to   w-wrk-ctr-00b          .
           if        w-wrk-ctr-00b        >    2
                     go to vis-spa-ver-300.
           move      zero                 to   w-wrk-ctr-00c          .
       vis-spa-ver-200.
           add       1                    to   w-wrk-ctr-00c          .
           if        w-wrk-ctr-00c        >    8
                     go to vis-spa-ver-100.
           add       1                    to   w-wrk-ctr-00a          .
           if        w-tst-spa-ver
                    (w-wrk-ctr-00a)       =    zero
                     go to vis-spa-ver-200.
           move      w-tst-spa-ver
                    (w-wrk-ctr-00a)       to   w-edt-sov-ele
                                              (w-wrk-ctr-00b,
                                               w-wrk-ctr-00c)         .
           go to     vis-spa-ver-200.
       vis-spa-ver-300.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      48                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-edt-sov-1o2 (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      48                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-edt-sov-1o2 (2)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-spa-ver-999.
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
      *              * Test                                            *
      *              *-------------------------------------------------*
           if        w-tst-cod-tst        =    spaces
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
       cnt-tdo-nok-050.
      *              *-------------------------------------------------*
      *              * Controllo su descrizione                        *
      *              *-------------------------------------------------*
           if        w-tst-des-tst        not  = spaces
                     go to cnt-tdo-nok-100.
           move      "Manca la descrizione per il tipo stampante !      
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-100.
      *              *-------------------------------------------------*
      *              * Controllo su driver di stampa                   *
      *              *-------------------------------------------------*
           if        w-tst-drv-tst        not  = spaces
                     go to cnt-tdo-nok-150.
           move      "Manca la specifica del driver di stampa !         
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-150.
      *              *-------------------------------------------------*
      *              * Controllo su classe stampante                   *
      *              *-------------------------------------------------*
           if        w-tst-cla-tst        =    spaces or
                     w-tst-cla-tst        =    "PS  " or
                     w-tst-cla-tst        =    "PCL5" or
                     w-tst-cla-tst        =    "SPPX" or
                     w-tst-cla-tst        =    "FAX "
                     go to cnt-tdo-nok-200.
           move      "Classe di stampante errata !                      
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-200.
      *              *-------------------------------------------------*
      *              * Controllo su sottoclasse stampante              *
      *              *-------------------------------------------------*
           if        w-tst-cla-tst        not  = "PS  " and
                     w-tst-cla-tst        not  = "PCL5"
                     go to cnt-tdo-nok-250.
           if        w-tst-scl-tst        not  = spaces
                     go to cnt-tdo-nok-250.
           move      "Manca la sottoclasse per la stampante !           
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-250.
      *              *-------------------------------------------------*
      *              * Controllo su larghezza carrello                 *
      *              *-------------------------------------------------*
           if        w-tst-cla-tst        =    "PS  " or
                     w-tst-cla-tst        =    "PCL5" or
                     w-tst-cla-tst        =    "SPPX" or
                     w-tst-cla-tst        =    "FAX "
                     go to cnt-tdo-nok-300.
           if        w-tst-l72-tst        not  = zero
                     go to cnt-tdo-nok-300.
           move      "Manca la larghezza carrello !                     
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-300.
      *              *-------------------------------------------------*
      *              * Controllo su spaziature orizzontali             *
      *              *-------------------------------------------------*
           if        w-tst-cla-tst        =    "PS  " or
                     w-tst-cla-tst        =    "PCL5" or
                     w-tst-cla-tst        =    "SPPX" or
                     w-tst-cla-tst        =    "FAX "
                     go to cnt-tdo-nok-350.
           if        w-tst-spa-ori (01)   not  = zero or
                     w-tst-spa-ori (02)   not  = zero or
                     w-tst-spa-ori (03)   not  = zero or
                     w-tst-spa-ori (04)   not  = zero or
                     w-tst-spa-ori (05)   not  = zero or
                     w-tst-spa-ori (06)   not  = zero or
                     w-tst-spa-ori (07)   not  = zero or
                     w-tst-spa-ori (08)   not  = zero or
                     w-tst-spa-ori (09)   not  = zero or
                     w-tst-spa-ori (10)   not  = zero or
                     w-tst-spa-ori (11)   not  = zero or
                     w-tst-spa-ori (12)   not  = zero or
                     w-tst-spa-ori (13)   not  = zero or
                     w-tst-spa-ori (14)   not  = zero or
                     w-tst-spa-ori (15)   not  = zero or
                     w-tst-spa-ori (16)   not  = zero or
                     w-tst-spa-ori (17)   not  = zero or
                     w-tst-spa-ori (18)   not  = zero or
                     w-tst-spa-ori (19)   not  = zero or
                     w-tst-spa-ori (20)   not  = zero
                     go to cnt-tdo-nok-350.
           move      "Mancano le spaziature orizzontali!                
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-350.
      *              *-------------------------------------------------*
      *              * Controllo su spaziature verticali               *
      *              *-------------------------------------------------*
           if        w-tst-cla-tst        =    "PS  " or
                     w-tst-cla-tst        =    "PCL5" or
                     w-tst-cla-tst        =    "SPPX" or
                     w-tst-cla-tst        =    "FAX "
                     go to cnt-tdo-nok-400.
           if        w-tst-spa-ver (01)   not  = zero or
                     w-tst-spa-ver (02)   not  = zero or
                     w-tst-spa-ver (03)   not  = zero or
                     w-tst-spa-ver (04)   not  = zero or
                     w-tst-spa-ver (05)   not  = zero or
                     w-tst-spa-ver (06)   not  = zero or
                     w-tst-spa-ver (07)   not  = zero or
                     w-tst-spa-ver (08)   not  = zero or
                     w-tst-spa-ver (09)   not  = zero or
                     w-tst-spa-ver (10)   not  = zero or
                     w-tst-spa-ver (11)   not  = zero or
                     w-tst-spa-ver (12)   not  = zero or
                     w-tst-spa-ver (13)   not  = zero or
                     w-tst-spa-ver (14)   not  = zero or
                     w-tst-spa-ver (15)   not  = zero or
                     w-tst-spa-ver (16)   not  = zero or
                     w-tst-spa-ver (17)   not  = zero or
                     w-tst-spa-ver (18)   not  = zero or
                     w-tst-spa-ver (19)   not  = zero or
                     w-tst-spa-ver (20)   not  = zero
                     go to cnt-tdo-nok-400.
           move      "Mancano le spaziature verticali!                  
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-400.
      *              *-------------------------------------------------*
      *              * Compattamento spaziature orizzontali            *
      *              *-------------------------------------------------*
           move      zero                 to   w-wrk-ctr-00a          .
           move      zero                 to   w-wrk-ctr-00b          .
       cnt-tdo-nok-410.
           add       1                    to   w-wrk-ctr-00b          .
           if        w-wrk-ctr-00b        >    20
                     go to cnt-tdo-nok-420.
           if        w-tst-spa-ori
                    (w-wrk-ctr-00b)       =    zero
                     go to cnt-tdo-nok-410.
           add       1                    to   w-wrk-ctr-00a          .
           if        w-wrk-ctr-00a        =    w-wrk-ctr-00b
                     go to cnt-tdo-nok-410.
           move      w-tst-spa-ori
                    (w-wrk-ctr-00b)       to   w-tst-spa-ori
                                              (w-wrk-ctr-00a)         .
           go to     cnt-tdo-nok-410.
       cnt-tdo-nok-420.
           move      w-wrk-ctr-00a        to   w-wrk-ctr-00c          .
       cnt-tdo-nok-430.
           add       1                    to   w-wrk-ctr-00c          .
           if        w-wrk-ctr-00c        >    20
                     go to cnt-tdo-nok-500.
           move      zero                 to   w-tst-spa-ori
                                              (w-wrk-ctr-00c)         .
           go to     cnt-tdo-nok-430.
       cnt-tdo-nok-500.
      *              *-------------------------------------------------*
      *              * Compattamento spaziature verticali              *
      *              *-------------------------------------------------*
           move      zero                 to   w-wrk-ctr-00a          .
           move      zero                 to   w-wrk-ctr-00b          .
       cnt-tdo-nok-510.
           add       1                    to   w-wrk-ctr-00b          .
           if        w-wrk-ctr-00b        >    20
                     go to cnt-tdo-nok-520.
           if        w-tst-spa-ver
                    (w-wrk-ctr-00b)       =    zero
                     go to cnt-tdo-nok-510.
           add       1                    to   w-wrk-ctr-00a          .
           if        w-wrk-ctr-00a        =    w-wrk-ctr-00b
                     go to cnt-tdo-nok-510.
           move      w-tst-spa-ver
                    (w-wrk-ctr-00b)       to   w-tst-spa-ver
                                              (w-wrk-ctr-00a)         .
           go to     cnt-tdo-nok-510.
       cnt-tdo-nok-520.
           move      w-wrk-ctr-00a        to   w-wrk-ctr-00c          .
       cnt-tdo-nok-530.
           add       1                    to   w-wrk-ctr-00c          .
           if        w-wrk-ctr-00c        >    20
                     go to cnt-tdo-nok-600.
           move      zero                 to   w-tst-spa-ver
                                              (w-wrk-ctr-00c)         .
           go to     cnt-tdo-nok-530.
       cnt-tdo-nok-600.
      *              *-------------------------------------------------*
      *              * Compattamento parametri di stampa               *
      *              *-------------------------------------------------*
           move      zero                 to   w-wrk-ctr-00a          .
           move      zero                 to   w-wrk-ctr-00b          .
       cnt-tdo-nok-610.
           add       1                    to   w-wrk-ctr-00b          .
           if        w-wrk-ctr-00b        >    10
                     go to cnt-tdo-nok-620.
           if        w-tst-par-pds
                    (w-wrk-ctr-00b)       =    spaces
                     go to cnt-tdo-nok-610.
           add       1                    to   w-wrk-ctr-00a          .
           if        w-wrk-ctr-00a        =    w-wrk-ctr-00b
                     go to cnt-tdo-nok-610.
           move      w-tst-par-pds
                    (w-wrk-ctr-00b)       to   w-tst-par-pds
                                              (w-wrk-ctr-00a)         .
           go to     cnt-tdo-nok-610.
       cnt-tdo-nok-620.
           move      w-wrk-ctr-00a        to   w-wrk-ctr-00c          .
       cnt-tdo-nok-630.
           add       1                    to   w-wrk-ctr-00c          .
           if        w-wrk-ctr-00c        >    10
                     go to cnt-tdo-nok-700.
           move      spaces               to   w-tst-par-pds
                                              (w-wrk-ctr-00c)         .
           go to     cnt-tdo-nok-630.
       cnt-tdo-nok-700.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Classe di appartenenza                      *
      *                  *---------------------------------------------*
           if        w-tst-cla-app        not  = "M"
                     move  "S"            to   w-tst-cla-app          .
      *                  *---------------------------------------------*
      *                  * Sistema applicativo di appartenenza         *
      *                  *---------------------------------------------*
           if        w-tst-cla-app        =    "S"
                     move  spaces         to   w-tst-sap-app          .
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
           move      "NO"                 to   j-ope                  .
           move      "TST"                to   j-tre                  .
           call      "swd/mod/prg/obj/mppssf"
                                        using  r
                                               j                      .
           move      j-dat                to   w-tst                  .
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
      *              * Lettura                                         *
      *              *-------------------------------------------------*
           move      "RD"                 to   j-ope                  .
           move      "TST"                to   j-tre                  .
           move      w-tst-cod-tst        to   j-kre                  .
           call      "swd/mod/prg/obj/mppssf"
                                        using  r
                                               j                      .
      *              *-------------------------------------------------*
      *              * Test su esito lettura                           *
      *              *-------------------------------------------------*
           if        j-rsc                =    e-not-fnd
                     go to rou-let-reg-100
           else if   j-rsc                =    spaces
                     go to rou-let-reg-300
           else      go to rou-let-reg-900.
       rou-let-reg-100.
      *              *-------------------------------------------------*
      *              * Se record non trovato                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo funzionamento : Inserimento            *
      *                  *---------------------------------------------*
           move      "I"                  to   w-cnt-mfu-tip-fun      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rou-let-reg-999.
       rou-let-reg-300.
      *              *-------------------------------------------------*
      *              * Se record trovato                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo funzionamento : Modifica               *
      *                  *---------------------------------------------*
           move      "M"                  to   w-cnt-mfu-tip-fun      .
      *                  *---------------------------------------------*
      *                  * Record in area di lavoro                    *
      *                  *---------------------------------------------*
           move      j-dat                to   w-tst                  .
      *                  *---------------------------------------------*
      *                  * Normalizzazione dei campi che potrebbero    *
      *                  * essere non normalizzati                     *
      *                  *---------------------------------------------*
           if        w-tst-nsu-tst        not  numeric
                     move  zero           to   w-tst-nsu-tst          .
           if        w-tst-nin-tst        not  numeric
                     move  zero           to   w-tst-nin-tst          .
           if        w-tst-nsi-tst        not  numeric
                     move  zero           to   w-tst-nsi-tst          .
           if        w-tst-nde-tst        not  numeric
                     move  zero           to   w-tst-nde-tst          .
           if        w-tst-pn1-tst        not  numeric
                     move  zero           to   w-tst-pn1-tst          .
           if        w-tst-pn2-tst        not  numeric
                     move  zero           to   w-tst-pn2-tst          .
           if        w-tst-pn3-tst        not  numeric
                     move  zero           to   w-tst-pn3-tst          .
           if        w-tst-pn4-tst        not  numeric
                     move  zero           to   w-tst-pn4-tst          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rou-let-reg-999.
       rou-let-reg-900.
      *              *-------------------------------------------------*
      *              * Se errore grave di i-o                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di errore                              *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-rou-let-reg      .
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
      *                  * Copia del valore precedente, preservando    *
      *                  * il codice tipo stampante                    *
      *                  *---------------------------------------------*
           move      w-tst-cod-tst        to   w-clo-tst-cod-tst      .
           move      w-clo-tst-rec-tst    to   w-tst                  .
           move      w-clo-tst-cod-tst    to   w-tst-cod-tst          .
       pre-acc-ins-140.
      *                  *---------------------------------------------*
      *                  * Normalizzazione flags di controllo          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di impostazione pagine di testata  *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-tes      .
       pre-acc-ins-150.
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
      *              * Se inserimento                                  *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-500.
      *                  *---------------------------------------------*
      *                  * Scrittura record                            *
      *                  *---------------------------------------------*
           move      "PT"                 to   j-ope                  .
           move      "TST"                to   j-tre                  .
           move      w-tst-cod-tst        to   j-kre                  .
           move      w-tst                to   j-dat                  .
           call      "swd/mod/prg/obj/mppssf"
                                        using  r
                                               j                      .
           if        j-rsc                =    spaces
                     go to scr-mov-fil-999.
           move      j-msg                to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     scr-mov-fil-999.
       scr-mov-fil-500.
      *              *-------------------------------------------------*
      *              * Se modifica                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Riscrittura record                          *
      *                  *---------------------------------------------*
           move      "UP"                 to   j-ope                  .
           move      "TST"                to   j-tre                  .
           move      w-tst-cod-tst        to   j-kre                  .
           move      w-tst                to   j-dat                  .
           call      "swd/mod/prg/obj/mppssf"
                                        using  r
                                               j                      .
           if        j-rsc                =    spaces
                     go to scr-mov-fil-999.
           move      j-msg                to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     scr-mov-fil-999.
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *-----------------------------------------------------------*
       del-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   j-ope                  .
           move      "TST"                to   j-tre                  .
           move      w-tst-cod-tst        to   j-kre                  .
           move      w-tst                to   j-dat                  .
           call      "swd/mod/prg/obj/mppssf"
                                        using  r
                                               j                      .
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Find su tipi stampante di [pss]                           *
      *    *-----------------------------------------------------------*
       fnd-pss-tst-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di selezione               *
      *              *-------------------------------------------------*
           move      spaces               to   w-fnd-pss-tst-sel      .
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pxpg4110"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     move  "#"            to   w-fnd-pss-tst-sel
                     go to  fnd-pss-tst-999.
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
      *              * Preparazione variabile di i.p.c. 'cod-tst'      *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cod-tst"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      08                   to   s-car                  .
           move      w-fnd-pss-tst-cod    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Richiamo programma di interrogazione            *
      *              *-------------------------------------------------*
           call      "swd/xpg/prg/obj/pxpg4110"                       .
           cancel    "swd/xpg/prg/obj/pxpg4110"                       .
      *              *-------------------------------------------------*
      *              * Estrazione di eventuale variabile di i.p.c. de- *
      *              * terminata da function-key "SLCT" durante l'in-  *
      *              * terrogazione                                    *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "pss-tst"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  spaces         to   w-fnd-pss-tst-sel
                     move  s-alf          to   w-fnd-pss-tst-cod
           else      move  "#"            to   w-fnd-pss-tst-sel      .
       fnd-pss-tst-999.
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
      *    * Open file [seq]                                           *
      *    *-----------------------------------------------------------*
       opn-fil-seq-000.
      *              *-------------------------------------------------*
      *              * Preparazione del name                           *
      *              *-------------------------------------------------*
           move      "seq "               to   f-seq-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione del pathname                       *
      *              *-------------------------------------------------*
           move      spaces               to   f-seq-pat              .
           string    "/abd/ogt/swd/drv/prg/obj/"
                                delimited by   spaces
                     w-tst-drv-tst
                                delimited by   spaces
                                          into f-seq-pat              .
      *              *-------------------------------------------------*
      *              * Normalizzazione del file status                 *
      *              *-------------------------------------------------*
           move      e-not-err            to   f-seq-sts              .
      *              *-------------------------------------------------*
      *              * Open effettiva                                  *
      *              *-------------------------------------------------*
           open      input  seq                                       .
       opn-fil-seq-999.
           exit.

      *    *===========================================================*
      *    * Close file [seq]                                          *
      *    *-----------------------------------------------------------*
       cls-fil-seq-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione del file status                 *
      *              *-------------------------------------------------*
           move      e-not-err            to   f-seq-sts              .
      *              *-------------------------------------------------*
      *              * Close effettiva                                 *
      *              *-------------------------------------------------*
           close     seq                                              .
       cls-fil-seq-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

