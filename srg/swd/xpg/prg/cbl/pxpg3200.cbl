       Identification Division.
       Program-Id.                                 pxpg3200           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    swd                 *
      *                        Area gestionale:    xpg                 *
      *                                Settore:    cmn                 *
      *                                   Fase:    xpg320              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 31/03/93    *
      *                       Ultima revisione:    NdK del 23/03/21    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione menu'                              *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * Nota importante                                                *
      *                                                                *
      * Attualmente si possono registrare menu' composti al massimo da *
      * 17 linee effettive, anziche' 18.                               *
      *                                                                *
      * Questo a causa di un errore di calcolo nella strutturazione    *
      * dell'archivio [auc] che, tra i vari tipi record, contiene      *
      * anche i menu'.                                                 *
      *                                                                *
      * Infatti l'area dati di [auc] e' predisposta per 1536 caratte-  *
      * ri, dove invece per un menu' di 18 linee ne sarebbero necessa- *
      * ri 1584 (vedere a questo proposito la work w-mnu).             *
      *                                                                *
      * Se si volesse un domani ovviare a questo inconveniente sara'   *
      * necessario provvedere alle seguenti modifiche:                 *
      *                                                                *
      * - In questo programma, pxpg3200, cercare tutti i literal che   *
      *   indicano il valore 17 e portarli al valore 18. Inoltre nel-  *
      *   la routine di normalizzazione per controllo sul tasto Do e'  *
      *   necessario eliminare la normalizzazione della diciottesima   *
      *   linea.                                                       *
      *                                                                *
      * - In tutti i programmi dell'area swd, moduli e pxpgxxxx, sa-   *
      *   ra' necessario portare l'ampiezza di [auc] a 1584 anziche'   *
      *   1536.                                                        *
      *                                                                *
      * - Approntare un programma di conversione per l'archivio [auc]  *
      *                                                                *
      * - Presso tutti gli utenti portare i nuovi programmi e far gi-  *
      *   rare il programma di conversione.                            *
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
                     "cmn"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "xpg320"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pxpg3200"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "             GESTIONE MENU'             "       .

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
      *    * Area di comunicazione per moduli                "maucmf"  *
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
      *    * Work per records di [auc] 'mnu'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/waucmnu0.cpw"                   .

      *    *===========================================================*
      *    * Work per clonazioni                                       *
      *    *-----------------------------------------------------------*
       01  w-clo.
      *        *-------------------------------------------------------*
      *        * Work per clonazione codice menu'                      *
      *        *-------------------------------------------------------*
           05  w-clo-mnu.
      *            *---------------------------------------------------*
      *            * Codice menu' per salvataggio                      *
      *            *---------------------------------------------------*
               10  w-clo-mnu-cod-mnu      pic  x(06)                  .
      *            *---------------------------------------------------*
      *            * Numero pagina menu' per salvataggio               *
      *            *---------------------------------------------------*
               10  w-clo-mnu-npg-mnu      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Record comando                                    *
      *            *---------------------------------------------------*
               10  w-clo-mnu-rec-mnu.
                   15  filler occurs 2048 pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Find                              *
      *    *-----------------------------------------------------------*
       01  w-fnd.
      *        *-------------------------------------------------------*
      *        * Work per Find su codici menu' di [auc]                *
      *        *-------------------------------------------------------*
           05  w-fnd-auc-mnu.
               10  w-fnd-auc-mnu-sel      pic  x(01)                  .
               10  w-fnd-auc-mnu-mpn.
                   15  w-fnd-auc-mnu-cod  pic  x(06)                  .
                   15  w-fnd-auc-mnu-npg  pic  9(02)                  .
               
      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
           05  filler                     pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
           05  filler                     pic  x(01)                  .

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
      *    * Work-area per test se blanks embedded                     *
      *    *-----------------------------------------------------------*
       01  w-bla-emb.
           05  w-bla-emb-flg              pic  x(01)                  .
           05  w-bla-emb-str.
               10  w-bla-emb-chr occurs 40
                                          pic  x(01)                  .
           05  w-bla-emb-ctr              pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per trasformazioni in uppercase                 *
      *    *-----------------------------------------------------------*
       01  w-upp.
           05  w-upp-des.
               10  w-upp-chr occurs 40    pic  x(01)                  .
           05  w-ctr                      pic  9(02)                  .
           05  w-upp-car.
               10  filler                 pic  x(26) value
                     "ABCDEFGHIJKLMNOPQRSTUVWXYZ"                     .
           05  w-upp-crr redefines w-upp-car.
               10  w-upc occurs 26        pic  x(01)                  .
           05  w-low.
               10  filler                 pic  x(26) value
                     "abcdefghijklmnopqrstuvwxyz"                     .
           05  w-lor redefines w-low.
               10  w-loc occurs 26        pic  x(01)                  .
           05  w-ulc                      pic  9(02)                  .

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

      *    *===========================================================*
      *    * Work-area per editing                                     *
      *    *-----------------------------------------------------------*
       01  w-edt.
      *        *-------------------------------------------------------*
      *        * Titolo centrale con codice menu' e numero pagina      *
      *        *-------------------------------------------------------*
           05  w-edt-tit.
      *            *---------------------------------------------------*
      *            * Literal per codice menu'                          *
      *            *---------------------------------------------------*
               10  w-edt-tit-lit-cod      pic  x(08)                  .
      *            *---------------------------------------------------*
      *            * Codice menu'                                      *
      *            *---------------------------------------------------*
               10  w-edt-tit-cod-mnu      pic  x(06)                  .
      *            *---------------------------------------------------*
      *            * Literal per trattino di separazione               *
      *            *---------------------------------------------------*
               10  w-edt-tit-lit-tds      pic  x(06)                  .
      *            *---------------------------------------------------*
      *            * Literal per numero pagina                         *
      *            *---------------------------------------------------*
               10  w-edt-tit-lit-npg      pic  x(09)                  .
      *            *---------------------------------------------------*
      *            * Numero pagina allineato a sinistra                *
      *            *---------------------------------------------------*
               10  w-edt-tit-npg-mnu      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Area editata completa allineata a sinistra        *
      *            *---------------------------------------------------*
               10  w-edt-tit-edt-aas      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Area editata completa allineata al centro         *
      *            *---------------------------------------------------*
               10  w-edt-tit-edt-aac      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Contatori di comodo                               *
      *            *---------------------------------------------------*
               10  w-edt-tit-ctr-001      pic  9(02)                  .
               10  w-edt-tit-ctr-002      pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per l'accettazione e la visualizzazione di una  *
      *    * intera pagina di menu'                                    *
      *    *-----------------------------------------------------------*
       01  w-acv-pag-mnu.
      *        *-------------------------------------------------------*
      *        * Contatori                                             *
      *        *-------------------------------------------------------*
           05  w-acv-pag-mnu-cta          pic  9(02)                  .
           05  w-acv-pag-mnu-ctv          pic  9(02)                  .
           05  w-acv-pag-mnu-c01          pic  9(02)                  .
           05  w-acv-pag-mnu-c02          pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per determinazione mark-points nella pagina in- *
      *    * tera pagina di menu'                                      *
      *    *-----------------------------------------------------------*
       01  w-dmp-pag-mnu.
      *        *-------------------------------------------------------*
      *        * Comodo per linea in esame                             *
      *        *-------------------------------------------------------*
           05  w-dmp-pag-mnu-lin.
               10  w-dmp-pag-mnu-chl
                               occurs 80  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per comando in esame                           *
      *        *-------------------------------------------------------*
           05  w-dmp-pag-mnu-cmd.
               10  w-dmp-pag-mnu-chc
                               occurs 06  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Contatori                                             *
      *        *-------------------------------------------------------*
           05  w-dmp-pag-mnu-c01          pic  9(02)                  .
           05  w-dmp-pag-mnu-c02          pic  9(02)                  .
           05  w-dmp-pag-mnu-c11          pic  9(02)                  .
           05  w-dmp-pag-mnu-c12          pic  9(02)                  .

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
           move      w-mnu                to   w-clo-mnu-rec-mnu      .
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
      *              * Funzione Open  per modulo              "maucmf" *
      *              *-------------------------------------------------*
           move      "OP"                 to   j-ope                  .
           call      "swd/mod/prg/obj/maucmf"
                                        using  j                      .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * Funzione Close per modulo              "maucmf" *
      *              *-------------------------------------------------*
           move      "CL"                 to   j-ope                  .
           call      "swd/mod/prg/obj/maucmf"
                                        using  j                      .
      *              *-------------------------------------------------*
      *              * Test di cancellabilita' per modulo     "maucmf" *
      *              *-------------------------------------------------*
           move      "X?"                 to   j-ope                  .
           call      "swd/mod/prg/obj/maucmf"
                                        using  j                      .
           if        j-rsc                not  = spaces
                     go to rou-cls-fls-999.
      *              *-------------------------------------------------*
      *              * Cancellazione modulo                   "maucmf" *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/maucmf"                         .
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
      *                  * Codice menu'                                *
      *                  *---------------------------------------------*
           perform   acc-cod-mnu-000      thru acc-cod-mnu-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
       acc-key-reg-250.
      *                  *---------------------------------------------*
      *                  * Numero pagina di menu'                      *
      *                  *---------------------------------------------*
           perform   acc-npg-mnu-000      thru acc-npg-mnu-999        .
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
      *              * Codice menu'                                    *
      *              *-------------------------------------------------*
           perform   vis-cod-mnu-000      thru vis-cod-mnu-999        .
      *              *-------------------------------------------------*
      *              * Numero pagina di menu'                          *
      *              *-------------------------------------------------*
           perform   vis-npg-mnu-000      thru vis-npg-mnu-999        .
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
      *              * Codice menu'                                    *
      *              *-------------------------------------------------*
           perform   pmt-cod-mnu-000      thru pmt-cod-mnu-999        .
      *              *-------------------------------------------------*
      *              * Numero pagina di menu'                          *
      *              *-------------------------------------------------*
           perform   pmt-npg-mnu-000      thru pmt-npg-mnu-999        .
      *              *-------------------------------------------------*
      *              * Lineette di separazione a linea 06              *
      *              *-------------------------------------------------*
           perform   pmt-lds-l06-000      thru pmt-lds-l06-999        .
       pmt-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Prompt per codice menu'                                   *
      *    *-----------------------------------------------------------*
       pmt-cod-mnu-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice menu'               :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-mnu-999.
           exit.

      *    *===========================================================*
      *    * Prompt per numero pagina di menu'                         *
      *    *-----------------------------------------------------------*
       pmt-npg-mnu-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero pagina del menu'    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-npg-mnu-999.
           exit.

      *    *===========================================================*
      *    * Prompt per lineette di separazione a linea 06             *
      *    *-----------------------------------------------------------*
       pmt-lds-l06-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-lds-l06-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice menu'                               *
      *    *-----------------------------------------------------------*
       acc-cod-mnu-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-mnu-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-mnu-cod-mnu        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-mnu-999.
       acc-cod-mnu-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-mnu-cod-mnu          .
       acc-cod-mnu-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-cod-mnu-400.
      *                  *---------------------------------------------*
      *                  * Find su codici menu'                        *
      *                  *---------------------------------------------*
           move      w-mnu-cod-mnu        to   w-fnd-auc-mnu-cod      .
           move      zero                 to   w-fnd-auc-mnu-npg      .
           perform   fnd-auc-mnu-000      thru fnd-auc-mnu-999        .
      *                  *---------------------------------------------*
      *                  * Se nessuna selezione : reimpostazione       *
      *                  *---------------------------------------------*
           if        w-fnd-auc-mnu-sel    not  = spaces
                     go to acc-cod-mnu-100.
      *                  *---------------------------------------------*
      *                  * Memorizzazione codice selezionato           *
      *                  *---------------------------------------------*
           move      w-fnd-auc-mnu-cod    to   w-mnu-cod-mnu          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione codice selezionato          *
      *                  *---------------------------------------------*
           perform   vis-cod-mnu-000      thru vis-cod-mnu-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione numero pagina selezionato    *
      *                  *---------------------------------------------*
           move      w-fnd-auc-mnu-npg    to   w-mnu-npg-mnu          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione numero pagina selezionato   *
      *                  *---------------------------------------------*
           perform   vis-npg-mnu-000      thru vis-npg-mnu-999        .
      *                  *---------------------------------------------*
      *                  * Forzatura tasto Do                          *
      *                  *---------------------------------------------*
           move      "DO  "               to   v-key                  .
       acc-cod-mnu-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cod-mnu-425.
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      w-mnu-cod-mnu        to   w-bla-emb-str          .
           perform   tst-bla-emb-000      thru tst-bla-emb-999        .
           if        w-bla-emb-flg        not  = spaces
                     go to acc-cod-mnu-100.
       acc-cod-mnu-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-mnu-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-mnu-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-mnu-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-mnu-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-mnu-999.
       acc-cod-mnu-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice menu'                            *
      *    *-----------------------------------------------------------*
       vis-cod-mnu-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-mnu-cod-mnu        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-mnu-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Numero pagina di menu'                     *
      *    *-----------------------------------------------------------*
       acc-npg-mnu-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-npg-mnu-025.
      *                  *---------------------------------------------*
      *                  * Eventuale preparazione valore di default    *
      *                  *---------------------------------------------*
           if        w-mnu-npg-mnu        =    zero
                     move  1              to   w-mnu-npg-mnu          .
       acc-npg-mnu-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-mnu-npg-mnu        to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-npg-mnu-999.
       acc-npg-mnu-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-mnu-npg-mnu          .
       acc-npg-mnu-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-npg-mnu-400.
      *                  *---------------------------------------------*
      *                  * Find su codici menu'                        *
      *                  *---------------------------------------------*
           perform   fnd-auc-mnu-000      thru fnd-auc-mnu-999        .
      *                  *---------------------------------------------*
      *                  * Se nessuna selezione : reimpostazione       *
      *                  *---------------------------------------------*
           if        w-fnd-auc-mnu-sel    not  = spaces
                     go to acc-cod-mnu-100.
      *                  *---------------------------------------------*
      *                  * Memorizzazione codice selezionato           *
      *                  *---------------------------------------------*
           move      w-fnd-auc-mnu-cod    to   w-mnu-cod-mnu          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione codice selezionato          *
      *                  *---------------------------------------------*
           perform   vis-cod-mnu-000      thru vis-cod-mnu-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione numero pagina selezionato    *
      *                  *---------------------------------------------*
           move      w-fnd-auc-mnu-npg    to   w-mnu-npg-mnu          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione numero pagina selezionato   *
      *                  *---------------------------------------------*
           perform   vis-npg-mnu-000      thru vis-npg-mnu-999        .
      *                  *---------------------------------------------*
      *                  * Forzatura tasto Do                          *
      *                  *---------------------------------------------*
           move      "DO  "               to   v-key                  .
       acc-npg-mnu-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-npg-mnu-425.
      *                  *---------------------------------------------*
      *                  * Test che il valore non manchi, a meno che   *
      *                  * non si sia in Up                            *
      *                  *---------------------------------------------*
           if        w-mnu-npg-mnu        not  = zero
                     go to acc-npg-mnu-450.
           if        v-key                =    "UP  "
                     go to acc-npg-mnu-600
           else      go to acc-npg-mnu-100.
       acc-npg-mnu-450.
      *                  *---------------------------------------------*
      *                  * Test che il valore non superi il valore     *
      *                  * massimo consentito                          *
      *                  *---------------------------------------------*
           if        w-mnu-npg-mnu        >    5
                     go to acc-npg-mnu-100.
       acc-npg-mnu-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-npg-mnu-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-npg-mnu-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-npg-mnu-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-npg-mnu-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-npg-mnu-999.
       acc-npg-mnu-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Numero pagina di menu'                  *
      *    *-----------------------------------------------------------*
       vis-npg-mnu-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-mnu-npg-mnu        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-npg-mnu-999.
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
      *                  * Pagina intera di menu'                      *
      *                  *---------------------------------------------*
           perform   acc-pag-mnu-000      thru acc-pag-mnu-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
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
      *              * Pagina intera di menu'                          *
      *              *-------------------------------------------------*
           perform   vis-pag-mnu-000      thru vis-pag-mnu-999        .
       vis-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts testata                           *
      *    *-----------------------------------------------------------*
       pmt-tes-reg-000.
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Pagina intera di menu'                     *
      *    *-----------------------------------------------------------*
       acc-pag-mnu-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-pag-mnu-025.
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione pagina intera di menu'      *
      *                  *---------------------------------------------*
           perform   vis-pag-mnu-000      thru vis-pag-mnu-999        .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-pag-mnu-050.
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatore 01..17           *
      *                  *---------------------------------------------*
           move      01                   to   w-acv-pag-mnu-cta      .
       acc-pag-mnu-100.
      *              *-------------------------------------------------*
      *              * Accettazione linea                              *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      w-acv-pag-mnu-cta    to   v-lin                  .
           add       03                   to   v-lin                  .
           move      01                   to   v-pos                  .
           if        w-acv-pag-mnu-cta    >    01
                     move  "UP  "         to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-mnu-lin-mnu (17)   =    spaces and
                     w-acv-pag-mnu-cta    <    17
                     move  "INSR"         to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "REMV"               to   v-pfk (06)             .
           move      w-mnu-lin-mnu
                    (w-acv-pag-mnu-cta)   to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-pag-mnu-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-pag-mnu-999.
       acc-pag-mnu-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-mnu-lin-mnu
                                              (w-acv-pag-mnu-cta)     .
       acc-pag-mnu-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-pag-mnu-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-pag-mnu-605.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tasto funzione im- *
      *                  * postato                                     *
      *                  *---------------------------------------------*
           if        v-key                =    spaces or
                     v-key                =    "DOWN"
                     go to acc-pag-mnu-610
           else if   v-key                =    "UP  "
                     go to acc-pag-mnu-615
           else if   v-key                =    "INSR"
                     go to acc-pag-mnu-620
           else if   v-key                =    "REMV"
                     go to acc-pag-mnu-655
           else if   v-key                =    "DO  "
                     go to acc-pag-mnu-800
           else      go to acc-pag-mnu-100.
       acc-pag-mnu-610.
      *                  *---------------------------------------------*
      *                  * Se Return o Down                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se sull'ultimo elemento : a controllo   *
      *                      * tasto Do                                *
      *                      *-----------------------------------------*
           if        w-acv-pag-mnu-cta    =    17
                     go to acc-pag-mnu-800.
      *                      *-----------------------------------------*
      *                      * Incremento contatore 01..17             *
      *                      *-----------------------------------------*
           add       1                    to   w-acv-pag-mnu-cta      .
      *                      *-----------------------------------------*
      *                      * Riciclo ad accettazione linea successi- *
      *                      * va                                      *
      *                      *-----------------------------------------*
           go to     acc-pag-mnu-100.
       acc-pag-mnu-615.
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Decremento contatore 01..17             *
      *                      *-----------------------------------------*
           subtract  1                    from w-acv-pag-mnu-cta      .
      *                      *-----------------------------------------*
      *                      * Riciclo ad accettazione linea preceden- *
      *                      * te                                      *
      *                      *-----------------------------------------*
           go to     acc-pag-mnu-100.
       acc-pag-mnu-620.
      *                  *---------------------------------------------*
      *                  * Se Insr                                     *
      *                  *---------------------------------------------*
       acc-pag-mnu-625.
      *                      *-----------------------------------------*
      *                      * Video in Off                            *
      *                      *-----------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-pag-mnu-630.
      *                      *-----------------------------------------*
      *                      * Scivolamento di tutte le linee succes-  *
      *                      * sive a quella trattata in giu' di una,  *
      *                      * compresa la linea trattata              *
      *                      *-----------------------------------------*
       acc-pag-mnu-631.
           move      17                   to   w-acv-pag-mnu-c01      .
       acc-pag-mnu-632.
           subtract  1                    from w-acv-pag-mnu-c01      .
           if        w-acv-pag-mnu-c01    <    w-acv-pag-mnu-cta
                     go to acc-pag-mnu-635.
           move      w-acv-pag-mnu-c01    to   w-acv-pag-mnu-c02      .
           add       1                    to   w-acv-pag-mnu-c02      .
           move      w-mnu-ele-mnu
                    (w-acv-pag-mnu-c01)   to   w-mnu-ele-mnu
                                              (w-acv-pag-mnu-c02)     .
           go to     acc-pag-mnu-632.
       acc-pag-mnu-635.
      *                      *-----------------------------------------*
      *                      * Normalizzazione della linea trattata    *
      *                      *-----------------------------------------*
           move      zero                 to   w-mnu-mkp-mnu
                                              (w-acv-pag-mnu-cta)     .
           move      spaces               to   w-mnu-mkc-mnu
                                              (w-acv-pag-mnu-cta)     .
           move      spaces               to   w-mnu-lin-mnu
                                              (w-acv-pag-mnu-cta)     .
       acc-pag-mnu-640.
      *                      *-----------------------------------------*
      *                      * Rivisualizzazione pagina intera di me-  *
      *                      * nu'                                     *
      *                      *-----------------------------------------*
           perform   vis-pag-mnu-000      thru vis-pag-mnu-999        .
       acc-pag-mnu-645.
      *                      *-----------------------------------------*
      *                      * Video in On                             *
      *                      *-----------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-pag-mnu-650.
      *                      *-----------------------------------------*
      *                      * Riciclo ad accettazione stessa linea    *
      *                      *-----------------------------------------*
           go to     acc-pag-mnu-100.
       acc-pag-mnu-655.
      *                  *---------------------------------------------*
      *                  * Se Remv                                     *
      *                  *---------------------------------------------*
       acc-pag-mnu-660.
      *                      *-----------------------------------------*
      *                      * Video in Off                            *
      *                      *-----------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-pag-mnu-665.
      *                      *-----------------------------------------*
      *                      * Scivolamento di tutte le linee succes-  *
      *                      * sive a quella trattata in su' di una,   *
      *                      * compresa la linea trattata              *
      *                      *-----------------------------------------*
       acc-pag-mnu-666.
           move      w-acv-pag-mnu-cta    to   w-acv-pag-mnu-c01      .
       acc-pag-mnu-667.
           move      w-acv-pag-mnu-c01    to   w-acv-pag-mnu-c02      .
           add       1                    to   w-acv-pag-mnu-c02      .
           if        w-acv-pag-mnu-c02    >    17
                     go to acc-pag-mnu-670.
           move      w-mnu-ele-mnu
                    (w-acv-pag-mnu-c02)   to   w-mnu-ele-mnu
                                              (w-acv-pag-mnu-c01)     .
           add       1                    to   w-acv-pag-mnu-c01      .
           go to     acc-pag-mnu-667.
       acc-pag-mnu-670.
      *                      *-----------------------------------------*
      *                      * Normalizzazione dell'ultima linea       *
      *                      *-----------------------------------------*
           move      zero                 to   w-mnu-mkp-mnu (17)     .
           move      spaces               to   w-mnu-mkc-mnu (17)     .
           move      spaces               to   w-mnu-lin-mnu (17)     .
       acc-pag-mnu-675.
      *                      *-----------------------------------------*
      *                      * Rivisualizzazione pagina intera di me-  *
      *                      * nu'                                     *
      *                      *-----------------------------------------*
           perform   vis-pag-mnu-000      thru vis-pag-mnu-999        .
       acc-pag-mnu-680.
      *                      *-----------------------------------------*
      *                      * Video in On                             *
      *                      *-----------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-pag-mnu-685.
      *                      *-----------------------------------------*
      *                      * Riciclo ad accettazione stessa linea    *
      *                      *-----------------------------------------*
           go to     acc-pag-mnu-100.
       acc-pag-mnu-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-pag-mnu-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-pag-mnu-100.
       acc-pag-mnu-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Pagina intera di menu'                  *
      *    *-----------------------------------------------------------*
       vis-pag-mnu-000.
      *              *-------------------------------------------------*
      *              * Visualizzazione titolo centrale con codice me-  *
      *              * nu' e numero pagina                             *
      *              *-------------------------------------------------*
           perform   vis-tit-mnu-000      thru vis-tit-mnu-999        .
       vis-pag-mnu-100.
      *              *-------------------------------------------------*
      *              * Inizializzazione contatore 01..17               *
      *              *-------------------------------------------------*
           move      zero                 to   w-acv-pag-mnu-ctv      .
       vis-pag-mnu-200.
      *              *-------------------------------------------------*
      *              * Incremento contatore 01..17                     *
      *              *-------------------------------------------------*
           add       1                    to   w-acv-pag-mnu-ctv      .
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : uscita                    *
      *              *-------------------------------------------------*
           if        w-acv-pag-mnu-ctv    >    17
                     go to vis-pag-mnu-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione linea                           *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      w-acv-pag-mnu-ctv    to   v-lin                  .
           add       03                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-mnu-lin-mnu
                    (w-acv-pag-mnu-ctv)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     vis-pag-mnu-200.
       vis-pag-mnu-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Titolo centrale con codice menu' e nu-  *
      *    *                   mero pagina                             *
      *    *-----------------------------------------------------------*
       vis-tit-mnu-000.
      *              *-------------------------------------------------*
      *              * Preparazione campi elementari editati           *
      *              *-------------------------------------------------*
       vis-tit-mnu-010.
      *                  *---------------------------------------------*
      *                  * Literal per codice menu'                    *
      *                  *---------------------------------------------*
           move      "Menu' : "           to   w-edt-tit-lit-cod      .
       vis-tit-mnu-020.
      *                  *---------------------------------------------*
      *                  * Codice menu'                                *
      *                  *---------------------------------------------*
           move      w-mnu-cod-mnu        to   w-edt-tit-cod-mnu      .
       vis-tit-mnu-030.
      *                  *---------------------------------------------*
      *                  * Literal per trattino di separazione         *
      *                  *---------------------------------------------*
           move      "      "             to   w-edt-tit-lit-tds      .
       vis-tit-mnu-040.
      *                  *---------------------------------------------*
      *                  * Literal per numero pagina                   *
      *                  *---------------------------------------------*
           move      "Pagina : "          to   w-edt-tit-lit-npg      .
       vis-tit-mnu-050.
      *                  *---------------------------------------------*
      *                  * Numero pagina allineato a sinistra          *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      w-mnu-npg-mnu        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-edt-tit-npg-mnu      .
       vis-tit-mnu-100.
      *              *-------------------------------------------------*
      *              * Preaparazione area editata completa allineata a *
      *              * sinistra                                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-edt-tit-edt-aas      .
           string    w-edt-tit-lit-cod
                                delimited by   size
                     w-edt-tit-cod-mnu
                                delimited by   spaces
                     w-edt-tit-lit-tds
                                delimited by   size
                     w-edt-tit-lit-npg
                                delimited by   size
                     w-edt-tit-npg-mnu
                                delimited by   spaces
                                          into w-edt-tit-edt-aas      .
       vis-tit-mnu-200.
      *              *-------------------------------------------------*
      *              * Preaparazione area editata completa allineata   *
      *              * al centro                                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-edt-tit-edt-aac      .
           move      zero                 to   w-edt-tit-ctr-001      .
           inspect   w-edt-tit-edt-aas
                                      tallying w-edt-tit-ctr-001
                                  for trailing spaces                 .
           move      40                   to   w-edt-tit-ctr-002      .
           subtract  w-edt-tit-ctr-001    from w-edt-tit-ctr-002      .
           divide    2                    into w-edt-tit-ctr-001      .
           add       1                    to   w-edt-tit-ctr-001      .
           move      w-edt-tit-edt-aas
                    (1 : w-edt-tit-ctr-002)
                                          to   w-edt-tit-edt-aac
                                              (w-edt-tit-ctr-001 :
                                               w-edt-tit-ctr-002 )    .
       vis-tit-mnu-300.
      *              *-------------------------------------------------*
      *              * Visualizzazione area editata completa allineata *
      *              * al centro                                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      w-edt-tit-edt-aac    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tit-mnu-999.
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
      *              * Se manca il codice menu' o il numero pagina di  *
      *              * menu' : controllo non superato                  *
      *              *-------------------------------------------------*
           if        w-mnu-cod-mnu        =    spaces or
                     w-mnu-npg-mnu        =    zero
                     move  "#"            to   w-cnt-tdo-key-flg
                     go to cnt-tdo-key-999.
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
      *              * Se manca sia il codice menu' che il numero pa-  *
      *              * gina di menu' : chiave vuota                    *
      *              *-------------------------------------------------*
           if        w-mnu-cod-mnu        =    spaces or
                     w-mnu-npg-mnu        =    zero
                     move  "#"            to   w-cnt-key-vuo-flg
                     go to cnt-key-vuo-999.
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
      *              * Determinazione dei mark-points nella pagina di  *
      *              * menu'                                           *
      *              *-------------------------------------------------*
           perform   det-mkp-mnu-000      thru det-mkp-mnu-999        .
       cnt-tdo-nok-800.
      *              *-------------------------------------------------*
      *              * Uscita per controlli tutti superati             *
      *              *-------------------------------------------------*
       cnt-tdo-nok-825.
      *                  *---------------------------------------------*
      *                  * Normalizzazione 18.ma linea                 *
      *                  *---------------------------------------------*
           move      zero                 to   w-mnu-mkp-mnu (18)     .
           move      spaces               to   w-mnu-mkc-mnu (18)     .
           move      spaces               to   w-mnu-lin-mnu (18)     .
       cnt-tdo-nok-850.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
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
      *    * Determinazione dei mark-points nella pagina di menu'      *
      *    *-----------------------------------------------------------*
       det-mkp-mnu-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione contatore 01..17               *
      *              *-------------------------------------------------*
           move      zero                 to   w-dmp-pag-mnu-c01      .
       det-mkp-mnu-100.
      *              *-------------------------------------------------*
      *              * Incremento contatore contatore 01..17           *
      *              *-------------------------------------------------*
           add       1                    to   w-dmp-pag-mnu-c01      .
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : uscita                    *
      *              *-------------------------------------------------*
           if        w-dmp-pag-mnu-c01    >    17
                     go to det-mkp-mnu-999.
       det-mkp-mnu-200.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare mark-point          *
      *              *-------------------------------------------------*
           move      zero                 to   w-mnu-mkp-mnu
                                              (w-dmp-pag-mnu-c01)     .
           move      spaces               to   w-mnu-mkc-mnu
                                              (w-dmp-pag-mnu-c01)     .
       det-mkp-mnu-300.
      *              *-------------------------------------------------*
      *              * Linea in esame in comodo di lavoro              *
      *              *-------------------------------------------------*
           move      w-mnu-lin-mnu
                    (w-dmp-pag-mnu-c01)   to   w-dmp-pag-mnu-lin      .
      *              *-------------------------------------------------*
      *              * Se linea a spaces : la si ignora                *
      *              *-------------------------------------------------*
           if        w-dmp-pag-mnu-lin    =    spaces
                     go to det-mkp-mnu-800.
      *              *-------------------------------------------------*
      *              * Conteggio del numero di caratteri nella linea   *
      *              * che precedono il primo mark-point               *
      *              *-------------------------------------------------*
           move      zero                 to   w-dmp-pag-mnu-c02      .
           inspect   w-dmp-pag-mnu-lin
                                      tallying w-dmp-pag-mnu-c02
                     for   characters   before initial   "@"          .
      *              *-------------------------------------------------*
      *              * Se non ci sono mark-point : si ignora la linea  *
      *              *-------------------------------------------------*
           if        w-dmp-pag-mnu-c02    =    80
                     go to det-mkp-mnu-800.
      *              *-------------------------------------------------*
      *              * Determinazione della posizione effettiva del    *
      *              * comando corrispondente al mark-point            *
      *              *-------------------------------------------------*
           add       2                    to   w-dmp-pag-mnu-c02      .
      *              *-------------------------------------------------*
      *              * Se posizione errata : si ignora la linea        *
      *              *-------------------------------------------------*
           if        w-dmp-pag-mnu-c02    >    80
                     go to det-mkp-mnu-700.
      *              *-------------------------------------------------*
      *              * Se il carattere indicato dalla posizione e' a   *
      *              * spaces : si ignora la linea                     *
      *              *-------------------------------------------------*
           if        w-dmp-pag-mnu-chl
                    (w-dmp-pag-mnu-c02)   =    spaces
                     go to det-mkp-mnu-700.
       det-mkp-mnu-400.
      *              *-------------------------------------------------*
      *              * Estrazione del comando corrispondente al mark-  *
      *              * point                                           *
      *              *-------------------------------------------------*
       det-mkp-mnu-405.
           move      spaces               to   w-dmp-pag-mnu-cmd      .
           move      w-dmp-pag-mnu-c02    to   w-dmp-pag-mnu-c11      .
           move      zero                 to   w-dmp-pag-mnu-c12      .
       det-mkp-mnu-410.
           add       1                    to   w-dmp-pag-mnu-c12      .
           if        w-dmp-pag-mnu-c12    >    6
                     go to det-mkp-mnu-500.
           move      w-dmp-pag-mnu-chl
                    (w-dmp-pag-mnu-c11)   to   w-dmp-pag-mnu-chc
                                              (w-dmp-pag-mnu-c12)     .
           add       1                    to   w-dmp-pag-mnu-c11      .
           if        w-dmp-pag-mnu-c11    >    80
                     go to det-mkp-mnu-500.
           if        w-dmp-pag-mnu-chl
                    (w-dmp-pag-mnu-c11)   =    spaces
                     go to det-mkp-mnu-500.
           go to     det-mkp-mnu-410.
       det-mkp-mnu-500.
      *              *-------------------------------------------------*
      *              * Se il comando estratto e' a spaces : si ignora  *
      *              * la linea                                        *
      *              *-------------------------------------------------*
           if        w-dmp-pag-mnu-cmd    =    spaces
                     go to det-mkp-mnu-700.
       det-mkp-mnu-600.
      *              *-------------------------------------------------*
      *              * Memorizzazione della posizione del mark-point e *
      *              * del comando associato                           *
      *              *-------------------------------------------------*
           move      w-dmp-pag-mnu-c02    to   w-mnu-mkp-mnu
                                              (w-dmp-pag-mnu-c01)     .
           move      w-dmp-pag-mnu-cmd    to   w-mnu-mkc-mnu
                                              (w-dmp-pag-mnu-c01)     .
       det-mkp-mnu-700.
      *              *-------------------------------------------------*
      *              * Eliminazione di tutti i segnali di mark-point   *
      *              * dalla linea in esame                            *
      *              *-------------------------------------------------*
           inspect   w-mnu-lin-mnu
                    (w-dmp-pag-mnu-c01)
                                     replacing all "@" by spaces      .
       det-mkp-mnu-800.
      *              *-------------------------------------------------*
      *              * Riciclo a linea successiva                      *
      *              *-------------------------------------------------*
           go to     det-mkp-mnu-100.
       det-mkp-mnu-999.
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
      *              *-------------------------------------------------*
      *              * Codice menu'                                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   j-ope                  .
           move      "MNU"                to   j-tre                  .
           call      "swd/mod/prg/obj/maucmf"
                                        using  j                      .
           move      j-dat                to   w-mnu                  .
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
           move      "MNU"                to   j-tre                  .
           move      spaces               to   j-kre                  .
           string    w-mnu-cod-mnu
                                delimited by   size
                     w-mnu-npg-mnu
                                delimited by   size
                                          into j-kre                  .
           call      "swd/mod/prg/obj/maucmf"
                                        using  j                      .
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
           move      j-dat                to   w-mnu                  .
      *                  *---------------------------------------------*
      *                  * Ricostruzione dei segnali di mark-point     *
      *                  *---------------------------------------------*
           perform   rcs-sgn-mkp-000      thru rcs-sgn-mkp-999        .
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
      *    * Ricostruzione dei segnali di mark-point                   *
      *    *-----------------------------------------------------------*
       rcs-sgn-mkp-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione contatore 01..17               *
      *              *-------------------------------------------------*
           move      zero                 to   w-dmp-pag-mnu-c01      .
       rcs-sgn-mkp-100.
      *              *-------------------------------------------------*
      *              * Incremento contatore contatore 01..17           *
      *              *-------------------------------------------------*
           add       1                    to   w-dmp-pag-mnu-c01      .
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : uscita                    *
      *              *-------------------------------------------------*
           if        w-dmp-pag-mnu-c01    >    17
                     go to rcs-sgn-mkp-999.
       rcs-sgn-mkp-200.
      *              *-------------------------------------------------*
      *              * Posizione del mark-point in comodo di lavoro    *
      *              *-------------------------------------------------*
           move      w-mnu-mkp-mnu
                    (w-dmp-pag-mnu-c01)   to   w-dmp-pag-mnu-c02      .
      *              *-------------------------------------------------*
      *              * Se linea senza mark-point : la si ignora        *
      *              *-------------------------------------------------*
           if        w-dmp-pag-mnu-c02    <    02 or
                     w-dmp-pag-mnu-c02    >    80
                     go to rcs-sgn-mkp-600.
      *              *-------------------------------------------------*
      *              * Posizione del carattere '@'                     *
      *              *-------------------------------------------------*
           subtract  1                    from w-dmp-pag-mnu-c02      .
       rcs-sgn-mkp-300.
      *              *-------------------------------------------------*
      *              * Linea in esame in comodo di lavoro              *
      *              *-------------------------------------------------*
           move      w-mnu-lin-mnu
                    (w-dmp-pag-mnu-c01)   to   w-dmp-pag-mnu-lin      .
       rcs-sgn-mkp-400.
      *              *-------------------------------------------------*
      *              * Ricostruzione segnale di mark-point             *
      *              *-------------------------------------------------*
           move      "@"                  to   w-dmp-pag-mnu-chl
                                              (w-dmp-pag-mnu-c02)     .
       rcs-sgn-mkp-500.
      *              *-------------------------------------------------*
      *              * Linea da comodo di lavoro a linea in esame      *
      *              *-------------------------------------------------*
           move      w-dmp-pag-mnu-lin    to   w-mnu-lin-mnu
                                              (w-dmp-pag-mnu-c01)     .
       rcs-sgn-mkp-600.
      *              *-------------------------------------------------*
      *              * Riciclo a linea successiva                      *
      *              *-------------------------------------------------*
           go to     rcs-sgn-mkp-100.
       rcs-sgn-mkp-999.
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
      *                  * il codice menu' ed il numero pagina menu'   *
      *                  *---------------------------------------------*
           move      w-mnu-cod-mnu        to   w-clo-mnu-cod-mnu      .
           move      w-mnu-npg-mnu        to   w-clo-mnu-npg-mnu      .
           move      w-clo-mnu-rec-mnu    to   w-mnu                  .
           move      w-clo-mnu-cod-mnu    to   w-mnu-cod-mnu          .
           move      w-clo-mnu-npg-mnu    to   w-mnu-npg-mnu          .
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
           move      "MNU"                to   j-tre                  .
           move      spaces               to   j-kre                  .
           string    w-mnu-cod-mnu
                                delimited by   size
                     w-mnu-npg-mnu
                                delimited by   size
                                          into j-kre                  .
           move      w-mnu                to   j-dat                  .
           call      "swd/mod/prg/obj/maucmf"
                                        using  j                      .
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
           move      "MNU"                to   j-tre                  .
           move      spaces               to   j-kre                  .
           string    w-mnu-cod-mnu
                                delimited by   size
                     w-mnu-npg-mnu
                                delimited by   size
                                          into j-kre                  .
           move      w-mnu                to   j-dat                  .
           call      "swd/mod/prg/obj/maucmf"
                                        using  j                      .
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
           move      "MNU"                to   j-tre                  .
           move      spaces               to   j-kre                  .
           string    w-mnu-cod-mnu
                                delimited by   size
                     w-mnu-npg-mnu
                                delimited by   size
                                          into j-kre                  .
           move      w-mnu                to   j-dat                  .
           call      "swd/mod/prg/obj/maucmf"
                                        using  j                      .
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Find su codici menu' di [auc]                             *
      *    *-----------------------------------------------------------*
       fnd-auc-mnu-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di selezione               *
      *              *-------------------------------------------------*
           move      spaces               to   w-fnd-auc-mnu-sel      .
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pxpg3210"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     move  "#"            to   w-fnd-auc-mnu-sel
                     go to  fnd-auc-mnu-999.
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
      *              * Preparazione variabile di i.p.c. 'cod-mnu'      *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cod-mnu"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      06                   to   s-car                  .
           move      w-fnd-auc-mnu-cod    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Richiamo programma di interrogazione            *
      *              *-------------------------------------------------*
           call      "swd/xpg/prg/obj/pxpg3210"                       .
           cancel    "swd/xpg/prg/obj/pxpg3210"                       .
      *              *-------------------------------------------------*
      *              * Estrazione di eventuale variabile di i.p.c. de- *
      *              * terminata da function-key "SLCT" durante l'in-  *
      *              * terrogazione                                    *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "auc-mnu"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  spaces         to   w-fnd-auc-mnu-sel
                     move  s-alf          to   w-fnd-auc-mnu-mpn
           else      move  "#"            to   w-fnd-auc-mnu-sel      .
       fnd-auc-mnu-999.
           exit.

      *    *===========================================================*
      *    * Trasformazione in uppercase                               *
      *    *-----------------------------------------------------------*
       trf-des-upp-000.
           move      zero                 to   w-ctr                  .
       trf-des-upp-100.
           add       1                    to   w-ctr                  .
           if        w-ctr                >    40
                     go to  trf-des-upp-999.
           move      zero                 to   w-ulc                  .
           inspect   w-low            tallying w-ulc
                     for characters     before initial w-upp-chr
                                                      (w-ctr)         .
           if        w-ulc                <    26
                     add     1            to   w-ulc
                     move    w-upc(w-ulc) to   w-upp-chr(w-ctr)       .
           go to     trf-des-upp-100.
       trf-des-upp-999.
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

